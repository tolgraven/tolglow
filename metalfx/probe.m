#import <Foundation/Foundation.h>
#import <CoreGraphics/CoreGraphics.h>
#import <ImageIO/ImageIO.h>
#import <Metal/Metal.h>
#import <MetalFX/MetalFX.h>

#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "scene.h"

typedef struct {
    const char *name;
    unsigned inputWidth, inputHeight, outputWidth, outputHeight;
} Raster;

static const Raster rasters[] = {
    {"small",   914,  410, 1828,  820},
    {"mobile", 1828,  821, 2806, 1260},
    {"desktop",2670, 1199, 2806, 1260}
};

typedef struct {
    double gpu;
    double wall;
} Timing;

static const char *const routeNames[] = {"bilinear", "metalfx"};
enum { warmFrames = 40, phases = 2 };

static void fail(NSString *step, NSError *error)
{
    fprintf(stderr, "%s: %s\n", step.UTF8String,
            error ? error.localizedDescription.UTF8String : "failed");
    exit(EXIT_FAILURE);
}

static double nowMS(void)
{
    return (double)clock_gettime_nsec_np(CLOCK_UPTIME_RAW) / 1e6;
}

static size_t rowBytes(unsigned width, BOOL hdr)
{
    size_t packed = width * (hdr ? 8u : 4u);
    return (packed + 255u) & ~(size_t)255u;
}

static id<MTLTexture> privateTexture(id<MTLDevice> device, MTLPixelFormat format,
                                    unsigned width, unsigned height,
                                    MTLTextureUsage usage)
{
    MTLTextureDescriptor *desc = [MTLTextureDescriptor
        texture2DDescriptorWithPixelFormat:format width:width height:height mipmapped:NO];
    desc.storageMode = MTLStorageModePrivate;
    desc.hazardTrackingMode = MTLHazardTrackingModeTracked;
    desc.usage = usage;
    id<MTLTexture> texture = [device newTextureWithDescriptor:desc];
    if (!texture) fail(@"Allocate private texture", nil);
    return texture;
}

static id<MTLBuffer> sharedBuffer(id<MTLDevice> device, size_t length)
{
    id<MTLBuffer> buffer = [device newBufferWithLength:length
                                              options:MTLResourceStorageModeShared];
    if (!buffer) fail(@"Allocate shared staging buffer", nil);
    return buffer;
}

static id<MTLCommandBuffer> command(id<MTLCommandQueue> queue, NSString *label)
{
    id<MTLCommandBuffer> buffer = [queue commandBuffer];
    if (!buffer) fail(@"Allocate command buffer", nil);
    buffer.label = label;
    return buffer;
}

static Timing finish(id<MTLCommandBuffer> buffer, double began)
{
    [buffer commit];
    [buffer waitUntilCompleted];
    double wall = nowMS() - began;
    if (buffer.status != MTLCommandBufferStatusCompleted) fail(buffer.label, buffer.error);
    double start = buffer.GPUStartTime;
    double end = buffer.GPUEndTime;
    double gpu = NAN;
    if (start > 0 && end >= start) gpu = (end - start) * 1000.0;
    return (Timing){gpu, wall};
}

static void upload(id<MTLCommandQueue> queue, id<MTLTexture> texture,
                   unsigned width, unsigned height, unsigned phase, BOOL hdr)
{
    size_t stride = rowBytes(width, hdr);
    id<MTLBuffer> staging = sharedBuffer(queue.device, stride * height);
    uint8_t *bytes = staging.contents;
    for (unsigned y = 0; y < height; ++y) {
        uint8_t *row = bytes + y * stride;
        for (unsigned x = 0; x < width; ++x) {
            SceneSample pixel = roomSample(x, y, width, height, phase, hdr);
            if (hdr) {
                _Float16 *rgba = (_Float16 *)row + 4u * x;
                for (unsigned c = 0; c < 3; ++c) rgba[c] = (_Float16)pixel.rgb[c];
                rgba[3] = 1;
            } else {
                uint8_t *rgba = row + 4u * x;
                for (unsigned c = 0; c < 3; ++c) rgba[c] = (uint8_t)lroundf(255 * pixel.rgb[c]);
                rgba[3] = 255;
            }
        }
    }
    id<MTLCommandBuffer> buffer = command(queue, @"Upload synthetic Room color");
    id<MTLBlitCommandEncoder> blit = [buffer blitCommandEncoder];
    if (!blit) fail(@"Create upload encoder", nil);
    [blit copyFromBuffer:staging sourceOffset:0 sourceBytesPerRow:stride
      sourceBytesPerImage:stride * height sourceSize:MTLSizeMake(width, height, 1)
               toTexture:texture destinationSlice:0 destinationLevel:0
       destinationOrigin:MTLOriginMake(0, 0, 0)];
    [blit endEncoding];
    finish(buffer, nowMS());
}

static id<MTLRenderPipelineState> bilinearPipeline(id<MTLDevice> device,
                                                    id<MTLLibrary> library,
                                                    MTLPixelFormat format)
{
    MTLRenderPipelineDescriptor *desc = [MTLRenderPipelineDescriptor new];
    desc.label = @"Native Room bilinear full-screen triangle";
    desc.vertexFunction = [library newFunctionWithName:@"fullscreen"];
    desc.fragmentFunction = [library newFunctionWithName:@"bilinear"];
    desc.colorAttachments[0].pixelFormat = format;
    NSError *error = nil;
    id<MTLRenderPipelineState> pipeline = [device newRenderPipelineStateWithDescriptor:desc
                                                                                error:&error];
    if (!pipeline) fail(@"Create bilinear pipeline", error);
    return pipeline;
}

static Timing scale(id<MTLCommandQueue> queue, id<MTLRenderPipelineState> pipeline,
                    id<MTLFXSpatialScaler> scaler, id<MTLTexture> input,
                    id<MTLTexture> output, NSString *label)
{
    double began = nowMS();
    id<MTLCommandBuffer> buffer = command(queue, label);
    if (scaler) {
        scaler.colorTexture = input;
        scaler.outputTexture = output;
        [scaler encodeToCommandBuffer:buffer];
    } else {
        MTLRenderPassDescriptor *pass = [MTLRenderPassDescriptor renderPassDescriptor];
        pass.colorAttachments[0].texture = output;
        pass.colorAttachments[0].loadAction = MTLLoadActionDontCare;
        pass.colorAttachments[0].storeAction = MTLStoreActionStore;
        id<MTLRenderCommandEncoder> encoder = [buffer renderCommandEncoderWithDescriptor:pass];
        if (!encoder) fail(@"Create bilinear render encoder", nil);
        [encoder setRenderPipelineState:pipeline];
        [encoder setFragmentTexture:input atIndex:0];
        [encoder drawPrimitives:MTLPrimitiveTypeTriangle vertexStart:0 vertexCount:3];
        [encoder endEncoding];
    }
    return finish(buffer, began);
}

static Timing readback(id<MTLCommandQueue> queue, id<MTLTexture> texture,
                       id<MTLBuffer> shared, size_t stride, NSString *label)
{
    double began = nowMS();
    id<MTLCommandBuffer> buffer = command(queue, label);
    id<MTLBlitCommandEncoder> blit = [buffer blitCommandEncoder];
    if (!blit) fail(@"Create readback encoder", nil);
    [blit copyFromTexture:texture sourceSlice:0 sourceLevel:0
             sourceOrigin:MTLOriginMake(0, 0, 0)
               sourceSize:MTLSizeMake(texture.width, texture.height, 1)
                 toBuffer:shared destinationOffset:0 destinationBytesPerRow:stride
 destinationBytesPerImage:stride * texture.height];
    [blit endEncoding];
    return finish(buffer, began);
}

static int compareDouble(const void *a, const void *b)
{
    double left = *(const double *)a;
    double right = *(const double *)b;
    return (left > right) - (left < right);
}

static void reportMetric(NSString *label, const char *metric, const Timing *series,
                         unsigned count, BOOL gpu)
{
    double *values = malloc(count * sizeof(*values));
    if (!values) fail(@"Allocate timing statistics", nil);
    unsigned valid = 0;
    for (unsigned index = 0; index < count; ++index) {
        double value = gpu ? series[index].gpu : series[index].wall;
        if (isfinite(value)) values[valid++] = value;
    }

    printf("timing %s metric=%s samples=%u/%u", label.UTF8String, metric, valid, count);
    if (valid) {
        qsort(values, valid, sizeof(*values), compareDouble);
        printf(" p50_ms=%.4f p95_ms=%.4f p99_ms=%.4f\n",
               values[(unsigned)ceil(0.50 * valid) - 1],
               values[(unsigned)ceil(0.95 * valid) - 1],
               values[(unsigned)ceil(0.99 * valid) - 1]);
    } else {
        puts(" p50_ms=n/a p95_ms=n/a p99_ms=n/a");
    }
    free(values);
}

static uint8_t displayByte(float linear, BOOL hdr)
{
    linear = fmaxf(linear, 0.0f);
    if (hdr) linear = linear / (1.0f + linear);
    float srgb = linear <= 0.0031308f ? linear * 12.92f
                                   : 1.055f * powf(linear, 1.0f / 2.4f) - 0.055f;
    return (uint8_t)lroundf(255 * clamp01(srgb));
}

static void savePNG(NSURL *url, uint8_t *rgba, unsigned width, unsigned height)
{
    NSData *bytes = [NSData dataWithBytesNoCopy:rgba length:(size_t)width * height * 4
                                    freeWhenDone:YES];
    CGDataProviderRef provider = CGDataProviderCreateWithCFData((__bridge CFDataRef)bytes);
    CGColorSpaceRef space = CGColorSpaceCreateWithName(kCGColorSpaceSRGB);
    if (!provider || !space) fail(@"Create PNG color and data provider", nil);
    CGImageRef image = CGImageCreate(width, height, 8, 32, width * 4, space,
                                     kCGBitmapByteOrder32Big | kCGImageAlphaNoneSkipLast,
                                     provider, NULL, false, kCGRenderingIntentDefault);
    CGImageDestinationRef destination = CGImageDestinationCreateWithURL((__bridge CFURLRef)url,
                                                                        CFSTR("public.png"), 1, NULL);
    if (!image || !destination) fail(@"Create PNG destination", nil);
    CGImageDestinationAddImage(destination, image, NULL);
    if (!CGImageDestinationFinalize(destination)) fail(@"Write PNG", nil);
    CFRelease(destination);
    CGImageRelease(image);
    CGColorSpaceRelease(space);
    CGDataProviderRelease(provider);
}

static void capture(Raster raster, BOOL hdr, unsigned phase, const SceneSample *reference,
                    const uint8_t *readbackBytes, size_t stride, NSString *label, NSURL *url)
{
    size_t pixels = (size_t)raster.outputWidth * raster.outputHeight;
    uint8_t *image = malloc(pixels * 4u);
    if (!image) fail(@"Allocate PNG pixels", nil);
    double absolute = 0, squared = 0, shaftAbsolute = 0;
    size_t shafts = 0, highlights = 0;
    float peak = -INFINITY;
    for (unsigned y = 0; y < raster.outputHeight; ++y) {
        for (unsigned x = 0; x < raster.outputWidth; ++x) {
            size_t pixel = (size_t)y * raster.outputWidth + x;
            SceneSample expected = reference[pixel];
            const uint8_t *row = readbackBytes ? readbackBytes + y * stride : NULL;
            BOOL shaft = expected.shaft >= 0.6f;
            if (shaft) ++shafts;
            for (unsigned c = 0; c < 3; ++c) {
                float actual = expected.rgb[c];
                if (row) {
                    actual = hdr ? (float)((const _Float16 *)row)[4u * x + c]
                                 : (float)row[4u * x + c] / 255.0f;
                }
                if (!isfinite(actual)) fail(@"Readback contains a non-finite sample", nil);
                double error = actual - expected.rgb[c];
                absolute += fabs(error);
                squared += error * error;
                if (shaft) shaftAbsolute += fabs(error);
                if (actual > 1.0f) ++highlights;
                peak = fmaxf(peak, actual);
                image[4u * pixel + c] = displayByte(actual, hdr);
            }
            image[4u * pixel + 3] = 255;
        }
    }
    if (peak < 0.2f || !shafts) fail(@"Capture lacks expected beam signal", nil);
    printf("quality %s phase=%u rgb_mae=%.6f rgb_rmse=%.6f shaft_mae=%.6f "
           "shaft_pixels=%zu rgb_peak=%.5f hdr_channels_above_one=%zu\n",
           label.UTF8String, phase, absolute / (3.0 * pixels), sqrt(squared / (3.0 * pixels)),
           shaftAbsolute / (3.0 * shafts), shafts, peak, highlights);
    savePNG(url, image, raster.outputWidth, raster.outputHeight);
}

static id<MTLFXSpatialScaler> makeScaler(id<MTLDevice> device, Raster raster,
                                         MTLPixelFormat format, BOOL hdr)
{
    MTLFXSpatialScalerDescriptor *desc = [MTLFXSpatialScalerDescriptor new];
    desc.inputWidth = raster.inputWidth;
    desc.inputHeight = raster.inputHeight;
    desc.outputWidth = raster.outputWidth;
    desc.outputHeight = raster.outputHeight;
    desc.colorTextureFormat = format;
    desc.outputTextureFormat = format;
    desc.colorProcessingMode = hdr ? MTLFXSpatialScalerColorProcessingModeHDR
                                   : MTLFXSpatialScalerColorProcessingModeLinear;
    id<MTLFXSpatialScaler> scaler = [desc newSpatialScalerWithDevice:device];
    scaler.inputContentWidth = raster.inputWidth;
    scaler.inputContentHeight = raster.inputHeight;
    return scaler;
}

static void benchmark(id<MTLCommandQueue> queue, id<MTLRenderPipelineState> pipeline,
                      Raster raster, BOOL hdr, BOOL tryMetalFX, unsigned samples,
                      NSURL *directory)
{
    MTLPixelFormat format = hdr ? MTLPixelFormatRGBA16Float : MTLPixelFormatRGBA8Unorm;
    NSString *group = [NSString stringWithFormat:@"%s-%s", raster.name, hdr ? "hdr" : "sdr"];
    double began = nowMS();
    id<MTLFXSpatialScaler> scaler = tryMetalFX ? makeScaler(queue.device, raster, format, hdr) : nil;
    printf("raster=%s input=%ux%u output=%ux%u format=%s metalfx_setup_ms=%.4f\n",
           group.UTF8String, raster.inputWidth, raster.inputHeight, raster.outputWidth,
           raster.outputHeight, hdr ? "rgba16float-linear-hdr" : "rgba8unorm-linear-sdr",
           nowMS() - began);
    if (tryMetalFX && !scaler) printf("skip %s route=metalfx reason=descriptor-rejected\n", group.UTF8String);

    unsigned routes = scaler ? 2 : 1;
    id<MTLTexture> inputs[phases];
    MTLTextureUsage inputUsage = MTLTextureUsageShaderRead | scaler.colorTextureUsage;
    for (unsigned phase = 0; phase < phases; ++phase) {
        inputs[phase] = privateTexture(queue.device, format, raster.inputWidth, raster.inputHeight,
                                       inputUsage);
        upload(queue, inputs[phase], raster.inputWidth, raster.inputHeight, phase, hdr);
    }
    id<MTLTexture> outputs[2];
    NSString *labels[2];
    NSString *copyLabels[2];
    Timing *resident[2];
    Timing *copies[2];
    for (unsigned route = 0; route < routes; ++route) {
        MTLTextureUsage usage = route ? scaler.outputTextureUsage : MTLTextureUsageRenderTarget;
        outputs[route] = privateTexture(queue.device, format, raster.outputWidth, raster.outputHeight,
                                        usage | MTLTextureUsageShaderRead);
        labels[route] = [NSString stringWithFormat:@"%@ route=%s", group, routeNames[route]];
        copyLabels[route] = [labels[route] stringByAppendingString:@" diagnostic-readback"];
        resident[route] = calloc(samples, sizeof(Timing));
        copies[route] = calloc(samples, sizeof(Timing));
        if (!resident[route] || !copies[route]) fail(@"Allocate timing samples", nil);
    }

    for (unsigned frame = 0; frame < warmFrames + samples; ++frame) {
        @autoreleasepool {
            for (unsigned index = 0; index < routes; ++index) {
                unsigned route = (index + frame / phases) % routes;
                Timing timing = scale(queue, pipeline, route ? scaler : nil,
                                      inputs[frame % phases], outputs[route], labels[route]);
                if (frame >= warmFrames) resident[route][frame - warmFrames] = timing;
            }
        }
    }

    size_t stride = rowBytes(raster.outputWidth, hdr);
    id<MTLBuffer> staging = sharedBuffer(queue.device, stride * raster.outputHeight);
    for (unsigned frame = 0; frame < warmFrames + samples; ++frame) {
        @autoreleasepool {
            for (unsigned index = 0; index < routes; ++index) {
                unsigned route = (index + frame) % routes;
                Timing timing = readback(queue, outputs[route], staging, stride, copyLabels[route]);
                if (frame >= warmFrames) copies[route][frame - warmFrames] = timing;
            }
        }
    }
    printf("readback_bytes=%zu raster=%s\n", stride * raster.outputHeight, group.UTF8String);
    for (unsigned route = 0; route < routes; ++route) {
        reportMetric(labels[route], "resident_gpu", resident[route], samples, YES);
        reportMetric(labels[route], "resident_wall", resident[route], samples, NO);
        reportMetric(labels[route], "readback_gpu", copies[route], samples, YES);
        reportMetric(labels[route], "readback_wall", copies[route], samples, NO);
        free(resident[route]);
        free(copies[route]);
    }

    SceneSample *reference = malloc((size_t)raster.outputWidth * raster.outputHeight * sizeof(*reference));
    if (!reference) fail(@"Allocate direct display-resolution reference", nil);
    for (unsigned phase = 0; phase < phases; ++phase) {
        @autoreleasepool {
            for (unsigned y = 0; y < raster.outputHeight; ++y) {
                for (unsigned x = 0; x < raster.outputWidth; ++x) {
                    reference[(size_t)y * raster.outputWidth + x] =
                        roomSample(x, y, raster.outputWidth, raster.outputHeight, phase, hdr);
                }
            }
            NSString *referenceLabel = [group stringByAppendingString:@" route=reference"];
            NSString *filename = [NSString stringWithFormat:@"%@-reference-%u.png", group, phase];
            capture(raster, hdr, phase, reference, NULL, stride, referenceLabel,
                    [directory URLByAppendingPathComponent:filename]);
            for (unsigned route = 0; route < routes; ++route) {
                scale(queue, pipeline, route ? scaler : nil, inputs[phase], outputs[route], labels[route]);
                readback(queue, outputs[route], staging, stride, copyLabels[route]);
                filename = [NSString stringWithFormat:@"%@-%s-%u.png", group, routeNames[route], phase];
                capture(raster, hdr, phase, reference, staging.contents, stride, labels[route],
                        [directory URLByAppendingPathComponent:filename]);
            }
        }
    }
    free(reference);
}

int main(int argc, const char **argv)
{
    @autoreleasepool {
        if (argc != 7) return 64;
        const char *rank = argv[1];
        const char *variant = argv[2];
        char *end = NULL;
        errno = 0;
        unsigned long parsed = strtoul(argv[3], &end, 10);
        BOOL validCount = isdigit((unsigned char)argv[3][0])
                       && !errno && end && !*end && parsed >= 20 && parsed <= 10000;
        BOOL both = !strcmp(argv[4], "both");
        if (!validCount
            || (!both && strcmp(argv[4], "portable"))
            || (strcmp(rank, "small") && strcmp(rank, "mobile") && strcmp(rank, "desktop") && strcmp(rank, "all"))
            || (strcmp(variant, "sdr") && strcmp(variant, "hdr") && strcmp(variant, "all"))) {
            fputs("Expected {small|mobile|desktop|all} {sdr|hdr|all} "
                  "{20..10000} {both|portable} RESULTS SHADER\n", stderr);
            return 64;
        }

        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        if (!device) fail(@"No default Metal device", nil);
        BOOL supported = [MTLFXSpatialScalerDescriptor supportsDevice:device];
        printf("device=%s unified_memory=%d spatial_supported=%d selection=%s warmup=%d\n",
               device.name.UTF8String, device.hasUnifiedMemory, supported, argv[4], warmFrames);
        if (both && !supported) puts("skip route=metalfx reason=device-unsupported");

        NSError *error = nil;
        NSString *source = [NSString stringWithContentsOfFile:@(argv[6])
                                                     encoding:NSUTF8StringEncoding error:&error];
        if (!source) fail(@"Read bilinear Metal source", error);
        double began = nowMS();
        id<MTLLibrary> library = [device newLibraryWithSource:source options:nil error:&error];
        if (!library) fail(@"Compile bilinear Metal source", error);
        printf("bilinear_library_setup_ms=%.4f\n", nowMS() - began);
        id<MTLCommandQueue> queue = [device newCommandQueue];
        if (!queue) fail(@"Create Metal command queue", nil);
        queue.label = @"Tolglow #542 standalone probe";
        NSURL *directory = [NSURL fileURLWithPath:@(argv[5]) isDirectory:YES];

        for (unsigned hdr = 0; hdr <= 1; ++hdr) {
            if (strcmp(variant, "all") && strcmp(variant, hdr ? "hdr" : "sdr")) continue;
            MTLPixelFormat format = hdr ? MTLPixelFormatRGBA16Float : MTLPixelFormatRGBA8Unorm;
            began = nowMS();
            id<MTLRenderPipelineState> pipeline = bilinearPipeline(device, library, format);
            printf("bilinear_pipeline_setup_ms=%.4f format=%s\n", nowMS() - began, hdr ? "hdr" : "sdr");
            for (unsigned index = 0; index < sizeof(rasters) / sizeof(rasters[0]); ++index) {
                if (!strcmp(rank, "all") || !strcmp(rank, rasters[index].name)) {
                    @autoreleasepool {
                        benchmark(queue, pipeline, rasters[index], hdr, both && supported,
                                  (unsigned)parsed, directory);
                    }
                }
            }
        }
    }
    return 0;
}
