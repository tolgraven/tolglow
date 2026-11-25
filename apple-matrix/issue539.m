#import <Foundation/Foundation.h>
#import <Accelerate/Accelerate.h>
#import <CoreML/CoreML.h>
#import <Metal/Metal.h>
#import <MetalPerformanceShaders/MetalPerformanceShaders.h>
#import <dispatch/dispatch.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

enum { Channels = 64, CoreMLWidth = 32, TensorTile = 64, Warmup = 12 };
typedef struct { double pack, run, read, total, gpu; } Timings;
typedef BOOL (^Step)(const float *, const float *, float *, Timings *);

static double ms(void) {
    return (double)clock_gettime_nsec_np(CLOCK_UPTIME_RAW) / 1e6;
}

static uint32_t nextRandom(uint32_t *state) {
    *state = *state * 1664525u + 1013904223u;
    return *state >> 24;
}

static void fill(int rows, unsigned version, float *x, float *weights) {
    uint32_t fixtureState = 539 + version;
    uint32_t weightsState = 305 + version;
    for (int i = 0; i < rows * Channels; i++) {
        x[i] = (i % Channels == Channels - 1) ? 1.0f : nextRandom(&fixtureState) / 255.0f;
    }
    for (int i = 0; i < Channels * Channels; i++) {
        float value = ((int)nextRandom(&weightsState) - 127) / 8192.0f;
        weights[i] = (i / Channels == Channels - 1) ? 0.5f + value : value;
    }
}

static void reference(int rows, const float *x, const float *w, float *y) {
    for (int row = 0; row < rows; row++) {
        for (int col = 0; col < Channels; col++) {
            double sum = 0.0;
            for (int k = 0; k < Channels; k++)
                sum += (double)x[row * Channels + k] * w[k * Channels + col];
            y[row * Channels + col] = (float)sum;
        }
    }
}

static int byteFor(float value) {
    return (int)lroundf(fminf(1.f, fmaxf(0.f, value)) * 255.f);
}

static int compareDouble(const void *a, const void *b) {
    const double x = *(const double *)a, y = *(const double *)b;
    return (x > y) - (x < y);
}

static void percentiles(double *data, int count) {
    qsort(data, count, sizeof(*data), compareDouble);
    printf(" %.4f/%.4f/%.4f", data[(count * 50 + 99) / 100 - 1],
           data[(count * 95 + 99) / 100 - 1], data[(count * 99 + 99) / 100 - 1]);
}

static BOOL measure(const char *name, int rows, int iterations, BOOL fixedWeights,
                    BOOL gpuTimes, float *const x[2], float *const w[2], Step step) {
    size_t elements = (size_t)rows * Channels;
    __attribute__((objc_precise_lifetime)) NSMutableData *output =
        [NSMutableData dataWithLength:elements * sizeof(float)];
    __attribute__((objc_precise_lifetime)) NSMutableData *expected =
        [NSMutableData dataWithLength:elements * sizeof(float)];
    __attribute__((objc_precise_lifetime)) NSMutableData *samples =
        [NSMutableData dataWithLength:5 * iterations * sizeof(double)];
    float *y = output.mutableBytes;
    float *ref = expected.mutableBytes;
    double *data = samples.mutableBytes;
    double first = 0.0, worst = 0.0;
    size_t bad = 0, packetDiffs = 0;
    for (int i = -Warmup; i < iterations; i++) {
        @autoreleasepool {
            int version = i & 1;
            const float *weights = w[fixedWeights ? 0 : version];
            Timings timing = {0};
            if (!step(x[version], weights, y, &timing)) return NO;
            if (i == -Warmup) first = timing.total;
            if (i >= 0) {
                data[i] = timing.pack;
                data[iterations + i] = timing.run;
                data[2 * iterations + i] = timing.read;
                data[3 * iterations + i] = timing.total;
                data[4 * iterations + i] = timing.gpu;
            }
            if (i == -2 || i == -1 || i >= iterations - 2) {
                reference(rows, x[version], weights, ref);
                for (size_t j = 0; j < elements; j++) {
                    double delta = fabs((double)y[j] - ref[j]);
                    if (!isfinite(y[j]) || delta > 0.001) bad++;
                    if (!isfinite(y[j]) || byteFor(y[j]) != byteFor(ref[j])) packetDiffs++;
                    worst = fmax(worst, delta);
                }
            }
        }
    }
    printf("result %-21s rows=%-5d first_ms=%.4f max_abs=%g numeric_fail=%zu synthetic_byte_diff=%zu/%zu |",
           name, rows, first, worst, bad, packetDiffs, 4 * elements);
    for (int phase = 0; phase < 4 + gpuTimes; phase++)
        percentiles(data + phase * iterations, iterations);
    puts("");
    return bad == 0;
}

static BOOL fail(const char *where, NSError *error) {
    fprintf(stderr, "%s: %s\n", where, error ? error.localizedDescription.UTF8String : "no result");
    return NO;
}

static id<MTLBuffer> sharedBuffer(id<MTLDevice> device, size_t length) {
    return [device newBufferWithLength:length options:MTLResourceStorageModeShared];
}

static Step mpsStep(id<MTLDevice> device, int rows) {
    id<MTLCommandQueue> queue = [device newCommandQueue];
    if (!queue || !MPSSupportsMTLDevice(device)) return nil;
    size_t rowBytes = Channels * sizeof(float), bytes = rows * rowBytes;
    id<MTLBuffer> x = sharedBuffer(device, bytes);
    id<MTLBuffer> w = sharedBuffer(device, Channels * rowBytes);
    id<MTLBuffer> y = sharedBuffer(device, bytes);
    if (!x || !w || !y) return nil;
    MPSMatrixDescriptor *shape = [MPSMatrixDescriptor matrixDescriptorWithRows:rows
        columns:Channels rowBytes:rowBytes dataType:MPSDataTypeFloat32];
    MPSMatrixDescriptor *weights = [MPSMatrixDescriptor matrixDescriptorWithRows:Channels
        columns:Channels rowBytes:rowBytes dataType:MPSDataTypeFloat32];
    MPSMatrix *a = [[MPSMatrix alloc] initWithBuffer:x descriptor:shape];
    MPSMatrix *b = [[MPSMatrix alloc] initWithBuffer:w descriptor:weights];
    MPSMatrix *c = [[MPSMatrix alloc] initWithBuffer:y descriptor:shape];
    MPSMatrixMultiplication *op = [[MPSMatrixMultiplication alloc] initWithDevice:device
        transposeLeft:NO transposeRight:NO resultRows:rows resultColumns:Channels
        interiorColumns:Channels alpha:1.0 beta:0.0];
    return ^BOOL(const float *input, const float *params, float *output, Timings *t) {
        double start = ms();
        memcpy(x.contents, input, bytes);
        memcpy(w.contents, params, Channels * rowBytes);
        double prepared = ms();
        id<MTLCommandBuffer> cmd = [queue commandBuffer];
        cmd.label = @"#539 MPS affine";
        [op encodeToCommandBuffer:cmd leftMatrix:a rightMatrix:b resultMatrix:c];
        [cmd commit];
        [cmd waitUntilCompleted];
        double done = ms();
        if (!cmd || cmd.status != MTLCommandBufferStatusCompleted)
            return fail("MPS command", cmd.error);
        memcpy(output, y.contents, bytes);
        double end = ms();
        *t = (Timings){prepared - start, done - prepared, end - done, end - start,
                       (cmd.GPUEndTime - cmd.GPUStartTime) * 1000.0};
        return YES;
    };
}

// Serialize the few messages this fixture needs from Apple's public Core ML model schema.
static void varint(NSMutableData *msg, uint64_t value) {
    while (value >= 128) {
        uint8_t byte = (uint8_t)value | 128;
        [msg appendBytes:&byte length:1];
        value >>= 7;
    }
    uint8_t byte = (uint8_t)value;
    [msg appendBytes:&byte length:1];
}

static void integer(NSMutableData *msg, unsigned field, uint64_t value) {
    varint(msg, field * 8);
    varint(msg, value);
}

static void message(NSMutableData *msg, unsigned field, NSData *value) {
    varint(msg, field * 8 + 2);
    varint(msg, value.length);
    [msg appendData:value];
}

static void string(NSMutableData *msg, unsigned field, NSString *value) {
    message(msg, field, [value dataUsingEncoding:NSUTF8StringEncoding]);
}

static NSData *feature(NSString *name, NSArray<NSNumber *> *dims) {
    NSMutableData *array = [NSMutableData data];
    for (NSNumber *dim in dims) integer(array, 1, dim.unsignedLongLongValue);
    integer(array, 2, 65568); // ArrayFeatureType.FLOAT32
    NSMutableData *type = [NSMutableData data];
    message(type, 5, array); // FeatureType.multiArrayType
    NSMutableData *spec = [NSMutableData data];
    string(spec, 1, name);
    message(spec, 3, type);
    return spec;
}

static NSData *modelBytes(NSArray<NSNumber *> *shape, const float *fixedWeights) {
    NSMutableData *description = [NSMutableData data];
    message(description, 1, feature(@"x", shape));
    message(description, 10, feature(@"y", shape));

    NSMutableData *layer = [NSMutableData data];
    string(layer, 1, @"lighting_affine");
    string(layer, 2, @"x");
    string(layer, 3, @"y");
    if (fixedWeights) {
        float transposed[Channels * Channels];
        for (int out = 0; out < Channels; out++)
            for (int in = 0; in < Channels; in++)
                transposed[out * Channels + in] = fixedWeights[in * Channels + out];
        NSMutableData *weights = [NSMutableData data];
        message(weights, 1, [NSData dataWithBytes:transposed length:sizeof(transposed)]);
        NSMutableData *conv = [NSMutableData data];
        integer(conv, 1, Channels); // outputChannels
        integer(conv, 2, Channels); // kernelChannels
        integer(conv, 10, 1);       // nGroups
        for (int i = 0; i < 2; i++) {
            integer(conv, 20, 1);  // kernelSize
            integer(conv, 30, 1);  // stride
            integer(conv, 40, 1);  // dilationFactor
        }
        NSMutableData *borders = [NSMutableData data];
        message(borders, 10, [NSData data]); // height, both edges zero
        message(borders, 10, [NSData data]); // width, both edges zero
        NSMutableData *valid = [NSMutableData data];
        message(valid, 1, borders); // paddingAmounts
        message(conv, 50, valid); // valid padding
        message(conv, 90, weights);
        message(layer, 100, conv); // convolution
    } else {
        message(description, 1, feature(@"w", @[@(Channels), @(Channels)]));
        string(layer, 2, @"w");
        message(layer, 1045, [NSData data]); // batchedMatmul, two inputs
    }

    NSMutableData *nn = [NSMutableData data];
    message(nn, 1, layer);
    if (!fixedWeights) integer(nn, 5, 1); // EXACT_ARRAY_MAPPING
    NSMutableData *model = [NSMutableData data];
    integer(model, 1, fixedWeights ? 1 : 4); // specificationVersion
    message(model, 2, description);
    message(model, 500, nn); // neuralNetwork
    return model;
}

static void reportPlan(NSURL *compiled, MLModelConfiguration *config, const char *route) {
    if (@available(macOS 14.4, *)) {
        dispatch_semaphore_t ready = dispatch_semaphore_create(0);
        __block MLComputePlan *plan;
        __block NSError *planError;
        [MLComputePlan loadContentsOfURL:compiled configuration:config
                       completionHandler:^(MLComputePlan *loaded, NSError *error) {
            plan = loaded;
            planError = error;
            dispatch_semaphore_signal(ready);
        }];
        if (dispatch_semaphore_wait(ready, dispatch_time(DISPATCH_TIME_NOW, 60 * NSEC_PER_SEC))) {
            fprintf(stderr, "plan %s timed out; placement unknown\n", route);
            return;
        }
        NSArray *layers = plan.modelStructure.neuralNetwork.layers;
        if (!layers.count) {
            fprintf(stderr, "plan %s unavailable: %s; placement unknown\n", route,
                    planError ? planError.localizedDescription.UTF8String : "no neural-network layers");
            return;
        }
        for (MLModelStructureNeuralNetworkLayer *layer in layers) {
            MLComputePlanDeviceUsage *usage = [plan computeDeviceUsageForNeuralNetworkLayer:layer];
            NSMutableArray *supported = [NSMutableArray array];
            for (id<MLComputeDeviceProtocol> device in usage.supportedComputeDevices)
                [supported addObject:NSStringFromClass([device class])];
            NSString *preferred = usage.preferredComputeDevice
                ? NSStringFromClass([usage.preferredComputeDevice class]) : @"unknown";
            printf("plan %s layer=%s type=%s preferred=%s supported=%s\n", route,
                   layer.name.UTF8String, layer.type.UTF8String, preferred.UTF8String,
                   [supported componentsJoinedByString:@","].UTF8String);
        }
    }
}

static void writeArray(MLMultiArray *array, const float *values, int rows, BOOL chw) {
    [array getMutableBytesWithHandler:^(void *buffer, NSInteger size, NSArray<NSNumber *> *strides) {
        (void)size;
        float *data = buffer;
        NSInteger s0 = strides[0].integerValue, s1 = strides[1].integerValue;
        if (chw) {
            int width = array.shape[2].intValue;
            NSInteger s2 = strides[2].integerValue;
            for (int channel = 0; channel < Channels; channel++)
                for (int row = 0; row < rows; row++)
                    data[channel * s0 + (row / width) * s1 + (row % width) * s2] =
                        values[row * Channels + channel];
        } else if (s0 == Channels && s1 == 1) {
            memcpy(data, values, (size_t)rows * Channels * sizeof(float));
        } else {
            for (int row = 0; row < rows; row++)
                for (int channel = 0; channel < Channels; channel++)
                    data[row * s0 + channel * s1] = values[row * Channels + channel];
        }
    }];
}

static void readArray(MLMultiArray *array, float *values, int rows, BOOL chw) {
    NSArray<NSNumber *> *strides = array.strides;
    int width = chw ? array.shape[2].intValue : 0;
    [array getBytesWithHandler:^(const void *buffer, NSInteger size) {
        (void)size;
        const float *data = buffer;
        NSInteger s0 = strides[0].integerValue, s1 = strides[1].integerValue;
        if (chw) {
            NSInteger s2 = strides[2].integerValue;
            for (int channel = 0; channel < Channels; channel++)
                for (int row = 0; row < rows; row++)
                    values[row * Channels + channel] =
                        data[channel * s0 + (row / width) * s1 + (row % width) * s2];
        } else if (s0 == Channels && s1 == 1) {
            memcpy(values, data, (size_t)rows * Channels * sizeof(float));
        } else {
            for (int row = 0; row < rows; row++)
                for (int channel = 0; channel < Channels; channel++)
                    values[row * Channels + channel] = data[row * s0 + channel * s1];
        }
    }];
}

static BOOL coreML(int rows, int iterations, float *const x[2], float *const w[2],
                   BOOL fixed, NSString *mode, NSURL *directory) {
    int width = rows == 1 ? 1 : CoreMLWidth;
    NSArray *shape = fixed ? @[@(Channels), @(rows / width), @(width)] : @[@(rows), @(Channels)];
    NSString *filename = [NSString stringWithFormat:@"%@-%d.mlmodel", fixed ? @"fixed" : @"dynamic", rows];
    NSURL *source = [directory URLByAppendingPathComponent:filename];
    NSError *error = nil;
    if (![modelBytes(shape, fixed ? w[0] : NULL) writeToURL:source options:NSDataWritingAtomic error:&error])
        return fail("write model", error);

    double start = ms();
    NSURL *compiled = [MLModel compileModelAtURL:source error:&error];
    double compile = ms() - start;
    if (!compiled) return fail("compile model", error);

    BOOL ok = YES;
    for (NSString *units in @[@"cpu", @"gpu", @"ane", @"auto"]) {
        if (![mode isEqualToString:@"all"] && ![mode isEqualToString:units]) continue;
        @autoreleasepool {
            MLModelConfiguration *config = [MLModelConfiguration new];
            if ([units isEqualToString:@"cpu"]) config.computeUnits = MLComputeUnitsCPUOnly;
            if ([units isEqualToString:@"gpu"]) config.computeUnits = MLComputeUnitsCPUAndGPU;
            if ([units isEqualToString:@"ane"]) config.computeUnits = MLComputeUnitsCPUAndNeuralEngine;
            if ([units isEqualToString:@"auto"]) config.computeUnits = MLComputeUnitsAll;

            NSString *label = [NSString stringWithFormat:@"coreml-%@-%@", fixed ? @"fixed" : @"dynamic", units];
            start = ms();
            MLModel *model = [MLModel modelWithContentsOfURL:compiled configuration:config error:&error];
            double load = ms() - start;
            if (!model) {
                ok = fail(label.UTF8String, error);
                continue;
            }
            printf("setup %s rows=%d compile_ms=%.4f load_ms=%.4f source=%s\n",
                   label.UTF8String, rows, compile, load, source.path.UTF8String);
            MLMultiArray *a = [[MLMultiArray alloc] initWithShape:shape
                                                       dataType:MLMultiArrayDataTypeFloat32 error:&error];
            MLMultiArray *b = fixed ? nil : [[MLMultiArray alloc] initWithShape:@[@(Channels), @(Channels)]
                                                       dataType:MLMultiArrayDataTypeFloat32 error:&error];
            if (!a || (!fixed && !b)) {
                ok = fail("allocate Core ML multiarrays", error);
                continue;
            }
            NSMutableDictionary *features = [NSMutableDictionary dictionaryWithObject:[MLFeatureValue
                featureValueWithMultiArray:a] forKey:@"x"];
            if (!fixed) features[@"w"] = [MLFeatureValue featureValueWithMultiArray:b];
            MLDictionaryFeatureProvider *provider = [[MLDictionaryFeatureProvider alloc]
                initWithDictionary:features error:&error];
            if (!provider) {
                ok = fail("prepare Core ML inputs", error);
                continue;
            }

            Step step = ^BOOL(const float *input, const float *params, float *output, Timings *t) {
                NSError *predictionError = nil;
                double begin = ms();
                writeArray(a, input, rows, fixed);
                if (!fixed) writeArray(b, params, Channels, NO);
                double prepared = ms();
                id<MLFeatureProvider> prediction = [model predictionFromFeatures:provider error:&predictionError];
                double done = ms();
                MLMultiArray *result = [prediction featureValueForName:@"y"].multiArrayValue;
                if (!result || result.dataType != MLMultiArrayDataTypeFloat32 ||
                    ![result.shape isEqualToArray:shape])
                    return fail("Core ML prediction output", predictionError);
                readArray(result, output, rows, fixed);
                double end = ms();
                *t = (Timings){prepared - begin, done - prepared, end - done, end - begin, 0};
                return YES;
            };
            if (!measure(label.UTF8String, rows, iterations, fixed, NO, x, w, step)) ok = NO;
        }
    }
    // Gate the placement probe behind timings so it does not warm the current model.
    for (NSString *units in @[@"cpu", @"gpu", @"ane", @"auto"]) {
        if (![mode isEqualToString:@"all"] && ![mode isEqualToString:units]) continue;
        MLModelConfiguration *config = [MLModelConfiguration new];
        if ([units isEqualToString:@"cpu"]) config.computeUnits = MLComputeUnitsCPUOnly;
        if ([units isEqualToString:@"gpu"]) config.computeUnits = MLComputeUnitsCPUAndGPU;
        if ([units isEqualToString:@"ane"]) config.computeUnits = MLComputeUnitsCPUAndNeuralEngine;
        if ([units isEqualToString:@"auto"]) config.computeUnits = MLComputeUnitsAll;
        NSString *label = [NSString stringWithFormat:@"coreml-%@-%@/rows=%d",
                           fixed ? @"fixed" : @"dynamic", units, rows];
        reportPlan(compiled, config, label.UTF8String);
    }
    [NSFileManager.defaultManager removeItemAtURL:compiled error:NULL];
    return ok;
}

#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 260000
API_AVAILABLE(macos(26.0))
static id<MTLTensor> tensorView(id<MTLBuffer> buffer, int rows, NSError **error) {
    const NSInteger dimensions[] = {Channels, rows};
    const NSInteger strides[] = {1, Channels};
    MTLTensorDescriptor *desc = [MTLTensorDescriptor new];
    desc.dimensions = [[MTLTensorExtents alloc] initWithRank:2 values:dimensions];
    desc.strides = [[MTLTensorExtents alloc] initWithRank:2 values:strides];
    desc.dataType = MTLTensorDataTypeFloat16;
    desc.usage = MTLTensorUsageCompute;
    desc.storageMode = MTLStorageModeShared;
    return [buffer newTensorWithDescriptor:desc offset:0 error:error];
}

API_AVAILABLE(macos(26.0))
static Step tensorStep(id<MTLDevice> device, NSURL *shaderURL, int rows) {
    NSError *error = nil;
    double setup = ms();
    id<MTLLibrary> library = [device newLibraryWithURL:shaderURL error:&error];
    if (!library) {
        fail("load Metal library", error);
        return nil;
    }
    id<MTLFunction> fn = [library newFunctionWithName:@"lightingAffine"];
    if (!fn) return nil;
    id<MTLComputePipelineState> pipeline = [device newComputePipelineStateWithFunction:fn error:&error];
    if (!pipeline || pipeline.maxTotalThreadsPerThreadgroup < 4 * pipeline.threadExecutionWidth) {
        fail("create TensorOps pipeline", error);
        return nil;
    }
    double pipelineTime = ms() - setup;

    int paddedRows = (rows + TensorTile - 1) / TensorTile * TensorTile;
    size_t rowBytes = Channels * sizeof(_Float16);
    id<MTLBuffer> x = sharedBuffer(device, paddedRows * rowBytes);
    id<MTLBuffer> w = sharedBuffer(device, Channels * rowBytes);
    id<MTLBuffer> y = sharedBuffer(device, paddedRows * rowBytes);
    if (!x || !w || !y) return nil;
    memset(x.contents, 0, paddedRows * rowBytes);
    id<MTLTensor> a = tensorView(x, paddedRows, &error);
    id<MTLTensor> b = tensorView(w, Channels, &error);
    id<MTLTensor> c = tensorView(y, paddedRows, &error);
    if (!a || !b || !c) {
        fail("create tensor views", error);
        return nil;
    }

    id<MTL4CommandQueue> queue = [device newMTL4CommandQueue];
    id<MTL4CommandBuffer> cmd = [device newCommandBuffer];
    id<MTL4CommandAllocator> allocator = [device newCommandAllocator];
    id<MTLSharedEvent> event = [device newSharedEvent];
    MTL4ArgumentTableDescriptor *argDesc = [MTL4ArgumentTableDescriptor new];
    argDesc.maxBufferBindCount = 3;
    id<MTL4ArgumentTable> args = [device newArgumentTableWithDescriptor:argDesc error:&error];
    MTLResidencySetDescriptor *resDesc = [MTLResidencySetDescriptor new];
    resDesc.initialCapacity = 6;
    id<MTLResidencySet> resident = [device newResidencySetWithDescriptor:resDesc error:&error];
    if (!queue || !cmd || !allocator || !event || !args || !resident) {
        fail("create Metal 4 command resources", error);
        return nil;
    }
    for (id<MTLAllocation> allocation in @[x, w, y, a, b, c]) [resident addAllocation:allocation];
    [resident commit];
    [queue addResidencySet:resident];
    [args setResource:a.gpuResourceID atBufferIndex:0];
    [args setResource:b.gpuResourceID atBufferIndex:1];
    [args setResource:c.gpuResourceID atBufferIndex:2];

    printf("setup tensorops rows=%d padded_rows=%d library_pipeline_ms=%.4f\n",
           rows, paddedRows, pipelineTime);
    __block uint64_t sequence = 0;
    return ^BOOL(const float *input, const float *params, float *output, Timings *t) {
        // Keep tensor views alive for the GPU resource IDs in the argument table.
        (void)a; (void)b; (void)c;
        double start = ms();
        _Float16 *inputHalf = x.contents, *weightsHalf = w.contents;
        for (int i = 0; i < rows * Channels; i++) inputHalf[i] = (_Float16)input[i];
        for (int i = 0; i < Channels * Channels; i++) weightsHalf[i] = (_Float16)params[i];
        double prepared = ms();

        [allocator reset];
        [cmd beginCommandBufferWithAllocator:allocator];
        cmd.label = @"#539 Metal 4 TensorOps affine";
        id<MTL4ComputeCommandEncoder> encoder = [cmd computeCommandEncoder];
        if (!encoder) return fail("create Metal 4 compute encoder", nil);
        [encoder setComputePipelineState:pipeline];
        [encoder setArgumentTable:args];
        [encoder dispatchThreadgroups:MTLSizeMake(Channels / TensorTile, paddedRows / TensorTile, 1)
                threadsPerThreadgroup:MTLSizeMake(4 * pipeline.threadExecutionWidth, 1, 1)];
        [encoder endEncoding];
        [cmd endCommandBuffer];
        id<MTL4CommandBuffer> submitted = cmd;
        [queue commit:&submitted count:1];
        [queue signalEvent:event value:++sequence];
        if (![event waitUntilSignaledValue:sequence timeoutMS:5000])
            return fail("Metal 4 completion wait", nil);
        double done = ms();

        const _Float16 *result = y.contents;
        for (int i = 0; i < rows * Channels; i++) output[i] = (float)result[i];
        double end = ms();
        *t = (Timings){prepared - start, done - prepared, end - done, end - start, 0};
        return YES;
    };
}
#endif

static BOOL validRows(long rows) {
    return rows == 1 || (rows >= CoreMLWidth && rows <= 20000 && rows % CoreMLWidth == 0);
}

int main(int argc, const char **argv) {
    @autoreleasepool {
        if (argc != 6) {
            fputs("usage: issue539 all|cpu|mps|tensorops|coreml-{all,cpu,gpu,ane,auto}|fixed-{all,cpu,gpu,ane,auto}"
                  " 0|ROWS ITERATIONS MODELS_DIR METALLIB|-\n", stderr);
            return 2;
        }
        setvbuf(stdout, NULL, _IOLBF, 0);
        NSString *route = @(argv[1]);
        char *endRows, *endIterations;
        long onlyRows = strtol(argv[2], &endRows, 10);
        long iterations = strtol(argv[3], &endIterations, 10);
        BOOL all = [route isEqualToString:@"all"];
        BOOL cpu = all || [route isEqualToString:@"cpu"];
        BOOL mps = all || [route isEqualToString:@"mps"];
        BOOL tensor = all || [route isEqualToString:@"tensorops"];
        BOOL dynamic = all || [route hasPrefix:@"coreml-"];
        BOOL fixed = all || [route hasPrefix:@"fixed-"];
        NSString *units = dynamic || fixed ? [[route componentsSeparatedByString:@"-"] lastObject] : @"";
        if ((!cpu && !mps && !tensor && !dynamic && !fixed) || !*argv[2] || *endRows ||
            (onlyRows != 0 && !validRows(onlyRows)) || !*argv[3] || *endIterations ||
            iterations < 2 || iterations > 100000 ||
            ((dynamic || fixed) && ![@[@"all", @"cpu", @"gpu", @"ane", @"auto"] containsObject:units])) {
            fputs("invalid route, row count, or iteration count\n", stderr);
            return 2;
        }

        NSError *error = nil;
        NSURL *directory = [NSURL fileURLWithPath:@(argv[4]) isDirectory:YES];
        if ((dynamic || fixed) && ![NSFileManager.defaultManager createDirectoryAtURL:directory
                withIntermediateDirectories:YES attributes:nil error:&error]) {
            fail("create model directory", error);
            return 1;
        }

        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        printf("system %s gpu=%s unified=%d\n", NSProcessInfo.processInfo.operatingSystemVersionString.UTF8String,
               device ? device.name.UTF8String : "none", device.hasUnifiedMemory);
        if (@available(macOS 14.0, *)) {
            for (id<MLComputeDeviceProtocol> dev in MLModel.availableComputeDevices)
                printf("coreml_available %s\n", NSStringFromClass([dev class]).UTF8String);
        }
        BOOL ok = YES, canTensor = NO;
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 260000
        if (@available(macOS 26.0, *)) {
            BOOL gpuSupportsTensor = [device supportsFamily:MTLGPUFamilyMetal4];
            canTensor = strcmp(argv[5], "-") != 0 && gpuSupportsTensor;
            printf("metal4_support=%d apple10=%d\n", gpuSupportsTensor,
                   [device supportsFamily:MTLGPUFamilyApple10]);
        }
#endif
        if (tensor && !canTensor) {
            puts("skip tensorops: need Metal 4 device, macOS 26, 26 SDK build and supplied metallib");
            if (!all) ok = NO;
        }
        puts("timings in ms, each group p50/p95/p99 | pack run_or_wait read total [MPS_GPU]");
        int sizes[] = {1, 64, 256, 1024, 20000};
        for (int k = 0; k < (onlyRows ? 1 : (int)(sizeof(sizes) / sizeof(sizes[0]))); k++) {
            @autoreleasepool {
                int rows = onlyRows ? (int)onlyRows : sizes[k];
                __attribute__((objc_precise_lifetime)) NSMutableData *x0 =
                    [NSMutableData dataWithLength:(size_t)rows * Channels * sizeof(float)];
                __attribute__((objc_precise_lifetime)) NSMutableData *x1 =
                    [NSMutableData dataWithLength:x0.length];
                float *inputs[] = {x0.mutableBytes, x1.mutableBytes};
                float w0[Channels * Channels], w1[Channels * Channels];
                float *weights[] = {w0, w1};
                fill(rows, 0, inputs[0], w0);
                fill(rows, 1, inputs[1], w1);
                if (cpu) {
                    Step step = ^BOOL(const float *input, const float *params, float *output, Timings *t) {
                        double start = ms();
                        cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, rows, Channels, Channels,
                                    1.0f, input, Channels, params, Channels, 0.0f, output, Channels);
                        double duration = ms() - start;
                        *t = (Timings){0, duration, 0, duration, 0};
                        return YES;
                    };
                    if (!measure("accelerate-fp32", rows, (int)iterations, NO, NO, inputs, weights, step)) ok = NO;
                }
                if (mps) {
                    double start = ms();
                    Step step = mpsStep(device, rows);
                    if (!step) {
                        puts("skip mps: no MPS-capable Metal device or resource allocation failed");
                        if (!all) ok = NO;
                    } else {
                        printf("setup mps rows=%d resources_ms=%.4f\n", rows, ms() - start);
                        if (!measure("mps-fp32", rows, (int)iterations, NO, YES, inputs, weights, step)) ok = NO;
                    }
                }
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 260000
                if (@available(macOS 26.0, *)) {
                    if (tensor && canTensor) {
                        Step step = tensorStep(device, [NSURL fileURLWithPath:@(argv[5])], rows);
                        if (!step || !measure("tensorops-fp16", rows, (int)iterations, NO, NO,
                                               inputs, weights, step)) ok = NO;
                    }
                }
#endif
                if (dynamic && !coreML(rows, (int)iterations, inputs, weights, NO, units, directory)) ok = NO;
                if (fixed && !coreML(rows, (int)iterations, inputs, weights, YES, units, directory)) ok = NO;
            }
        }
        return ok ? 0 : 1;
    }
}
