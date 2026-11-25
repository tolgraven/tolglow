#import <Foundation/Foundation.h>
#import <Metal/Metal.h>
#import <simd/simd.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

typedef struct { float xyz[3]; } Vertex;
typedef struct {
    vector_float4 originAndSegmentMax;
    vector_float4 directionAndSegmentMin;
} RayInput;
typedef struct { float distance; uint32_t primitive, blocked, completed; } RayResult;

_Static_assert(sizeof(Vertex) == 12, "Packed Float3 vertex layout");
_Static_assert(sizeof(RayInput) == 32, "MSL RayInput layout");
_Static_assert(sizeof(RayResult) == 16, "MSL RayResult layout");

static int fail(NSString *message) {
    fprintf(stderr, "status=failed %s\n", message.UTF8String);
    return 1;
}

static double seconds(void) {
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC, &time);
    return (double)time.tv_sec + (double)time.tv_nsec * 1e-9;
}

static void reportCommand(NSString *name, id<MTLCommandBuffer> command, double start) {
    printf("%s_submit_to_completion_ms=%.6f\n", name.UTF8String, (seconds() - start) * 1e3);
    if (command.GPUStartTime > 0 && command.GPUEndTime > command.GPUStartTime) {
        printf("%s_command_gpu_ms=%.6f\n", name.UTF8String,
               (command.GPUEndTime - command.GPUStartTime) * 1e3);
    } else {
        printf("%s_command_gpu_ms=unavailable\n", name.UTF8String);
    }
}

static int run(const char *libraryPath, const char *fixturePath) {
    id<MTLDevice> device = MTLCreateSystemDefaultDevice();
    if (device == nil) {
        puts("status=portable device=unavailable");
        return 2;
    }
    BOOL apple9 = [device supportsFamily:MTLGPUFamilyApple9];
    BOOL apple10 = NO;
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 260000
    if (@available(macOS 26.0, *)) {
        apple10 = [device supportsFamily:MTLGPUFamilyApple10];
    }
#endif
    BOOL acceleratorEligible = apple9 || apple10;
    BOOL allowAPIOnly = [NSProcessInfo.processInfo.environment[@"ALLOW_API_ONLY"] isEqualToString:@"1"];
    printf("device=%s registry_id=0x%llx unified_memory=%d raytracing_api=%d apple9=%d apple10=%d\n",
           device.name.UTF8String, (unsigned long long)device.registryID, device.hasUnifiedMemory,
           device.supportsRaytracing, apple9, apple10);
    if (!device.supportsRaytracing || (!acceleratorEligible && !allowAPIOnly)) {
        puts("status=portable reason=hardware-gate");
        return 2;
    }

    NSError *error = nil;
    NSString *fixture = [NSString stringWithContentsOfFile:@(fixturePath) encoding:NSUTF8StringEncoding error:&error];
    if (!fixture) return fail(error.localizedDescription);

    NSMutableData *vertices = [NSMutableData data];
    NSMutableData *rays = [NSMutableData data];
    NSMutableArray<NSString *> *receivers = [NSMutableArray array];
    NSMutableArray<NSArray<NSString *> *> *cases = [NSMutableArray array];
    NSUInteger line = 0;
    for (NSString *record in [fixture componentsSeparatedByString:@"\n"]) {
        line++;
        NSArray<NSString *> *fields = [record componentsSeparatedByString:@"\t"];
        if (record.length == 0 || [fields[0] isEqualToString:@"#"]) continue;
        if ([fields[0] isEqualToString:@"tri"] && fields.count == 11) {
            Vertex triangle[3];
            for (NSUInteger corner = 0; corner < 3; corner++) {
                for (NSUInteger axis = 0; axis < 3; axis++) {
                    triangle[corner].xyz[axis] = fields[2 + 3 * corner + axis].floatValue;
                }
            }
            [vertices appendBytes:triangle length:sizeof(triangle)];
            [receivers addObject:fields[1]];
        } else if ([fields[0] isEqualToString:@"ray"] && fields.count == 13) {
            // Round into the open interval even if converting to float rounded past it.
            float segmentMax = nextafterf((float)(fields[8].doubleValue - 1e-9), -INFINITY);
            float segmentMin = nextafterf(1e-10f, INFINITY);
            if (!(segmentMax >= segmentMin)) return fail(@"invalid fixture segment limits");
            RayInput ray = {
                {fields[2].floatValue, fields[3].floatValue, fields[4].floatValue, segmentMax},
                {fields[5].floatValue, fields[6].floatValue, fields[7].floatValue, segmentMin}
            };
            [rays appendBytes:&ray length:sizeof(ray)];
            [cases addObject:fields];
        } else {
            return fail([NSString stringWithFormat:@"invalid fixture line %lu", (unsigned long)line]);
        }
    }
    if (receivers.count == 0 || cases.count == 0 || cases.count > UINT32_MAX / 4096) {
        return fail(@"invalid triangle or ray count");
    }
    NSString *repeatSetting = NSProcessInfo.processInfo.environment[@"RAY_REPEATS"] ?: @"1";
    NSInteger repeats = repeatSetting.integerValue;
    if (repeats < 1 || repeats > 4096) return fail(@"RAY_REPEATS must be from 1 to 4096");
    uint32_t caseCount = (uint32_t)cases.count;
    NSUInteger rayCount = caseCount * (NSUInteger)repeats;
    printf("triangles=%lu cases=%u rays=%lu intersector_calls=%lu\n", (unsigned long)receivers.count,
           caseCount, (unsigned long)rayCount, (unsigned long)(2 * rayCount));

    double start = seconds();
    id<MTLLibrary> library = [device newLibraryWithURL:[NSURL fileURLWithPath:@(libraryPath)] error:&error];
    if (!library) return fail(error.localizedDescription);
    id<MTLFunction> function = [library newFunctionWithName:@"fixtureSightlines"];
    if (!function) return fail(@"fixtureSightlines is absent from the library");
    id<MTLComputePipelineState> pipeline = [device newComputePipelineStateWithFunction:function error:&error];
    if (!pipeline) return fail(error.localizedDescription);
    id<MTLCommandQueue> queue = [device newCommandQueue];
    if (!queue) return fail(@"could not allocate Metal command queue");
    printf("pipeline_queue_setup_ms=%.6f\n", (seconds() - start) * 1e3);

    start = seconds();
    id<MTLBuffer> mesh = [device newBufferWithBytes:vertices.bytes length:vertices.length options:MTLResourceStorageModeShared];
    double meshUpload = (seconds() - start) * 1e3;
    start = seconds();
    id<MTLBuffer> input = [device newBufferWithBytes:rays.bytes length:rays.length options:MTLResourceStorageModeShared];
    double rayUpload = (seconds() - start) * 1e3;
    id<MTLBuffer> output = [device newBufferWithLength:rayCount * sizeof(RayResult) options:MTLResourceStorageModeShared];
    if (!mesh || !input || !output) return fail(@"could not allocate shared buffers");
    memset(output.contents, 0, output.length);
    printf("mesh_shared_copy_ms=%.6f ray_shared_copy_ms=%.6f\n", meshUpload, rayUpload);

    MTLAccelerationStructureTriangleGeometryDescriptor *triangles = [MTLAccelerationStructureTriangleGeometryDescriptor descriptor];
    triangles.vertexBuffer = mesh;
    triangles.vertexStride = sizeof(Vertex);
    triangles.triangleCount = receivers.count;
    triangles.opaque = YES;
    MTLPrimitiveAccelerationStructureDescriptor *room = [MTLPrimitiveAccelerationStructureDescriptor descriptor];
    room.geometryDescriptors = @[triangles];
    MTLAccelerationStructureSizes sizes = [device accelerationStructureSizesWithDescriptor:room];
    id<MTLAccelerationStructure> structure = [device newAccelerationStructureWithSize:sizes.accelerationStructureSize];
    id<MTLBuffer> scratch = [device newBufferWithLength:sizes.buildScratchBufferSize options:MTLResourceStorageModePrivate];
    if (!structure || !scratch) return fail(@"could not allocate acceleration structure or scratch buffer");
    structure.label = @"Tolglow static room triangles";

    NSString *capture = NSProcessInfo.processInfo.environment[@"CAPTURE_PATH"];
    MTLCaptureManager *captureManager = [MTLCaptureManager sharedCaptureManager];
    if (capture.length) {
        if (![captureManager supportsDestination:MTLCaptureDestinationGPUTraceDocument]) {
            return fail(@"Metal GPU trace file capture is unavailable");
        }
        MTLCaptureDescriptor *descriptor = [MTLCaptureDescriptor new];
        descriptor.captureObject = queue;
        descriptor.destination = MTLCaptureDestinationGPUTraceDocument;
        descriptor.outputURL = [NSURL fileURLWithPath:capture];
        if (![captureManager startCaptureWithDescriptor:descriptor error:&error]) return fail(error.localizedDescription);
    }

    start = seconds();
    id<MTLCommandBuffer> build = [queue commandBuffer];
    build.label = @"Build static receiver acceleration structure";
    id<MTLAccelerationStructureCommandEncoder> builder = [build accelerationStructureCommandEncoder];
    if (!builder) return fail(@"could not create acceleration structure encoder");
    [builder buildAccelerationStructure:structure descriptor:room scratchBuffer:scratch scratchBufferOffset:0];
    [builder endEncoding];
    printf("build_encode_ms=%.6f\n", (seconds() - start) * 1e3);
    start = seconds();
    [build commit];
    [build waitUntilCompleted];
    if (build.status != MTLCommandBufferStatusCompleted) return fail(build.error.localizedDescription ?: @"build incomplete");
    reportCommand(@"build", build, start);

    start = seconds();
    id<MTLCommandBuffer> query = [queue commandBuffer];
    query.label = @"Fixture nearest-hit and open-segment batch";
    id<MTLComputeCommandEncoder> compute = [query computeCommandEncoder];
    if (!compute) return fail(@"could not create compute encoder");
    compute.label = @"Direct Metal fixture intersector";
    [compute setComputePipelineState:pipeline];
    [compute setAccelerationStructure:structure atBufferIndex:0];
    [compute setBytes:&caseCount length:sizeof(caseCount) atIndex:1];
    [compute setBuffer:input offset:0 atIndex:2];
    [compute setBuffer:output offset:0 atIndex:3];
    [compute dispatchThreads:MTLSizeMake(rayCount, 1, 1)
        threadsPerThreadgroup:MTLSizeMake(pipeline.threadExecutionWidth, 1, 1)];
    [compute endEncoding];
    printf("query_encode_ms=%.6f\n", (seconds() - start) * 1e3);
    start = seconds();
    [query commit];
    [query waitUntilCompleted];
    if (query.status != MTLCommandBufferStatusCompleted) return fail(query.error.localizedDescription ?: @"dispatch incomplete");
    reportCommand(@"query", query, start);
    if (capture.length) [captureManager stopCapture];

    start = seconds();
    NSData *readback = [NSData dataWithBytes:output.contents length:output.length];
    printf("shared_readback_copy_ms=%.6f\n", (seconds() - start) * 1e3);
    const RayResult *results = readback.bytes;
    for (NSUInteger i = 0; i < rayCount; i++) {
        NSArray<NSString *> *expected = cases[i % caseCount];
        RayResult actual = results[i];
        NSString *actualID = actual.primitive < receivers.count ? receivers[actual.primitive] : @"-";
        NSString *canonical = expected[11];
        NSArray<NSString *> *allowed = [expected[12] componentsSeparatedByString:@","];
        double distance = [expected[9] isEqualToString:@"-"] ? -1.0 : expected[9].doubleValue;
        BOOL identityOK = distance < 0 ? actual.primitive == UINT32_MAX : [allowed containsObject:actualID];
        BOOL distanceOK = distance < 0 ? actual.distance == -1.0f
                                       : fabs((double)actual.distance - distance) <= 2e-6 * fmax(1.0, distance);
        if (actual.completed != 0x538 || !identityOK || !distanceOK || actual.blocked != expected[10].intValue) {
            return fail([NSString stringWithFormat:@"case=%@ repeat=%lu hit=%@ distance=%.9g blocked=%u cpu_hit=%@ allowed=%@ cpu_distance=%.9g cpu_blocked=%@",
                         expected[1], (unsigned long)(i / caseCount), actualID, actual.distance,
                         actual.blocked, canonical, expected[12], distance, expected[10]]);
        }
        if (i < caseCount) {
            printf("case=%s metal_hit=%s distance_m=%.9g blocked=%u cpu_hit=%s allowed=%s\n",
                   expected[1].UTF8String, actualID.UTF8String, actual.distance, actual.blocked,
                   canonical.UTF8String, expected[12].UTF8String);
        }
    }
    puts("refit=not-exercised publication=not-exercised");
    puts(acceleratorEligible ? "status=hardware-eligible-parity-pass" : "status=api-only-parity-pass");
    return 0;
}

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        if (argc != 3) return fail(@"usage: probe library.metallib fixture.tsv");
        return run(argv[1], argv[2]);
    }
}
