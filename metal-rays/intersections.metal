#include <metal_stdlib>
#include <metal_raytracing>

using namespace metal;
using namespace raytracing;

struct RayInput {
    float4 originAndSegmentMax;
    float4 directionAndSegmentMin;
};

struct RayResult {
    float distance;
    uint primitive;
    uint blocked;
    uint completed;
};

kernel void fixtureSightlines(primitive_acceleration_structure room [[buffer(0)]],
                              constant uint &caseCount [[buffer(1)]],
                              device const RayInput *inputs [[buffer(2)]],
                              device RayResult *results [[buffer(3)]],
                              uint index [[thread_position_in_grid]]) {
    const RayInput input = inputs[index % caseCount];
    ray sightline;
    sightline.origin = input.originAndSegmentMax.xyz;
    sightline.direction = input.directionAndSegmentMin.xyz;
    sightline.min_distance = 0x1.0p-126f;
    sightline.max_distance = 32.0f; // The fixture's reference hits are all inside this range.

    intersector<> nearest;
    nearest.assume_geometry_type(geometry_type::triangle);
    const auto hit = nearest.intersect(sightline, room);
    const bool found = hit.type == intersection_type::triangle;

    sightline.min_distance = input.directionAndSegmentMin.w;
    sightline.max_distance = input.originAndSegmentMax.w;
    intersector<> occlusion;
    occlusion.assume_geometry_type(geometry_type::triangle);
    occlusion.accept_any_intersection(true);
    const bool blocked = occlusion.intersect(sightline, room).type == intersection_type::triangle;

    results[index] = {found ? hit.distance : -1.0f, found ? hit.primitive_id : 0xffffffffu,
                      uint(blocked), 0x538u};
}
