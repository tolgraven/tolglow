#include <metal_stdlib>
#include <metal_tensor>
#include <MetalPerformancePrimitives/MetalPerformancePrimitives.h>

using namespace metal;
using namespace mpp;
using namespace mpp::tensor_ops;

kernel void lightingAffine(tensor<device half, dextents<int, 2>> input [[buffer(0)]],
                           tensor<device half, dextents<int, 2>> weights [[buffer(1)]],
                           tensor<device half, dextents<int, 2>> output [[buffer(2)]],
                           uint2 group [[threadgroup_position_in_grid]])
{
    constexpr int tile = 64;
    constexpr int channels = 64;
    auto left = input.slice<channels, tile>(0, int(group.y) * tile);
    auto right = weights.slice<tile, channels>(int(group.x) * tile, 0);
    auto result = output.slice<tile, tile>(int(group.x) * tile, int(group.y) * tile);

    constexpr matmul2d_descriptor shape(tile, tile, channels);
    matmul2d<shape, execution_simdgroups<4>> multiply;
    auto product = multiply.get_destination_cooperative_tensor<decltype(left), decltype(right), half>();
    multiply.run(left, right, product);
    product.store(result);
}
