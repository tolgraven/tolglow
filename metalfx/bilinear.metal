#include <metal_stdlib>
using namespace metal;

struct RasterVertex {
    float4 position [[position]];
    float2 uv;
};

vertex RasterVertex fullscreen(uint vertexID [[vertex_id]])
{
    float2 uv = float2((vertexID << 1) & 2u, vertexID & 2u);
    return {float4(2.0f * uv.x - 1.0f, 1.0f - 2.0f * uv.y, 0.0f, 1.0f), uv};
}

fragment float4 bilinear(RasterVertex input [[stage_in]],
                         texture2d<float> color [[texture(0)]])
{
    constexpr sampler linearSampler(coord::normalized, address::clamp_to_edge,
                                    filter::linear);
    return color.sample(linearSampler, input.uv);
}
