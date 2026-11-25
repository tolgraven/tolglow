#pragma once

#include <math.h>

typedef struct {
    float rgb[3];
    float shaft;
} SceneSample;

static inline float clamp01(float value)
{
    return fminf(fmaxf(value, 0.0f), 1.0f);
}

static inline float smoothEdge(float start, float end, float value)
{
    float fraction = clamp01((value - start) / (end - start));
    return fraction * fraction * (3.0f - 2.0f * fraction);
}

static inline SceneSample roomSample(unsigned x, unsigned y, unsigned width,
                                     unsigned height, unsigned phase, int hdr)
{
    const float beamColors[4][3] = {
        {1.0f, 0.19f, 0.03f}, {0.10f, 0.43f, 1.0f},
        {0.33f, 1.0f, 0.42f}, {1.0f, 0.35f, 0.69f}
    };
    float u = ((float)x + 0.5f) / (float)width;
    float v = ((float)y + 0.5f) / (float)height;
    float horizontalPixel = 1.0f / (float)width;
    float verticalPixel = 1.0f / (float)height;
    float movement = (float)phase;
    float hazeU = u - (0.46f + 0.024f * movement);
    float hazeV = v - 0.38f;
    float fog = expf(-8.0f * hazeU * hazeU - 4.0f * hazeV * hazeV);
    float floor = smoothEdge(0.76f - verticalPixel, 0.76f + verticalPixel, v);
    SceneSample sample = {
        .rgb = {0.008f + 0.042f * fog + 0.016f * floor,
                0.011f + 0.053f * fog + 0.013f * floor,
                0.021f + 0.083f * fog + 0.012f * floor},
        .shaft = 0.0f
    };

    for (unsigned beam = 0; beam < 8; ++beam) {
        float index = (float)beam;
        float start = 0.14f + 0.10f * index;
        float slope = (3.5f - index) * 0.13f
                    + movement * (beam % 2 ? 0.023f : -0.019f);
        float heightAboveEmitter = v - 0.10f - 0.011f * (float)(beam % 3);
        float distance = fabsf(u - start - slope * heightAboveEmitter);
        float halfWidth = 0.00043f + 0.00037f * (float)(beam % 4)
                        + 0.00043f * fmaxf(heightAboveEmitter, 0.0f);
        float footprint = 0.5f * (horizontalPixel + fabsf(slope) * verticalPixel);
        float core = 1.0f - smoothEdge(fmaxf(halfWidth - footprint, 0.0f),
                                       halfWidth + footprint, distance);
        float halo = 0.22f * expf(-distance * distance / 0.000045f);
        float length = smoothEdge(-verticalPixel, 0.055f, heightAboveEmitter)
                     * (1.0f - smoothEdge(0.58f, 0.74f, heightAboveEmitter));
        float energy = length * (0.57f * core + halo);
        sample.shaft = fmaxf(sample.shaft, core * length);
        for (unsigned channel = 0; channel < 3; ++channel) {
            sample.rgb[channel] += energy * beamColors[beam % 4][channel];
        }
    }

    for (unsigned channel = 0; channel < 3; ++channel) {
        float linear = sample.rgb[channel];
        sample.rgb[channel] = hdr ? linear * 5.0f : clamp01(linear);
    }
    return sample;
}
