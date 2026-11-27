Renderer: ANGLE (Apple, ANGLE Metal Renderer: Apple M4 Pro, Unspecified Version)

| Raster | Fixtures | Before (16 spots) | After N=8 (default) | After N=16 | After N=0 (all faked) | After Low tier (haze) | Before moving | After moving |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 960x540 | 16 | 6.7 | 3.3 | 5.5 | 2.5 | 2.4 | 7.3 | 3.4 |
| 960x540 | 32 | 7.2 | 3.8 | 6.4 | 2.7 | 1.9 | 7.1 | 3.7 |
| 960x540 | 64 | 8.8 | 5.2 | 8.9 | 3.3 | 3.5 | 8.7 | 5.0 |
| 960x540 | 128 | 12.6 | 7.6 | 11.4 | 7.1 | 6.6 | 12.1 | 8.5 |
| 1440x900 | 16 | 11.8 | 5.4 | 9.6 | 2.8 | 2.8 | 12.5 | 5.9 |
| 1440x900 | 32 | 11.5 | 5.9 | 10.7 | 3.3 | 3.3 | 12.1 | 5.9 |
| 1440x900 | 64 | 14.4 | 7.4 | 13.7 | 4.2 | 3.7 | 15.2 | 7.8 |
| 1440x900 | 128 | 15.8 | 9.7 | 18.9 | 6.3 | 6.8 | 15.9 | 9.9 |
| 1920x1080 | 16 | 14.9 | 7.5 | 13.1 | 3.7 | 3.2 | 15.5 | 7.9 |
| 1920x1080 | 32 | 13.2 | 8.2 | 15.0 | 4.1 | 3.7 | 13.4 | 8.3 |
| 1920x1080 | 64 | 18.4 | 10.3 | 19.1 | 5.1 | 5.0 | 19.3 | 10.7 |
| 1920x1080 | 128 | 23.1 | 12.8 | 24.5 | 8.0 | 7.5 | 20.4 | 13.2 |
| 2560x1440 | 16 | 28.6 | 9.5 | 17.5 | 5.3 | 4.8 | 27.9 | 10.1 |
| 2560x1440 | 32 | 21.7 | 10.1 | 19.4 | 5.5 | 5.0 | 22.3 | 10.3 |
| 2560x1440 | 64 | 24.7 | 12.3 | 23.8 | 6.7 | 5.8 | 25.4 | 13.6 |
| 2560x1440 | 128 | 32.7 | 15.2 | 30.7 | 7.8 | 7.8 | 31.2 | 15.8 |
| 3024x1964 | 16 | 28.7 | 14.3 | 26.8 | 7.5 | 7.8 | 35.9 | 15.3 |
| 3024x1964 | 32 | 33.8 | 15.3 | 30.0 | 8.1 | 8.3 | 28.5 | 15.0 |
| 3024x1964 | 64 | 32.2 | 17.6 | 34.5 | 8.7 | 8.7 | 35.6 | 18.1 |
| 3024x1964 | 128 | 37.6 | 16.6 | 33.3 | 7.8 | 7.8 | 37.1 | 16.5 |

Attribution (full minus the run with the pass off, ms):

| Raster | Fixtures | Before fog | Before spots | Before shadows | After fog+beams | After spots | After shadows | After faked beams |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 960x540 | 16 | 2.6 | 2.6 | 1.4 | 0.7 | 0.6 | 0.4 | 0.1 |
| 960x540 | 32 | 3.0 | 3.0 | 1.7 | 1.1 | 0.6 | 0.5 | 0.2 |
| 960x540 | 64 | 2.5 | 4.5 | 2.9 | 1.6 | 1.0 | 0.8 | 0.4 |
| 960x540 | 128 | 3.2 | 5.7 | 3.8 | 0.4 | 1.2 | 1.0 | 0.4 |
| 1440x900 | 16 | 5.6 | 4.4 | 2.7 | 2.5 | 0.8 | 0.8 | -0.2 |
| 1440x900 | 32 | 3.5 | 5.0 | 2.5 | 2.5 | 1.0 | 0.8 | 0.1 |
| 1440x900 | 64 | 4.3 | 7.7 | 4.0 | 2.7 | 1.7 | 1.2 | -0.1 |
| 1440x900 | 128 | 3.1 | 8.5 | 3.1 | 2.8 | 2.7 | 1.9 | 1.1 |
| 1920x1080 | 16 | 6.6 | 6.5 | 3.6 | 4.0 | 1.4 | 1.1 | 0.3 |
| 1920x1080 | 32 | 4.5 | 6.0 | 2.7 | 4.0 | 1.6 | 1.2 | 0.6 |
| 1920x1080 | 64 | 4.6 | 9.4 | 4.7 | 5.0 | 2.1 | 1.5 | 0.6 |
| 1920x1080 | 128 | 8.0 | 12.2 | 8.0 | 4.5 | 3.3 | 2.2 | 1.1 |
| 2560x1440 | 16 | 14.1 | 11.3 | 7.1 | 4.0 | 1.8 | 1.1 | 0.4 |
| 2560x1440 | 32 | 7.8 | 8.6 | 3.5 | 4.3 | 2.1 | 1.4 | 0.6 |
| 2560x1440 | 64 | 7.7 | 11.3 | 4.2 | 5.3 | 2.7 | 1.6 | 0.5 |
| 2560x1440 | 128 | 11.3 | 18.0 | 9.4 | 6.2 | 4.0 | 2.5 | 1.3 |
| 3024x1964 | 16 | 4.1 | 0.9 | -10.4 | 5.5 | 2.7 | 1.6 | -0.1 |
| 3024x1964 | 32 | 17.7 | 16.7 | 9.6 | 5.9 | 3.7 | 2.0 | 1.3 |
| 3024x1964 | 64 | 12.8 | 14.5 | 6.5 | 7.6 | 4.1 | 2.4 | 0.6 |
| 3024x1964 | 128 | 14.1 | 18.3 | 7.0 | 7.1 | 5.2 | 3.1 | 1.7 |

After inventory at 960x540 n=16: sceneTarget 960x540 RGBA16F MSAA4 +depthTex; compositeTarget 960x540 RGBA16F; composer.read 960x540 RGBA16F; composer.write 960x540 RGBA16F; bloom.bright 480x270 RGBA16F; bloom.h0 480x270 RGBA16F; bloom.h1 240x135 RGBA16F; bloom.h2 120x68 RGBA16F; bloom.h3 60x34 RGBA16F; bloom.h4 30x17 RGBA16F; bloom.v0 480x270 RGBA16F; bloom.v1 240x135 RGBA16F; bloom.v2 120x68 RGBA16F; bloom.v3 60x34 RGBA16F; bloom.v4 30x17 RGBA16F; fog.volume 480x270 RGBA16F; shadow0 1024x1024 RGBA8 +depthTex; shadow1 1024x1024 RGBA8 +depthTex; shadow2 1024x1024 RGBA8 +depthTex; shadow3 1024x1024 RGBA8 +depthTex

Before inventory: sceneTarget 960x540 RGBA16F MSAA4 +depthTex; compositeTarget 960x540 RGBA16F; composer.read 960x540 RGBA16F; composer.write 960x540 RGBA16F; bloom.bright 480x270 RGBA16F; bloom.h0 480x270 RGBA16F; bloom.h1 240x135 RGBA16F; bloom.h2 120x68 RGBA16F; bloom.h3 60x34 RGBA16F; bloom.h4 30x17 RGBA16F; bloom.v0 480x270 RGBA16F; bloom.v1 240x135 RGBA16F; bloom.v2 120x68 RGBA16F; bloom.v3 60x34 RGBA16F; bloom.v4 30x17 RGBA16F; fog.volume 480x270 RGBA16F; shadow0 1024x1024 RGBA8 +depthTex; shadow1 1024x1024 RGBA8 +depthTex; shadow2 1024x1024 RGBA8 +depthTex; shadow3 1024x1024 RGBA8 +depthTex
