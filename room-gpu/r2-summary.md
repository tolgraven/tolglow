Renderer: ANGLE (Apple, ANGLE Metal Renderer: Apple M4 Pro, Unspecified Version)

| Raster | Fixtures | Before: meshes, ms | After N=8 (default): meshes, ms | After N=16 | Before Low (own raster) | After Low (own raster, haze) | Before moving | After moving |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1920x1080 | 64 | 64, 15.8 | 64, 10.1 | 18.5 | 3.2 (566×318) | 2.7 (566×318) | 15.9 | 10.1 |
| 1920x1080 | 128 | 64, 17.0 | 128, 12.6 | 23.6 | 3.7 (566×318) | 2.7 (566×318) | 17.0 | 12.8 |
| 2560x1440 | 64 | 64, 21.3 | 64, 12.3 | 23.5 | 3.1 (566×318) | 2.6 (566×318) | 21.7 | 12.4 |
| 2560x1440 | 128 | 64, 23.2 | 128, 15.2 | 29.7 | 3.8 (566×318) | 2.9 (566×318) | 23.1 | 15.4 |

Attribution (full minus the run with the pass off, ms):

| Raster | Fixtures | Before fog | Before spots | Before shadows | After fog+beams | After spots | After shadows | After faked beams |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1920x1080 | 64 | 5.0 | 8.2 | 4.0 | 5.1 | 2.1 | 1.4 | 0.5 |
| 1920x1080 | 128 | 5.3 | 9.0 | 4.3 | 5.1 | 3.4 | 2.3 | 1.1 |
| 2560x1440 | 64 | 7.0 | 10.3 | 4.8 | 5.2 | 2.6 | 1.4 | 0.3 |
| 2560x1440 | 128 | 7.6 | 11.5 | 5.6 | 6.2 | 4.4 | 2.5 | 1.3 |

After inventory at 1920x1080 n=64: sceneTarget 1920x1080 RGBA16F MSAA4 +depthTex; compositeTarget 1920x1080 RGBA16F; composer.read 1920x1080 RGBA16F; composer.write 1920x1080 RGBA16F; bloom.bright 960x540 RGBA16F; bloom.h0 960x540 RGBA16F; bloom.h1 480x270 RGBA16F; bloom.h2 240x135 RGBA16F; bloom.h3 120x68 RGBA16F; bloom.h4 60x34 RGBA16F; bloom.v0 960x540 RGBA16F; bloom.v1 480x270 RGBA16F; bloom.v2 240x135 RGBA16F; bloom.v3 120x68 RGBA16F; bloom.v4 60x34 RGBA16F; fog.volume 960x540 RGBA16F; shadow0 1024x1024 RGBA8 +depthTex; shadow1 1024x1024 RGBA8 +depthTex; shadow2 1024x1024 RGBA8 +depthTex; shadow3 1024x1024 RGBA8 +depthTex

Before inventory: sceneTarget 1920x1080 RGBA16F MSAA4 +depthTex; compositeTarget 1920x1080 RGBA16F; composer.read 1920x1080 RGBA16F; composer.write 1920x1080 RGBA16F; bloom.bright 960x540 RGBA16F; bloom.h0 960x540 RGBA16F; bloom.h1 480x270 RGBA16F; bloom.h2 240x135 RGBA16F; bloom.h3 120x68 RGBA16F; bloom.h4 60x34 RGBA16F; bloom.v0 960x540 RGBA16F; bloom.v1 480x270 RGBA16F; bloom.v2 240x135 RGBA16F; bloom.v3 120x68 RGBA16F; bloom.v4 60x34 RGBA16F; fog.volume 960x540 RGBA16F; shadow0 1024x1024 RGBA8 +depthTex; shadow1 1024x1024 RGBA8 +depthTex; shadow2 1024x1024 RGBA8 +depthTex; shadow3 1024x1024 RGBA8 +depthTex
