#version 300 es
precision highp float;

// A world-space aim and the last accepted normalized pan (-1 on the first frame).
layout(location = 0) in vec4 targetAndPrevious;
layout(location = 1) in vec3 mountPosition;
layout(location = 2) in vec3 mountCos;
layout(location = 3) in vec3 mountSin;
// Offset and scale convert degrees to calibrated unit values, for pan then tilt.
layout(location = 4) in vec4 axisCalibration;
out vec4 axes;

bool legal(float x) { return x >= 0.0 && x <= 1.0; }

void main() {
  // Inverse of the X, Y, Z mounting rotations. Sines and cosines of the negative
  // Euler angles are uploaded once per rig. Each invocation owns one fixture.
  vec3 d = targetAndPrevious.xyz - mountPosition;
  vec2 xy = vec2(mountCos.z*d.x - mountSin.z*d.y, mountSin.z*d.x + mountCos.z*d.y);
  vec2 xz = vec2(mountCos.y*xy.x + mountSin.y*d.z, mountCos.y*d.z - mountSin.y*xy.x);
  vec3 local = vec3(xz.x, mountCos.x*xy.y - mountSin.x*xz.y, mountSin.x*xy.y + mountCos.x*xz.y);
  float horizontal = length(local.xz);
  float yaw = local.z == 0.0 ? sign(local.x)*90.0 : degrees(atan(local.x, local.z));
  float elevation = horizontal == 0.0 ? sign(local.y)*90.0 : degrees(atan(local.y, horizontal));
  float prior = targetAndPrevious.w < 0.0 ? axisCalibration.x : targetAndPrevious.w;
  vec3 candidates = axisCalibration.x + axisCalibration.y * (yaw + vec3(0.0, -360.0, 360.0));
  float pan = candidates.x;
  for (int i = 1; i < 3; ++i) {
    float c = candidates[i];
    if ((legal(c) && !legal(pan)) ||
        (legal(c) == legal(pan) && abs(c-prior) < abs(pan-prior))) pan = c;
  }
  float tilt = axisCalibration.z + axisCalibration.w*elevation;
  axes = vec4(clamp(vec2(pan,tilt), 0.0, 1.0), float(legal(pan) && legal(tilt)), float(length(local) > 0.0));
  gl_Position = vec4(0.0, 0.0, 0.0, 1.0);
}
