# Bounding the displacement of a moving spotlight

The paired beam replay now reports which keyframe strides a geometric error
bound admits *before seeing the output*. This is an offline experiment for
one linear-interpolated aim program, a fixed lens and a static tempo. The
release scheduler still uses its existing pressure policy.

The test rig has 64 moving heads over an unbounded horizontal floor. At a
maximum allowed error of 0.25 reference spot radii, the observations below
come from the fractional-255 encoder. The full-word encoder selected the same
strides. The reported error is the largest displacement in either bar.

| Target depth | Full beam angle | Admitted stride | Worst measured displacement at that stride | Predicted upper bound |
| --- | --- | --- | --- | --- |
| 0.7 m | 5° | 1 | 0 | 0 |
| 0.7 m | 40° | 2 | 0.055 | 0.115 |
| 6 m | 5° | 2 | 0.068 | 0.072 |
| 6 m | 40° | 8 | 0.138 | 0.153 |

The 40° near beam was safe at stride four in the recorded orbit. The bound
admits only two because it covers all combinations of the two oscillators'
phases. On the fast four-frame sine orbit the bound rejected stride four for
every group. The narrow near beam's 40 midpoint checks had all missed an
actual excursion above ten spot radii. [Output replay](multirate-quality.md)

## From cue expression to a floor error

1. Inspect the existing scalar expression tree once for each fixture. Numbers
   and static fixture positions have zero time derivatives. Beat advances at
   one beat per beat. Addition and subtraction sum absolute derivative bounds.
   Multiplication by a constant scales them. For a sine or cosine of phase
   $p$, the speed bound is $|p'|$ and the second-derivative bound is
   $|p'|^2 + |p''|$. This also handles nested oscillators. Reading a dynamic
   parameter, multiplying two varying values, or varying the target height
   yields no certificate.
2. Let $M$ bound the horizontal target acceleration per beat squared. Over a
   keyframe interval of $T$ beats, linear interpolation displaces the target by
   at most $MT^2/8$. This follows from the scalar linear-interpolation
   remainder in each coordinate and a Euclidean norm over the two bounds.
   Dividing by the fixed vertical target depth gives a bound on the error of
   the horizontal aim slope.
3. A small change in slope can move the centre of an oblique spot more than
   the central ray's intersection suggests. Let $v$ be the horizontal slope,
   $r=\|v\|$, $q=\sqrt{1+r^2}$, and $s=\sin(\alpha)$ for beam half-angle
   $\alpha$. A fixture at height $h$ with lens radius $a$ has a virtual
   apex offset $L=a/\tan(\alpha)$. Afterglow's cone-plane ellipse has
   horizontal centre offset $v F(r)$, where
   $F(r)=(h + Ls^2q)/(1-s^2q^2)$. The maximum local planar sensitivity
   is $F(r)+rF'(r)$. Both the radial and tangential derivatives increase
   up to the cone's grazing angle, so evaluate at the largest possible
   slope. The ellipse's minor radius is at least $h\tan(\alpha)+a$.
4. Include pan and tilt rounding for both the full and predicted command.
   Under the reference rig's default 360° pan and 180° tilt, one direction
   can rotate by at most $\epsilon=(360°+180°)/(2\,\mathrm{maxCode})$.
   Use radians for the calculation. Enlarge the permitted inclination by
   $\epsilon$. At the enlarged slope $r$, the angular-to-slope derivative
   is at most $1+r^2$. Add $2\epsilon(1+r^2)$ to the interpolation error,
   then multiply by the floor sensitivity and divide by the minimum minor
   radius. A stride of one uses the exact reference frame.

A grazing beam can produce an unbounded ellipse. That geometry, a lens
intersecting the floor, variable zoom, a non-default mount or calibration,
and an unsupported expression all leave the effect at full rate in this
experiment. Mechanical slew, fixture latency and finite receiver edges
remain outside the model.

Run the existing `tolglow.perf.beam-fidelity` replay and inspect
`:curvature-admission`. It checks the calculated upper bound against the
actual worst decoded frame at strides 1, 2, 4 and 8 and against the separate
fast-orbit counterexample. [Run instructions and paired calibration](multirate-quality.md)
