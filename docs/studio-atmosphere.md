# Fog machine and venue controls

In **Rig → Fixture library**, create or edit a type and give its output channel the
attribute `:fog`. Under **Fog machine**, enable **Model fog output** and enter
the rated flow in cubic metres per minute. The off and full fields describe the
coarse DMX byte at each end of the output range. A fixture whose output increases
as its DMX value falls can use 255 for off and 0 for full.

Set the plume mixing time in seconds and the nozzle direction in world-space
X, Y, Z. Y points up. For example, `[1 0 0]` points along positive X. The
renderer normalizes direction. The default nozzle is `[0 0 1]`, mixing time
is 8 seconds and DMX endpoints are 0 and 255. **Validate & publish type** saves
the profile with its fixture definition. **Add fixture to rig** patches an
instance. Removing its fog channel removes the profile.

Under **Rig → Room & fixtures → Room atmosphere**, enter the venue’s air changes
per hour and the fog half-life in seconds, then **Save rig**. Leave a field
empty to use the model’s default of zero air changes per hour or a 180-second
half-life. Room dimensions remain in metres. The model combines the natural
decay and the air exchange, and derives emission from delivered fog DMX.
