# Mac Studio packaging verification

The still and short GIF are from the packaged Apple Silicon Studio app at `ea993d3c1ec04b193e3c8e8c725c1672015ede4a`, captured by [Packaged Apple Studio](https://github.com/tolgraven/tolglow/actions/runs/45290490338). They show the real packaged window running the disconnected Ember, Breathe, and Orchid cues.

The archive’s 383 sealed resource records matched on extraction. Strict code-sign verification failed because the bundle had four dangling links to the excluded GraalVM Native Image development tools. An isolated copy omitted those four links, retained the original Studio JAR and helper, and passed an ad hoc re-sign followed by `codesign --verify --deep --strict`. The project’s packaged Studio check passed with the bundle’s Java, including user namespace load, serving the release client, and backup restore. The screenshot precedes the packaging change and illustrates the unchanged application UI.
