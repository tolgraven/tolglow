#!/bin/bash
set -euo pipefail

if (( $# != 2 )); then
  echo 'usage: verify-oracle.sh /path/to/tolglow /path/to/afterglow' >&2
  exit 1
fi

probe_dir=$(cd -- "$(dirname -- "$0")" && pwd)
source_dir=$(mktemp -d "${TMPDIR:-/tmp}/tolglow-oracle-538.XXXXXXXX")
trap 'rm -rf "$source_dir"' EXIT
mkdir -p "$source_dir/tolglow/rig" "$source_dir/afterglow/render"
git -C "$1" show c1fcdb9cffcf9329e736f29cd1cbe2efcce0cc8d:src/tolglow/rig/surfaces.clj \
  > "$source_dir/tolglow/rig/surfaces.clj"
git -C "$2" show e1eb9ff50985a9e3e304b54ec7a97db735956c5a:src/afterglow/render/optics.cljc \
  > "$source_dir/afterglow/render/optics.cljc"

clojure_classpath=${CLOJURE_CLASSPATH:-}
if [[ -z $clojure_classpath ]]; then
  clojure_classpath=$(cd "$source_dir" && clojure -Srepro -Sdeps \
    '{:deps {org.clojure/clojure {:mvn/version "1.12.7"}}}' -Spath)
fi
java -cp "$source_dir:$clojure_classpath" clojure.main "$probe_dir/oracle.clj" > "$source_dir/fixture.tsv"
diff -u "$probe_dir/fixture.tsv" "$source_dir/fixture.tsv"
echo 'status=cpu-oracle-fixture-matches'
