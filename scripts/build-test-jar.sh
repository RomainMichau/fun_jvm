#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd "$script_dir/.." && pwd)"

src_dir="$project_root/src/main/java"
build_dir="$project_root/target/test-classes"
jar_path="$project_root/target/jvmarch-test.jar"

rm -rf "$build_dir"
mkdir -p "$build_dir"

mapfile -t java_files < <(find "$src_dir" -name '*.java')
if [ "${#java_files[@]}" -eq 0 ]; then
  echo "No .java files found under $src_dir" >&2
  exit 1
fi

javac -d "$build_dir" "${java_files[@]}"

jar --create --file "$jar_path" -C "$build_dir" .

echo "Built $jar_path"
