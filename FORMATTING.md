# Source formatting

Run `python3 scripts/format-code.py` before committing. Use `--check` for a read-only check.
It formats first-party Scala, Java, GLSL, Python and shell sources, including tests;
it preserves the native layout of the short Groovy example and Windows launchers.
Worlds, downloaded dependencies, resource packs and binary assets are excluded.

Required tools on `PATH`:

- Scala CLI, using Scalafmt 3.11.5 pinned in `.scalafmt.conf`.
- clang-format (validated with 23.1.3), using `.clang-format` for Java and GLSL.
- Black (validated with 26.10.1), with a 100-column limit.
- shfmt (validated with 3.14.1), with two-space shell indentation.

`.editorconfig` defines shared whitespace and Windows launcher line endings.
After changing a Vulkan compute shader, rebuild its shipped SPIR-V with
`glslangValidator -V --target-env vulkan1.2 assets/shaders/rt/trace.comp -o assets/shaders/rt/trace.spv`.
Run `scripts/validate-strict.sh` for compilation, gameplay and multiplayer checks;
set `BLOCKBOX_RUN_ENGINE_RENDER_TESTS=1` to include real engine rendering.
