# Third-party NOTICE guidance

This project depends on many crates. Some dependencies are licensed under Apache-2.0 (or Apache-2.0 OR MIT). When redistributing compiled binaries, Apache-2.0 may require you to include `NOTICE` files (or preserve attribution) from those dependencies.

The lists below were generated from `cargo license --avoid-dev-deps` output included in `THIRD-PARTY-LICENSES.md`.

## Direct Apache-2.0 dependencies (exact)
ab_glyph, ab_glyph_rasterizer, accesskit_winit, codespan-reporting, gethostname, gl_generator, glutin, glutin_egl_sys, glutin_glx_sys, glutin_wgl_sys, khronos_api, owned_ttf_parser, rpassword, rtoolbox, spirv, winit

## Large set of Apache-2.0 OR MIT (and similar) dependencies
There are many dependencies that are Apache-2.0 OR MIT (or variants). See `THIRD-PARTY-LICENSES.md` for the full machine-generated list. Examples include: accesskit, egui, eframe, serde, clap, wgpu, and many others.

## What you should do before redistributing binaries
- If you publish only the crate to crates.io (source), including `THIRD-PARTY-LICENSES.md` is usually sufficient. For some Apache-2.0 dependencies you may need to include their NOTICE text.
- If you redistribute compiled binaries (releases), collect NOTICE files from dependencies that use Apache-2.0 and append or bundle them with your release artifacts.
- To collect NOTICE files automatically: fetch each dependency's repository (from crates.io metadata) and look for a `NOTICE`, `NOTICE.txt`, or `LICENSE` file with additional attribution. I can help automate this if you want.

If you'd like I can try to fetch NOTICE files for the Apache-2.0 dependencies and append them here (this requires more network access and some heuristics). Say "Collect NOTICE files" to continue and I'll attempt it.
