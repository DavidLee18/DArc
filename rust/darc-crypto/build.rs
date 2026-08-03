// Generate the C ABI declarations from DArc's own headers.
//
// These types were hand-transcribed at first. That works right up until a
// header changes and nobody re-transcribes -- which is exactly how this
// codebase ended up with 41 helpers whose declarations disagreed with their
// definitions, 8 of them truncating a `long` return to `int` at every call
// site. Generating them means the compiler reads the same header the C side
// compiles against, so the two cannot silently drift apart.
use std::{env, path::PathBuf};

fn main() {
    let root = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .join("../..")
        .canonicalize()
        .expect("repo root");
    let compression = root.join("Compression");

    println!("cargo:rerun-if-changed=wrapper.h");
    println!("cargo:rerun-if-changed={}", compression.join("Compression.h").display());
    println!("cargo:rerun-if-changed={}", compression.join("Common.h").display());

    // The same defines the C build uses. Compression/Common.h errors out
    // without an OS and a byte-order define, so these are mandatory, not
    // decoration -- see Utils.hs:31 for the Haskell-side equivalent.
    // `cfg!` is the HOST here, not the target, and that is deliberate --
    // see darc-codecs/build.rs for why switching it breaks the cross-builds
    // and why it cannot reach the generated bindings.
    let os_define = if cfg!(target_os = "windows") { "-DFREEARC_WIN" } else { "-DFREEARC_UNIX" };

    // libclang does not know the `gnullvm` environment and rejects the whole
    // triple; the `-gnu` spelling describes the same thing to a C header.
    // Same fix as darc-codecs/build.rs, which has the long version.
    let clang_target = env::var("TARGET").unwrap_or_default().replace("-gnullvm", "-gnu");

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .clang_args([
            os_define,
            "-DFREEARC_INTEL_BYTE_ORDER",
            "-DFREEARC_64BIT",
            "-x",
            "c++",
            "-std=c++17",
        ])
        .clang_arg(format!("-I{}", compression.display()))
        .clang_arg(format!("-I{}", root.display()))
        .clang_arg(format!("--target={clang_target}"))
        // Only what the codec boundary actually needs. Without an allowlist
        // bindgen emits thousands of items from the C++ headers and the
        // signal is lost.
        .allowlist_type("CALLBACK_FUNC")
        .allowlist_type("MemSize")
        .allowlist_var("FREEARC_OK")
        .allowlist_var("FREEARC_ERRCODE_.*")
        .layout_tests(false)
        .generate()
        .expect("bindgen failed to generate DArc ABI bindings");

    bindings
        .write_to_file(PathBuf::from(env::var("OUT_DIR").unwrap()).join("darc_abi.rs"))
        .expect("failed to write bindings");
}
