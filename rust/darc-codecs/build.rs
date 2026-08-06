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
    // The C ABI headers live with the Rust that consumes them. They used to be
    // Compression/Compression.h and Common.h; Compression/ was deleted once the
    // difftest harnesses started reading their C from a pin, and these two are
    // the only part cargo needs at BUILD time -- a build script cannot git-archive
    // without breaking offline and vendored builds. c-header-check.sh asserts they
    // are byte-identical to the pinned copies, so the two cannot drift.
    let compression = root.join("rust/include");

    println!("cargo:rerun-if-changed=wrapper.h");
    println!("cargo:rerun-if-changed={}", compression.join("Compression.h").display());
    println!("cargo:rerun-if-changed={}", compression.join("Common.h").display());

    // The same defines the C build uses. Compression/Common.h errors out
    // without an OS and a byte-order define, so these are mandatory, not
    // decoration -- see Utils.hs:31 for the Haskell-side equivalent.
    //
    // `cfg!` in a build script is the HOST, not the target, and that is
    // deliberate: Common.h:18's FREEARC_WIN branch includes <windows.h>, which
    // libclang cannot find when cross-compiling from a Linux runner. Every item
    // the allowlist below extracts is declared outside that branch -- MemSize is
    // `unsigned` at Common.h:93 and CALLBACK_FUNC is a plain function type at
    // Compression.h:62 -- so which branch is parsed does not reach the
    // generated bindings, and parsing the one that compiles is what lets the
    // Windows cross-builds run at all.
    let os_define = if cfg!(target_os = "windows") { "-DFREEARC_WIN" } else { "-DFREEARC_UNIX" };

    // bindgen passes `--target=$TARGET` to clang, and libclang older than the
    // llvm-mingw toolchain does not know the `gnullvm` environment: it reads
    // `aarch64-pc-windows-gnullvm` as environment `gnu` plus version `llvm` and
    // rejects it. The two differ in which linker and CRT the Rust side uses,
    // not in anything a header sees, so clang is given the `-gnu` spelling.
    // Last `--target` wins in the clang driver, so this overrides bindgen's.
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
