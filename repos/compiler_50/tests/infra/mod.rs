use std::{
    path::{Path, PathBuf},
    process::Command,
};

pub(crate) enum TestKind {
    Success,
    RuntimeError,
    StaticError,
}

#[macro_export]
macro_rules! success_tests {
    ($($tt:tt)*) => { $crate::tests!(Success => $($tt)*); }
}

#[macro_export]
macro_rules! runtime_error_tests {
    ($($tt:tt)*) => { $crate::tests!(RuntimeError => $($tt)*); }
}

#[macro_export]
macro_rules! static_error_tests {
    ($($tt:tt)*) => { $crate::tests!(StaticError => $($tt)*); }
}

#[macro_export]
macro_rules! tests {
    ($kind:ident =>
        $(
            {
                name: $name:ident,
                file: $file:literal,
                $(input: $input:literal,)?
                expected: $expected:literal $(,)?
                $(" $(tt:$tt)* ")?
            }
        ),*
        $(,)?
    ) => {
        $(
            #[test]
            fn $name() {
                #[allow(unused_assignments, unused_mut)]
                let mut input = None;
                $(input = Some($input);)?
                let kind = $crate::infra::TestKind::$kind;
                $crate::infra::run_test($file, input, $expected, kind);
            }
        )*
    };
}

pub(crate) fn run_test(name: &str, input: Option<&str>, expected: &str, kind: TestKind) {
    match kind {
        TestKind::Success => run_success_test(name, expected, input),
        TestKind::RuntimeError => run_runtime_error_test(name, expected, input),
        TestKind::StaticError => run_static_error_test(name, expected),
    }
}

fn run_success_test(name: &str, expected: &str, input: Option<&str>) {
    if let Err(err) = compile(name) {
        panic!("expected a successful compilation, but got an error: `{err}`");
    }
    match run(name, input) {
        Err(err) => {
            panic!("expected a successful execution, but got an error: `{err}`");
        }
        Ok(actual_output) => {
            diff(expected, actual_output);
        }
    }
}

fn run_runtime_error_test(name: &str, expected: &str, input: Option<&str>) {
    if let Err(err) = compile(name) {
        panic!("expected a successful compilation, but got an error: `{err}`");
    }
    match run(name, input) {
        Ok(out) => {
            panic!("expected a runtime error, but program executed succesfully: `{out}`");
        }
        Err(err) => check_error_msg(&err, expected),
    }
}

fn run_static_error_test(name: &str, expected: &str) {
    match compile(name) {
        Ok(()) => panic!("expected a failure, but compilation succeeded"),
        Err(err) => check_error_msg(&err, expected),
    }
}

fn compile(name: &str) -> Result<(), String> {
    // Run the compiler
    let compiler: PathBuf = ["target", "debug", env!("CARGO_PKG_NAME")].iter().collect();
    let output = Command::new(&compiler)
        .arg(&mk_path(name, Ext::Snek))
        .arg(&mk_path(name, Ext::Asm))
        .output()
        .expect("could not run the compiler");
    if !output.status.success() {
        return Err(String::from_utf8(output.stderr).unwrap());
    }

    // Assemble and link
    let output = Command::new("make")
        .arg(&mk_path(name, Ext::Run))
        .output()
        .expect("could not run make");
    assert!(output.status.success(), "linking failed");

    Ok(())
}

fn run(name: &str, input: Option<&str>) -> Result<String, String> {
    let mut cmd = Command::new(&mk_path(name, Ext::Run));
    if let Some(input) = input {
        cmd.arg(input);
    }
    let output = cmd.output().unwrap();
    if output.status.success() {
        Ok(String::from_utf8(output.stdout).unwrap().trim().to_string())
    } else {
        Err(String::from_utf8(output.stderr).unwrap().trim().to_string())
    }
}

fn check_error_msg(found: &str, expected: &str) {
    assert!(
        found.contains(expected.trim()),
        "the reported error message does not match",
    );
}

fn diff(expected: &str, actual_output: String) {
    let expected_output = expected.trim();
    if expected_output != actual_output {
        eprintln!(
            "output differed!\n{}",
            prettydiff::diff_lines(&actual_output, expected_output)
        );
        panic!("test failed");
    }
}

fn mk_path(name: &str, ext: Ext) -> PathBuf {
    Path::new("tests").join(format!("{name}.{ext}"))
}

#[derive(Copy, Clone)]
enum Ext {
    Snek,
    Asm,
    Run,
}

impl std::fmt::Display for Ext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ext::Snek => write!(f, "snek"),
            Ext::Asm => write!(f, "s"),
            Ext::Run => write!(f, "run"),
        }
    }
}
