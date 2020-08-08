use insta::{
    assert_snapshot,
    glob,
};

use std::io::Read;
use std::path::Path;
use std::process::{
    Command,
    Stdio,
};
use std::sync::Once;

static INIT_BUILD: Once = Once::new();

fn initialize() {
    INIT_BUILD.call_once(|| {
        assert!(Command::new("cargo")
            .arg("build")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .unwrap()
            .success());
    });
}

fn exec(path: &Path) -> Result<String, String> {
    let mut child = Command::new("./target/debug/lx")
        .arg(path)
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to spawn test");
    let exit_status = child.wait().expect("failed to run test");
    let mut output_str = String::new();
    child
        .stdout
        .expect("no stdout for test")
        .read_to_string(&mut output_str)
        .expect("failed to read stdout for test");
    if exit_status.success() {
        Ok(output_str)
    } else {
        Err(output_str)
    }
}

#[test]
fn test_scripts_success() {
    initialize();
    glob!("scripts_success/*.lx", |path| {
        println!("testing {:?}", path);
        match exec(path) {
            Ok(output) => {
                assert_snapshot!(output);
            }
            Err(output) => {
                panic!("unexpected error:\n{}", output);
            }
        }
    });
}

#[test]
fn test_scripts_error() {
    initialize();
    glob!("scripts_error/*.lx", |path| {
        println!("testing {:?}", path);
        match exec(path) {
            Ok(output) => {
                panic!("expected failure, but got success:\n{}", output);
            }
            Err(output) => {
                assert_snapshot!(output);
            }
        }
    });
}
