use std::io::Read;
use std::process::{
    Command,
    Stdio,
};

#[test]
fn test_snapshots() {
    glob!("testdata/*.lx", |path| {
        println!("testing {:?}", path);
        let mut child = Command::new("cargo")
            .arg("run")
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
            .expect("can't read test output");

        if path.to_str().unwrap().ends_with("_error.lx") {
            if exit_status.success() {
                panic!(
                    "expected failure, got success: {:?}\n{}",
                    path, output_str
                );
            }
        } else {
            if !exit_status.success() {
                panic!(
                    "expected success, got failure: {:?}\n{}",
                    path, output_str
                );
            }
        }
        assert_snapshot!(output_str);
    });
}
