use std::process::Command;

#[test]
fn test_snapshots() {
    glob!("testdata/*.lx", |path| {
        println!("testing {:?}", path);
        let output = Command::new("cargo")
            .arg("run")
            .arg(path)
            .output()
            .expect("failed to execute test");
        let output_str =
            String::from_utf8(output.stdout).expect("oops, output not utf8");
        assert_snapshot!(output_str);
    });
}
