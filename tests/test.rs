use assert_cmd::prelude::*;
use std::process::Command;

fn assert_uroman_output(input_path: &str, expected_output: &str) {
    let expected_output_normalized = expected_output.replace("\r\n", "\n");

    let cmd = Command::new("uroman-rs")
        .arg("-i")
        .arg(input_path)
        .unwrap();
    let output = cmd
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();

    let actual_output_normalized = String::from_utf8(output).unwrap().replace("\r\n", "\n");

    assert_eq!(actual_output_normalized, expected_output_normalized);
}

#[test]
fn test_multi_script_romanization() {
    assert_uroman_output(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/test/multi-script.txt"),
        include_str!("test/multi-script.uroman-ref.txt"),
    );
}
