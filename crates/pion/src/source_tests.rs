use std::env;
use std::path::PathBuf;

use serde::Deserialize;
use walkdir::WalkDir;

const CONFIG_COMMENT_START: &str = "//~";

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Config {
    run: String,
    #[serde(default = "FALSE")]
    ignore: bool,
}

const FALSE: fn() -> bool = || false;

fn main() {
    let args = libtest_mimic::Arguments::from_args();
    let workspace_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_dir = workspace_dir.parent().unwrap().parent().unwrap();
    std::env::set_current_dir(workspace_dir).unwrap();
    let workspace_dir = workspace_dir.display().to_string();

    let tests = WalkDir::new("tests")
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.file_type().is_file())
        .filter(|entry| matches!(entry.path().extension(), Some(ext) if ext == "pion"))
        .map(|entry| entry.into_path())
        .map(|path| {
            let test_name = path.display().to_string();
            let input_source = std::fs::read_to_string(&path).unwrap();
            let absolute_path = format!("{workspace_dir}/{test_name}");

            // Collect the lines with CONFIG_COMMENT_START prefix, stripping the prefix in
            // the process
            let config_source: String = input_source
                .lines()
                .filter_map(|line| line.split(CONFIG_COMMENT_START).nth(1))
                .map(|s| format!("{s}\n")) // TODO: replace with `intersperse` when it is stabilized
                .collect();

            // Parse those lines as TOML
            let config = match basic_toml::from_str::<Config>(&config_source) {
                Ok(config) => config,
                Err(error) => {
                    return libtest_mimic::Trial::test(test_name, move || {
                        Err(error.to_string().into())
                    })
                }
            };

            if config.ignore {
                return libtest_mimic::Trial::test(test_name, || Ok(())).with_ignored_flag(true);
            }

            libtest_mimic::Trial::test(test_name.clone(), move || {
                let mut command = std::process::Command::new("sh");
                command.env_clear();
                command.env("PION", env!("CARGO_BIN_EXE_pion"));
                command.env("FILE", test_name);
                command.args(["-c", "-u", &config.run]);

                let output = match command.output() {
                    Err(err) => {
                        return Err(libtest_mimic::Failed::from(format!(
                            "Could not run command: `{:?}`\n{:?}",
                            command, err
                        )))
                    }
                    Ok(output) => output,
                };

                let status = output.status;
                let stdout = String::from_utf8(output.stdout).unwrap();
                let stderr = String::from_utf8(output.stderr).unwrap();

                let result = format!("{status}\nstdout:\n{stdout}\nstderr:\n{stderr}");
                let snapshot_path = format!("{absolute_path}.snap");

                let expect_result = std::panic::catch_unwind(|| {
                    expect_test::expect_file![snapshot_path].assert_eq(&result)
                });

                match expect_result {
                    Ok(_) => Ok(()),
                    Err(_) => Err(libtest_mimic::Failed::without_message()),
                }
            })
        })
        .collect();

    libtest_mimic::run(&args, tests).exit()
}
