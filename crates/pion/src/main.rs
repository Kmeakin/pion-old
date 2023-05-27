#![warn(clippy::all, clippy::nursery, unused_qualifications)]

use std::sync::Arc;

use clap::Parser;
use pion_core::db::CoreDatabase;
use pion_source::db::SourceDatabase;
use pion_source::input::InputString;

mod driver;

#[derive(Parser)]
#[clap(author, version, about)]
enum Opts {
    Check { files: Vec<String> },
}

fn read_file(file: &String) -> Result<InputString, String> {
    let input = match std::fs::read_to_string(file) {
        Ok(input) => input,
        Err(err) => return Err(format!("Cannot read `{file}`: {err}")),
    };

    match pion_source::input::InputString::new(input) {
        Ok(input) => Ok(input),
        Err(err) => Err(err.to_string()),
    }
}

fn unwrap_or_exit<T, E: std::fmt::Display>(value: Result<T, E>) -> T {
    match value {
        Ok(value) => value,
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1)
        }
    }
}

fn main() {
    match Opts::parse() {
        Opts::Check { mut files } => {
            files.sort_unstable();
            let files: Arc<[_]> = Arc::from(files);

            let mut db = CliDatabase::default();

            for (file_id, file) in files.iter().enumerate() {
                let text = std::fs::read_to_string(file).unwrap_or_default();
                let text = InputString::new(text).unwrap_or_default();
                db.set_file_text(file_id, Arc::from(text));
            }

            db.set_files(files.clone());

            for (file, _) in files.iter().enumerate() {
                for (_name, def) in db.file_items(file).iter() {
                    let (def, messages) = db.def_core(file, *def);
                    dbg!(def);
                    dbg!(messages);
                }
            }
        }
    };
}

#[salsa::database(
    pion_source::db::SourceDatabaseStorage,
    pion_surface::db::SurfaceDatabaseStorage,
    pion_core::db::CoreDatabaseStorage
)]
#[derive(Default)]
pub struct CliDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for CliDatabase {}

impl salsa::ParallelDatabase for CliDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
            storage: self.storage.snapshot(),
        })
    }
}
