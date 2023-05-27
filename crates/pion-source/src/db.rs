use std::sync::Arc;

use crate::input::InputString;
use crate::FileId;

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: salsa::Database {
    #[salsa::input]
    fn files(&self) -> Arc<[String]>;

    #[salsa::input]
    fn file_text(&self, file_id: FileId) -> Arc<InputString>;
}
