use std::fmt;
use std::sync::Arc;

use ouroboros::self_referencing;
use pion_source::FileId;

use crate::reporting::Message;
use crate::syntax;

#[salsa::query_group(SurfaceDatabaseStorage)]
pub trait SurfaceDatabase: salsa::Database + pion_source::db::SourceDatabase {
    fn file_module<'arena>(&self, file: FileId) -> (Arc<ArenaAndModule>, Arc<[Message]>);
}

fn file_module(db: &dyn SurfaceDatabase, file: FileId) -> (Arc<ArenaAndModule>, Arc<[Message]>) {
    let input = db.file_text(file);

    let mut messages = Vec::new();
    let mut on_message = |message| messages.push(message);

    let arena = bumpalo::Bump::new();
    let arena_and_module = ArenaAndModuleBuilder {
        arena,
        module_builder: |arena| syntax::Module::parse(arena, &mut on_message, &input),
    }
    .build();
    (Arc::new(arena_and_module), Arc::from(messages))
}

#[self_referencing]
pub struct ArenaAndModule {
    arena: bumpalo::Bump,
    #[borrows(arena)]
    #[covariant]
    pub module: syntax::Module<'this>,
}

impl PartialEq for ArenaAndModule {
    fn eq(&self, other: &Self) -> bool { self.borrow_module() == other.borrow_module() }
}

impl Eq for ArenaAndModule {}

impl fmt::Debug for ArenaAndModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.borrow_module(), f)
    }
}

// TODO: is this safe?
unsafe impl Sync for ArenaAndModule {}
