use std::fmt;
use std::sync::Arc;

use ouroboros::self_referencing;
use pion_source::FileId;
use pion_surface::syntax::Symbol;

use crate::reporting::Message;
use crate::{elab, syntax};

pub type DefId = usize;

#[salsa::query_group(CoreDatabaseStorage)]
pub trait CoreDatabase: salsa::Database + pion_surface::db::SurfaceDatabase {
    fn file_items(&self, file: FileId) -> Arc<[(Symbol, DefId)]>;

    fn def_core(&self, file: FileId, def: DefId) -> (Arc<ArenaAndDef>, Arc<[Message]>);
}

fn file_items(db: &dyn CoreDatabase, file: FileId) -> Arc<[(Symbol, DefId)]> {
    let (surface_module, _) = db.file_module(file);
    let module: &pion_surface::syntax::Module = surface_module.borrow_module();

    module
        .items
        .iter()
        .enumerate()
        .map(|(id, item)| match item {
            pion_surface::syntax::Item::Def(def) => (def.name, id),
        })
        .collect()
}

fn def_core(db: &dyn CoreDatabase, file: FileId, def: DefId) -> (Arc<ArenaAndDef>, Arc<[Message]>) {
    let (surface_module, _) = db.file_module(file);
    let pion_surface::syntax::Item::Def(surface_def) = surface_module.borrow_module().items[def];

    let arena = bumpalo::Bump::new();
    let mut messages = Vec::new();
    let mut on_message = |message| messages.push(message);
    let arena_and_def = ArenaAndDef::new(arena, |arena| {
        elab::elab_def(db, arena, &mut on_message, file, surface_def)
    });

    (Arc::new(arena_and_def), Arc::from(messages))
}

pub fn lookup_name(db: &dyn CoreDatabase, file: FileId, name: Symbol) -> Option<DefId> {
    let items = db.file_items(file);
    items.iter().find(|(n, _)| n == &name).map(|(_, id)| *id)
}

#[self_referencing]
pub struct ArenaAndDef {
    arena: bumpalo::Bump,
    #[borrows(arena)]
    #[covariant]
    pub def: syntax::Def<'this>,
}

impl PartialEq for ArenaAndDef {
    fn eq(&self, other: &Self) -> bool { self.borrow_def() == other.borrow_def() }
}

impl Eq for ArenaAndDef {}

impl fmt::Debug for ArenaAndDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.borrow_def(), f)
    }
}

// TODO: is this safe?
unsafe impl Sync for ArenaAndDef {}
