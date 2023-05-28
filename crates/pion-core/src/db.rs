use std::sync::Arc;

use pion_common::arena::yoke::Yoke;
use pion_common::arena::ArenaAnd;
use pion_source::FileId;
use pion_surface::syntax::Symbol;

use crate::reporting::Message;
use crate::{elab, syntax};

pub type DefId = usize;

#[salsa::query_group(CoreDatabaseStorage)]
pub trait CoreDatabase: salsa::Database + pion_surface::db::SurfaceDatabase {
    fn file_items(&self, file: FileId) -> Arc<[(Symbol, DefId)]>;

    fn def_core(
        &self,
        file: FileId,
        def: DefId,
    ) -> (ArenaAnd<syntax::Def<'static>>, Arc<[Message]>);
}

fn file_items(db: &dyn CoreDatabase, file: FileId) -> Arc<[(Symbol, DefId)]> {
    let (surface_module, _) = db.file_module(file);
    let module: &pion_surface::syntax::Module = surface_module.get();

    module
        .items
        .iter()
        .enumerate()
        .map(|(id, item)| match item {
            pion_surface::syntax::Item::Def(def) => (def.name, id),
        })
        .collect()
}

fn def_core(
    db: &dyn CoreDatabase,
    file: FileId,
    def: DefId,
) -> (ArenaAnd<syntax::Def<'static>>, Arc<[Message]>) {
    let (surface_module, _) = db.file_module(file);
    let pion_surface::syntax::Item::Def(surface_def) = surface_module.get().items[def];

    let arena = bumpalo::Bump::new();
    let mut messages = Vec::new();
    let mut on_message = |message| messages.push(message);
    let yoked = Yoke::attach_to_cart(Arc::new(arena), |arena| {
        elab::elab_def(db, arena, &mut on_message, file, surface_def)
    });

    (ArenaAnd::new(yoked), Arc::from(messages))
}

pub fn lookup_name(db: &dyn CoreDatabase, file: FileId, name: Symbol) -> Option<DefId> {
    let items = db.file_items(file);
    items.iter().find(|(n, _)| n == &name).map(|(_, id)| *id)
}
