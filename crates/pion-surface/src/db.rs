use std::sync::Arc;

use pion_common::arena::yoke::Yoke;
use pion_common::arena::ArenaAnd;
use pion_source::FileId;

use crate::reporting::Message;
use crate::syntax;

#[salsa::query_group(SurfaceDatabaseStorage)]
pub trait SurfaceDatabase: salsa::Database + pion_source::db::SourceDatabase {
    fn file_module(&self, file: FileId) -> (ArenaAnd<syntax::Module<'static>>, Arc<[Message]>);
}

fn file_module(
    db: &dyn SurfaceDatabase,
    file: FileId,
) -> (ArenaAnd<syntax::Module<'static>>, Arc<[Message]>) {
    let input = db.file_text(file);

    let arena = bumpalo::Bump::new();
    let mut messages = Vec::new();
    let mut on_message = |message| messages.push(message);
    let yoked = Yoke::attach_to_cart(Arc::new(arena), |arena| {
        let module = syntax::Module::parse(arena, &mut on_message, &input);
        module
    });

    (ArenaAnd::new(yoked), Arc::from(messages))
}
