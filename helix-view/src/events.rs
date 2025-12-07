use helix_core::{ChangeSet, Rope};
use helix_event::events;
use helix_lsp::LanguageServerId;

use crate::{editor::Config, Document, DocumentId, Editor, ViewId};

events! {
    DocumentDidOpen<'a> {
        editor: &'a mut Editor,
        doc: DocumentId,
        path: &'a std::path::PathBuf
    }
    DocumentDidChange<'a> {
        doc: &'a mut Document,
        view: ViewId,
        old_text: &'a Rope,
        changes: &'a ChangeSet,
        ghost_transaction: bool
    }
    DocumentDidClose<'a> {
        editor: &'a mut Editor,
        doc: Document
    }
    SelectionDidChange<'a> { doc: &'a mut Document, view: ViewId }
    DiagnosticsDidChange<'a> { editor: &'a mut Editor, doc: DocumentId }
    DocumentFocusLost<'a> { editor: &'a mut Editor, doc: DocumentId }

    LanguageServerInitialized<'a> {
        editor: &'a mut Editor,
        server_id: LanguageServerId
    }
    LanguageServerExited<'a> {
        editor: &'a mut Editor,
        server_id: LanguageServerId
    }

    EditorConfigDidChange<'a> {
        old_config: &'a Config,
        editor: &'a mut Editor
    }

    ConfigDidChange<'a> {
        editor: &'a mut Editor,
        old: &'a Config,
        new: &'a Config
    }
}
