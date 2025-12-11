use helix_core::hashmap;
use helix_view::icons::ICONS;
use helix_view::{theme::Style, Editor};
use std::borrow::Cow;
use std::error::Error as _;
use std::{
    fs,
    path::{Path, PathBuf},
};
use tui::text::{Span, Spans, ToSpan};
use tui::widgets::Cell;
use crate::job::RequireRender;

use helix_stdx::path;

use crate::{compositor::Context, ctrl, job::Callback};

use super::prompt::Movement;
use super::{
    directory_content, overlay,  Picker, PickerColumn, Prompt, PromptEvent,
};

/// for each path: (path to item, is the path a directory?)
type ExplorerItem = (PathBuf, bool);
/// (file explorer root, directory style)
type ExplorerData = (PathBuf, Style);

type FileExplorer = Picker<ExplorerItem, ExplorerData>;





pub fn file_explorer(root: PathBuf, editor: &Editor) -> Result<FileExplorer, std::io::Error> {
    let directory_style = editor.theme.get("ui.text.directory");
    let directory_content = directory_content(&root, editor)?;

    let columns = [PickerColumn::new(
        path::get_relative_dir(&root),
        |(path, is_dir): &ExplorerItem, (_root, directory_style): &ExplorerData| {
            let icons = ICONS.load();
            let name = path.file_name();
            // If path is `..` then this will be `None` and signifies being the
            // previous directory, which said another way, is the currently open
            // directory we are viewing.
            let is_open = name.is_none() && *is_dir;

            // Path `..` does not have a name, and so will become `..` as a string.
            let name = name.map_or_else(|| Cow::Borrowed(".."), |dir| dir.to_string_lossy());
            if *is_dir {
                match icons.fs().directory(is_open) {
                    Some(icon) => Span::styled(format!("{icon} {name}/"), *directory_style).into(),
                    None => Span::styled(format!("{name}/"), *directory_style).into(),
                }
            } else if let Some(icon) = icons.fs().from_path(path) {
                let mut spans = Vec::with_capacity(2);
                spans.push(icon.to_span_with(|icon| format!("{icon} ")));
                spans.push(Span::raw(name));

                Cell::from(Spans::from(spans))
            } else {
                name.into()
            }
        },
    )];
    
    let picker = Picker::new(
        columns,
        0,
        directory_content,
        (root, directory_style),
        move |cx, (path, is_dir): &(PathBuf, bool), action| {
            if *is_dir {
                let new_root = helix_stdx::path::normalize(path);
                let callback = Box::pin(async move {
                    let call: Callback =
                        Callback::EditorCompositor(Box::new(move |editor, compositor| {
                            if let Ok(picker) = file_explorer(new_root, editor) {
                                compositor.push(Box::new(overlay::overlaid(picker)));
                            }
                            RequireRender::Render
                        }));
                    Ok(call)
                });
                cx.jobs.callback(callback);
            } else if let Err(e) = cx.editor.open(path, action) {
                let err = if let Some(err) = e.source() {
                    format!("{}", err)
                } else {
                    format!("unable to open \"{}\"", path.display())
                };
                cx.editor.set_error(err);
            }
        },
    )
    .always_show_headers()
    .with_preview(|_editor, (path, _is_dir)| Some((path.as_path().into(), None)))
    .with_title("File Explorer".into());


    Ok(picker)
}


