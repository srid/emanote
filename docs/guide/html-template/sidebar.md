# Sidebar

In [[html-template|the `book` template]], the sidebar is rendered on the left side. The sidebar tree is determined from the directory layout of the [[markdown|Markdown]] or [[orgmode|Org]] files.

- The sidebar tree is collapsed by default. But this can be disabled by setting `template.sidebar.collapsed` to `false` in [[yaml-config]]
- The ordering of children in the tree is determined in the following order (this is also the order in which [[query]] results are rendered by default):
  1. If the `order` [[yaml-config|frontmatter]] metadata exists, use that as the primary sort key.
  2. If the note has a H1 title, use that as the secondary sort key; otherwise, use the note filename as the secondary sort key.

- [ ] The same layout is used in the note index page. Create index/{note,tag,task} doc tree. 