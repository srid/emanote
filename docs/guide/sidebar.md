# Sidebar

In [[html-template|the `book` template]], the sidebar is rendered on the left side. The sidebar tree is determined from the directory layout of the [[markdown|Markdown]] or [[orgmode|Org]] files.in the `sidebar.html` file.

- Sidebar tree is collapsed by default. But this can be disabled by Setting `template.sidebar.collapsed` to `false` in [[yaml-config]]
- The ordering of children in the tree is determined in the following order:
  1. If `order` [[yaml-config|frontmatter]] metadata exists, use that as sort key.
  2. If the note has a H1 title, use that as well; otherwise, use the note filename.