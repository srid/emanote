# Sidebar

In [[html-template|the default template]], the sidebar is rendered on the left side. The sidebar tree is determined from [[folgezettel|folgezettel heterarchy]].

- The sidebar tree is collapsed by default. But this can be disabled by setting `template.sidebar.collapsed` to `false` in [[yaml-config]]
- The ordering of children in the tree is determined in the following order (this is also the order in which [[query]] results are rendered by default):
  1. if `template.sidebar.folders-first` is set to `true` (default `false`), directories are put before single notes, respecting the other order criteria. Otherwise, directories and notes will be interleaved.
  2. If the `order` [[yaml-config|frontmatter]] metadata exists, use that as the primary sort key.
  3. If the note has a H1 title, use that as the secondary sort key; otherwise, use the note filename as the secondary sort key.

## Customizing page title

The sidebar will use a page's title by default, but you can override the sidebar item text by setting the `short-title` [[yaml-config|frontmatter]] property.

```markdown
---
short-title: hello
---

# Hello World

Foo bar ...
```

This page will use "Hello World" as its title in the main view, but "hello" in the sidebar and index pages. For real-world example, see the page "[[lua-filters]]".

## Disabling the sidebar

In [[yaml-config]],

```yaml
template:
  sidebar:
    enable: false
```