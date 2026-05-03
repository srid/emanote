# Cascaded-tag fixture

This note declares no tags in its own frontmatter. The tag
`issue-352-cascaded` is declared in the sibling `issue-352.yaml`
and cascades onto every note inside this folder.

The cascaded tag must reach the global tag index so the chip URL
above resolves and the tag-index page lists this note. The body
contains no inline hash-tag syntax — that would produce a same-index
auto-tag and lodash-merge would clobber the cascaded element.
