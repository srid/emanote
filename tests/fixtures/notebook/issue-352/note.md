<!--
Fixture maintenance note (kept out of the rendered body so it doesn't
itself become a regression vector):

Do NOT add inline hash-tag syntax to this fixture. Body-extracted
tags lodash-merge with cascade tags by array index, and a same-index
collision would clobber the cascaded element — see issue srid/emanote#697.
The whole point of this fixture is to assert the cascade survives.
-->

# Cascaded-tag fixture

This note declares no tags in its own frontmatter. The tag
`issue-352-cascaded` is declared in the sibling `issue-352.yaml`
and cascades onto every note inside this folder.

The cascaded tag must reach the global tag index so the chip URL
above resolves and the tag-index page lists this note.
