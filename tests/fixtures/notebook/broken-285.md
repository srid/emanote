# Broken-yaml landing page

This note's sibling `broken-285.yaml` contains a malformed YAML body
(non-string mapping key, see issue #285). The note exists so the
regression scenario can land on a route whose cascade actually
includes the broken yaml — making the banner appear here, not on
every other page in the fixture.
