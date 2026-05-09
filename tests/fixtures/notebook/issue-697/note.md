---
tags:
  - issue-697-own
---

# Cascade-survives-own-tags fixture

This note declares `issue-697-own` in its frontmatter, and an inline
#issue-697-inline tag in the body. The sibling `issue-697.yaml`
contributes `issue-697-cascaded` via the data cascade.

All three must reach the page chip strip and the global tag index;
prior to #697 the cascaded tag was clobbered the moment the leaf
note declared any tags of its own.
