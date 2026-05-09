---
slug: rawhtml
---

# Raw HTML regression (issue #119)

Pandoc treats the block below as a raw HTML block. The `</div>` literal
inside it used to crash xmlhtml's renderer with "div cannot contain
text looking like its end tag" — see srid/heist-extra#13 for the fix.
The marker on the inner element is what the e2e step asserts on.

<div>
  <p data-marker="RAWHTML_DIV_OK">inside the div</p>
</div>
