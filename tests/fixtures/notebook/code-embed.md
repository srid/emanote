# Code embed fixture

Regression for [#24](https://github.com/srid/emanote/issues/24): `![[..]]` of
a source file must render through Pandoc's skylighting pipeline so the same
`<span class="kw">`, `<span class="fu">`, … token spans appear as on a fenced
` ```haskell ` block. PR #444 introduced the embed; #624 then moved
highlighting from client-side `highlight.js` to server-side skylighting and
the embed-code template was left emitting raw text.

![[code-embed.hs]]
