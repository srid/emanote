# Example inside subfolder/index/ (#542)

Companion fixture for the regression scenario that exercises the
*coexistence* case: `subfolder/index.md` is the folder note for
`subfolder/`, and this file lives inside a deeper folder *also* named
`index`. Pre-#542 the two collapsed onto a single URL; the fix sends
them to distinct ones (`/subfolder.html` vs `/subfolder/index/...`).
