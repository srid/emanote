# Example inside nested index folders (#542)

Sole purpose: drive the regression scenario in `smoke.feature` for
[#542](https://github.com/srid/emanote/issues/542). The on-disk layout
`index/index/index/example.md` exists so that the breadcrumb generated
for this note has to traverse three folders that are themselves named
`index`. Before the fix, the immediate-parent breadcrumb's `href`
collapsed to one folder too shallow.
