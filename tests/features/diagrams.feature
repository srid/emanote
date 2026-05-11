Feature: Bundled diagram Lua filter (issue #625)
  Notes opting into `lua-filters/diagram.lua` from their
  `pandoc.filters.render.html` frontmatter get fenced code blocks
  rewritten into inline `<svg>` elements at HTML render time. The
  diagram renderer binaries (`d2`, `typst`) live in the wrapped
  Emanote closure so static and live builds render the same SVG
  offline.

  Scenario: A d2 code block renders to inline SVG
    When I open "/diagram-d2-demo.html"
    Then the article body contains an "svg" element

  Scenario: A cetz code block renders to inline SVG
    When I open "/diagram-cetz-demo.html"
    Then the article body contains an "svg" element
