Feature: Pandoc Lua filter hot-reload (issue #263)
  A note's Lua filter declaration resolves from YAML frontmatter.
  Markdown notes use `pandoc.filters.parse` for parse-time filters and
  `pandoc.filters.render.html` for HTML render-time filters. Org notes
  use `#+PANDOC_FILTERS_PARSE` and `#+PANDOC_FILTERS_RENDER_HTML` keywords.
  When a referenced `.lua` file is created, edited, or deleted,
  every dependent note's rendered HTML reflects the new filter
  behavior — without restarting `emanote run`.

  The static-mode build also has to apply a referenced filter at
  generation time. Static gen treats a frontmatter reference to a
  missing filter as a fatal note error (no fsnotify watcher to make
  it self-heal), so the missing-at-parse-time case is exercised
  only in live mode below.

  Scenario: A note's pandoc.filters.parse is applied at build time
    When I open "/lua-filter-demo.html"
    Then the article body contains "DEMO_FILTER:HELLO"
    And the article body does not contain "EMANOTE_LUA_DEMO_TOKEN"

  Scenario: A note's pandoc.filters.render.html is applied at render time
    When I open "/lua-filter-render-demo.html"
    Then the article body contains "RENDER_FILTER:HTML"
    And the article body does not contain "EMANOTE_RENDER_FILTER_TOKEN"

  Scenario: A note can use a bundled default-layer Lua filter
    When I open "/lua-filter-bundled.html"
    Then the page contains a table element with class "bundled-list-table"
    And the article table link with text "a sibling note" has href containing "subfolder/sibling"

  Scenario: An Org note's PANDOC_FILTERS_PARSE keyword is applied at build time
    When I open "/lua-filter-org-demo.html"
    Then the article body contains "DEMO_FILTER:HELLO"
    And the article body does not contain "EMANOTELUAORGDEMO"

  Scenario: An Org note's PANDOC_FILTERS_RENDER_HTML keyword is applied at render time
    When I open "/lua-filter-org-render-demo.html"
    Then the article body contains "RENDER_FILTER:ORG-HTML"
    And the article body does not contain "EMANOTEORGRENDERFILTERTOKEN"

  Scenario: Lua filter sources remain linkable by wikilink
    When I open "/lua-filter-demo.html"
    Then the article link with text "demo filter source" has href containing "filters/demo-filter.lua"
    When I fetch "/filters/demo-filter.lua"
    Then the response body contains "EMANOTE_LUA_DEMO_TOKEN"

  @live @hot-reload
  Scenario: Editing a .lua filter live-updates dependent notes (#263)
    When I open "/lua-filter-demo.html"
    Then the article body contains "DEMO_FILTER:HELLO"
    When I write "filters/demo-filter.lua" so EMANOTE_LUA_DEMO_TOKEN maps to "DEMO_FILTER:CHANGED"
    Then the article body contains "DEMO_FILTER:CHANGED" within 10 seconds

  @live @hot-reload
  Scenario: Editing a render-time .lua filter live-updates dependent notes (#721)
    When I open "/lua-filter-render-demo.html"
    Then the article body contains "RENDER_FILTER:HTML"
    When I write "filters/render-format.lua" so EMANOTE_RENDER_FILTER_TOKEN maps to "RENDER_FILTER:CHANGED"
    Then the article body contains "RENDER_FILTER:CHANGED" within 10 seconds

  @live @hot-reload
  Scenario: Editing a render-time .lua filter live-updates dependent Org notes (#721)
    When I open "/lua-filter-org-render-demo.html"
    Then the article body contains "RENDER_FILTER:ORG-HTML"
    When I write "filters/render-format.lua" so EMANOTEORGRENDERFILTERTOKEN maps to "RENDER_FILTER:ORG-CHANGED"
    Then the article body contains "RENDER_FILTER:ORG-CHANGED" within 10 seconds

  @live @hot-reload
  Scenario: Editing a .lua filter live-updates dependent Org notes (#721)
    When I open "/lua-filter-org-demo.html"
    Then the article body contains "DEMO_FILTER:HELLO"
    When I write "filters/demo-filter.lua" so EMANOTELUAORGDEMO maps to "DEMO_FILTER:ORG-CHANGED"
    Then the article body contains "DEMO_FILTER:ORG-CHANGED" within 10 seconds

  @live @hot-reload
  Scenario: Creating a previously-missing .lua filter wires up its dependents (#263)
    When I write a note "lua-filter-late.md" that references missing filter "filters/late-bound.lua" containing token "EMANOTE_LATE_TOKEN"
    And I wait for "/lua-filter-late.html" to contain "EMANOTE_LATE_TOKEN"
    And I open "/lua-filter-late.html"
    Then the article body contains "EMANOTE_LATE_TOKEN"
    When I write "filters/late-bound.lua" so EMANOTE_LATE_TOKEN maps to "LATE_BOUND:WIRED"
    Then the article body contains "LATE_BOUND:WIRED" within 10 seconds

  @live @hot-reload
  Scenario: Creating a previously-missing .lua filter wires up Org dependents (#721)
    When I write an Org note "lua-filter-late.org" that references missing filter "filters/late-bound.lua" containing token "EMANOTEORGLATETOKEN"
    And I wait for "/lua-filter-late.html" to contain "EMANOTEORGLATETOKEN"
    And I open "/lua-filter-late.html"
    Then the article body contains "EMANOTEORGLATETOKEN"
    When I write "filters/late-bound.lua" so EMANOTEORGLATETOKEN maps to "ORG_LATE_BOUND:WIRED"
    Then the article body contains "ORG_LATE_BOUND:WIRED" within 10 seconds

  @live @hot-reload
  Scenario: Deleting a .lua filter re-parses dependents without it (#263)
    When I open "/lua-filter-demo.html"
    Then the article body contains "DEMO_FILTER:HELLO"
    When I delete "filters/demo-filter.lua"
    Then the article body contains "EMANOTE_LUA_DEMO_TOKEN" within 10 seconds

  @live @hot-reload
  Scenario: Deleting a .lua filter re-parses Org dependents without it (#721)
    When I open "/lua-filter-org-demo.html"
    Then the article body contains "DEMO_FILTER:HELLO"
    When I delete "filters/demo-filter.lua"
    Then the article body contains "EMANOTELUAORGDEMO" within 10 seconds
