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
  Scenario: Editing the .md note that declares a Lua filter live-updates its page
    When I open "/lua-filter-demo.html"
    Then the article body contains "DEMO_FILTER:HELLO"
    When I replace "Lua Filter Demo" with "Lua Filter Demo MD_BODY_EDIT_MARKER" in "lua-filter-demo.md"
    Then the article body contains "MD_BODY_EDIT_MARKER" within 10 seconds
    And the article body contains "DEMO_FILTER:HELLO"

  @live @hot-reload
  Scenario: Editing the .org note that declares a Lua filter live-updates its page
    # Marker avoids underscores so Pandoc's Org reader does not consume
    # them as `_subscript_` syntax — otherwise the rendered innerText
    # collapses the marker and the assertion can't find it.
    When I open "/lua-filter-org-demo.html"
    Then the article body contains "DEMO_FILTER:HELLO"
    When I replace "Lua Filter Org Demo" with "Lua Filter Org Demo ORGBODYEDITMARKER" in "lua-filter-org-demo.org"
    Then the article body contains "ORGBODYEDITMARKER" within 10 seconds
    And the article body contains "DEMO_FILTER:HELLO"

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

  # Hot-reload matrix for a CodeBlock-matching render-time filter
  # (bundled `lua-filters/hello.lua`). The earlier scenarios prove
  # hot-reload for inline-Str-rewrite filters; these three close the
  # gap for filters that transform fenced blocks and prove that all
  # three axes of change (note prose, fence body, filter source) flow
  # to the live page.

  @live @hot-reload
  Scenario: Hot reload — editing prose around a filtered fence
    When I open "/lua-filter-hello-fence.html"
    Then the article body contains "HELLO_PROSE_TOKEN"
    And the article body contains "Hello, HELLO_FENCE_TOKEN!"
    When I replace "HELLO_PROSE_TOKEN" with "HELLO_PROSE_EDITED" in "lua-filter-hello-fence.md"
    Then the article body contains "HELLO_PROSE_EDITED" within 10 seconds
    And the article body contains "Hello, HELLO_FENCE_TOKEN!"

  @live @hot-reload
  Scenario: Hot reload — editing a fence body re-runs the filter on the new content
    When I open "/lua-filter-hello-fence.html"
    Then the article body contains "Hello, HELLO_FENCE_TOKEN!"
    When I replace "HELLO_FENCE_TOKEN" with "HELLO_FENCE_EDITED" in "lua-filter-hello-fence.md"
    Then the article body contains "Hello, HELLO_FENCE_EDITED!" within 10 seconds

  @live @hot-reload
  Scenario: Hot reload — shadowing the bundled hello.lua reloads dependent notes
    # Default-layer hello.lua greets with "Hello, …". Writing a
    # notebook-local `lua-filters/hello.lua` shadows the bundled
    # version (notebook layer beats default layer); the filesystem
    # watcher fires, dependent notes re-render with the new greeting.
    When I open "/lua-filter-hello-fence.html"
    Then the article body contains "Hello, HELLO_FENCE_TOKEN!"
    When I write a hello-shadow filter at "lua-filters/hello.lua" with prefix "Howdy,"
    Then the article body contains "Howdy, HELLO_FENCE_TOKEN" within 10 seconds
