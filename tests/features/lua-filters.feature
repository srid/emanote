Feature: Pandoc Lua filter hot-reload (issue #263)
  A note's `pandoc.filters` frontmatter resolves at parse time.
  When a referenced `.lua` file is created, edited, or deleted,
  every dependent note's rendered HTML reflects the new filter
  behavior — without restarting `emanote run`.

  The static-mode build also has to apply a referenced filter at
  generation time. Static gen treats a frontmatter reference to a
  missing filter as a fatal note error (no fsnotify watcher to make
  it self-heal), so the missing-at-parse-time case is exercised
  only in live mode below.

  Scenario: A note's pandoc.filters is applied at build time
    When I open "/lua-filter-demo.html"
    Then the article body contains "DEMO_FILTER:HELLO"
    And the article body does not contain "EMANOTE_LUA_DEMO_TOKEN"

  @live @hot-reload
  Scenario: Editing a .lua filter live-updates dependent notes (#263)
    When I open "/lua-filter-demo.html"
    Then the article body contains "DEMO_FILTER:HELLO"
    When I write "filters/demo-filter.lua" so EMANOTE_LUA_DEMO_TOKEN maps to "DEMO_FILTER:CHANGED"
    Then the article body contains "DEMO_FILTER:CHANGED" within 10 seconds

  @live @hot-reload
  Scenario: Creating a previously-missing .lua filter wires up its dependents (#263)
    When I write a note "lua-filter-late.md" that references missing filter "filters/late-bound.lua" containing token "EMANOTE_LATE_TOKEN"
    And I wait for "/lua-filter-late.html" to contain "EMANOTE_LATE_TOKEN"
    And I open "/lua-filter-late.html"
    Then the article body contains "EMANOTE_LATE_TOKEN"
    When I write "filters/late-bound.lua" so EMANOTE_LATE_TOKEN maps to "LATE_BOUND:WIRED"
    Then the article body contains "LATE_BOUND:WIRED" within 10 seconds

  @live @hot-reload
  Scenario: Deleting a .lua filter re-parses dependents without it (#263)
    When I open "/lua-filter-demo.html"
    Then the article body contains "DEMO_FILTER:HELLO"
    When I delete "filters/demo-filter.lua"
    Then the article body contains "EMANOTE_LUA_DEMO_TOKEN" within 10 seconds
