Feature: Smoke
  Backend-agnostic checks that the Tailwind v4 CSS pipeline
  produces a usable site in both live (`emanote run`, Tailwind CDN
  MutationObserver) and static (`emanote gen`, Tailwind CLI) modes.

  Scenario: CSS loads on the home page
    When I open "/"
    Then the primary palette custom property resolves to a non-empty value

  Scenario: Per-page template.theme override is applied
    Given I note the resolved primary palette at "/"
    When I open "/themed.html"
    Then the resolved primary palette differs from the noted value

  Scenario: Inline math renders to MathML at build time
    When I open "/math.html"
    Then the page contains an inline <math> element in the MathML namespace

  Scenario: Display math renders to block MathML at build time
    When I open "/math.html"
    Then the page contains a block <math> element in the MathML namespace

  Scenario: KaTeX is not loaded by default
    When I open "/math.html"
    Then no KaTeX stylesheet is referenced

  Scenario: Clicking a parent-level footnote ref opens the popup
    When I open "/footnotes.html"
    And I click the footnote ref with index "1" in the parent body
    Then the footnote popup contains "PARENT_FOOTNOTE_BODY"
    And the footnote popup body has a non-transparent background

  Scenario: Clicking a footnote ref inside a callout resolves to that callout's footnote body
    When I open "/footnotes.html"
    And I click the footnote ref with index "1" inside a callout
    Then the footnote popup contains "CALLOUT_FOOTNOTE_BODY"

  Scenario: Clicking a footnote ref inside an embedded note opens the popup with the embed's footnote body
    When I open "/footnotes.html"
    And I click the footnote ref with index "1" inside an embedded note
    Then the footnote popup contains "EMBED_FOOTNOTE_BODY"

  Scenario: Raw HTML block containing </div> renders without crashing (regression: #119)
    When I open "/rawhtml.html"
    Then the page contains an element with data-marker "RAWHTML_DIV_OK"

  Scenario: Markdown between blank-line-separated raw-HTML tags nests inside (#433)
    When I open "/rawhtml-details.html"
    Then the emitted HTML for "/rawhtml-details.html" wraps no <rawhtml> around its <details> tags

  Scenario: A feed-enabled note whose query matches no notes does not crash the build (regression: #490)
    When I fetch "/empty-feed.xml"
    Then the response is a valid Atom feed

  Scenario: Relative links from <dir>/index.md resolve against <dir>/ (issue #608)
    When I open "/subfolder.html"
    Then the article link with text "sibling" has href containing "subfolder/sibling"

  Scenario: Wiki link custom titles render HTML entities (regression: #441)
    When I open "/wikilink-entities.html"
    Then the first article link has HTML containing "Spivak&nbsp;(2014)"

  Scenario: URL-bearing link labels render as one hyperlink (regression: #349)
    When I open "/issue-349.html"
    Then the article link with text "https://issue349-case1.example.com" has href containing "issue349-case1.example.com#anchor"
    And the article link with text "case2@issue349.example.com" has href containing "mailto:case2@issue349.example.com"
    And the article link with text "A sentence linking to" has href containing "issue349-case3-target.example.com"
    And the article link with text "https://issue349-case4.example.com" has href containing "issue349-case4-target.example.com"
    And the article body has exactly 4 hyperlinks to issue-349 case targets

  Scenario: A malformed YAML file is surfaced as a banner on its sibling note (regression: #285)
    When I open "/broken-285.html"
    Then the page rendered without an Ema exception
    And the page shows the YAML errors banner

  Scenario: The YAML errors banner does not leak onto unrelated pages (regression: #285)
    When I open "/"
    Then the page rendered without an Ema exception
    And the page does not show the YAML errors banner

  Scenario: Markdown link to a static .xml asset resolves to the file (regression: #547)
    When I open "/xmllink.html"
    Then the article link with text "xml asset" has href containing "test.xml"

  Scenario: Cyclic note embedding stops at a placeholder instead of nesting forever (regression: #362)
    When I open "/cycle-a.html"
    Then the page rendered without an Ema exception
    And the page contains a cyclic-embed placeholder for "Cycle A"

  Scenario: The footnote list is hidden on screen but rendered in print mode
    When I open "/footnotes.html"
    Then no footnote list is visible on screen
    When the page is emulated as print media
    Then at least one footnote list is visible
    And the printed footnote list contains "PARENT_FOOTNOTE_BODY"

  Scenario: Theme toggle button flips the dark class and persists to localStorage
    When I open "/"
    And I click the theme toggle
    Then the documentElement has class "dark"
    And localStorage "emanote-theme" is "dark"

  Scenario: Every fenced code block gets a copy button at first paint
    When I open "/code.html"
    Then every <pre> with a child <code> has a .code-copy-button

  Scenario: Scrolling a section into view highlights its TOC link
    When I open "/toc.html"
    And I scroll the heading with id "cherry" into the active band
    Then the TOC link for "#cherry" has class "toc-item-active"

  @morph
  Scenario: TOC scroll-spy survives Ema's in-app morph navigation (issue #667)
    When I open "/"
    And I navigate via Ema to "/toc.html"
    And I scroll the heading with id "cherry" into the active band
    Then the TOC link for "#cherry" has class "toc-item-active"

  Scenario: Ctrl+K opens the Stork search modal; Esc closes it
    When I open "/"
    Then the Stork search modal is "hidden"
    When I press "Control+K"
    Then the Stork search modal is "visible"
    When I press "Escape"
    Then the Stork search modal is "hidden"

  Scenario: Clicking a sidebar/breadcrumbs search button opens the Stork search modal
    When I open "/"
    Then the Stork search modal is "hidden"
    When I click the Stork search trigger
    Then the Stork search modal is "visible"

  @morph
  Scenario: Stork search dialog stays styled after Ema's in-app morph navigation
    When I open "/"
    And I navigate via Ema to "/toc.html"
    And I click the Stork search trigger
    Then the Stork search modal is "visible"
    And the Stork wrapper has the edible theme class

  Scenario: Daily-named backlinks land in the Timeline panel
    When I open "/dailyhost.html"
    Then the Timeline panel links to "dailyhost/2025-01-01"
    And the Timeline panel links to "dailyhost/2025-01-02"

  Scenario: Non-daily backlinks land in the Backlinks panel and not the Timeline
    When I open "/dailyhost.html"
    Then the Backlinks panel links to "dailyhost-mention"
    And the Backlinks panel does not link to "dailyhost/2025-01-01"
    And the Timeline panel does not link to "dailyhost-mention"

  Scenario: Backlink context wrapper does not impose a vertical scrollbar (no overflow-x:auto on the outer wrapper)
    When I open "/dailyhost.html"
    Then every backlink context wrapper has overflow-y "visible"

  Scenario: Folders named index don't collapse breadcrumb URLs (regression: #542)
    When I open "/index/index/index/example.html"
    Then the immediate-parent breadcrumb href contains "index/index/index"
    And the immediate-parent breadcrumb href does not equal "index/index"

  Scenario: Folder note coexists with a same-named child folder (regression: #542)
    When I open "/subfolder/index/example.html"
    Then the breadcrumb at depth 1 has href containing "subfolder"
    And the breadcrumb at depth 2 has href containing "subfolder/index"
    And the breadcrumb at depth 1 has a different href from depth 2

  Scenario: Tag URLs percent-encode reserved characters (regression: #199)
    When I open "/issue-199.html"
    Then the article tag link with text "###structure" has href containing "-/tags/%23%23structure.html"
    And the metadata tag chip with text "###structure" has href containing "-/tags/%23%23structure.html"

  Scenario: Tag declared in sibling folder YAML appears as a metadata chip on the child note (regression: #352)
    When I open "/issue-352/note.html"
    Then the metadata tag chip with text "issue-352-cascaded" has href containing "-/tags/issue-352-cascaded.html"

  Scenario: Tag declared in sibling folder YAML produces a tag-index page that lists the cascaded note (regression: #352)
    When I fetch "/-/tags/issue-352-cascaded.html"
    Then the response body contains "issue-352/note"

  Scenario: Tag declared in sibling folder YAML appears on the root tag-index page (regression: #352)
    When I fetch "/-/tags.html"
    Then the response body contains "-/tags/issue-352-cascaded.html"

  Scenario: Cascaded YAML tag survives when the child note declares its own tags (regression: #697)
    When I open "/issue-697/note.html"
    Then the metadata tag chip with text "issue-697-cascaded" has href containing "-/tags/issue-697-cascaded.html"
    And the metadata tag chip with text "issue-697-own" has href containing "-/tags/issue-697-own.html"
    And the metadata tag chip with text "issue-697-inline" has href containing "-/tags/issue-697-inline.html"

  Scenario: Cascaded YAML tag still indexes a child note that declares its own tags (regression: #697)
    When I fetch "/-/tags/issue-697-cascaded.html"
    Then the response body contains "issue-697/note"

  @no-morph
  Scenario: Sidebar month folder renders as a calendar grid (issue #700)
    When I open "/calendar-test/2026/04/2026-04-15.html"
    Then the sidebar month calendar is visible with header "Apr 2026"
    And the sidebar month calendar links day 1 to "calendar-test/2026/04/2026-04-01"
    And the sidebar month calendar links day 15 to "calendar-test/2026/04/2026-04-15"
    And the sidebar month calendar links day 30 to "calendar-test/2026/04/2026-04-30"
    And the sidebar month calendar has 3 filled day cells

  @no-morph
  Scenario: Sidebar month calendar distinguishes active route and today (issue #700)
    Given the browser date is "2026-04-30"
    When I open "/calendar-test/2026/04/2026-04-15.html"
    Then the sidebar month calendar marks day 15 as the active route
    And the sidebar month calendar marks day 30 as today

  @no-morph
  Scenario: Sidebar month folder swaps the linear list, not augments it (issue #700)
    When I open "/calendar-test/2026/04/2026-04-15.html"
    Then the sidebar has no plain link with text "2026-04-01"
    And the sidebar has no plain link with text "2026-04-30"

  @no-morph
  Scenario: Sidebar non-month folders do not render as a calendar (issue #700)
    When I open "/"
    Then the sidebar tree has no calendar wrapper for non-month folders

  @morph
  Scenario: Sidebar calendar survives Ema's in-app morph navigation (issue #700)
    When I open "/calendar-test/2026/04/2026-04-01.html"
    And I navigate via Ema to "/calendar-test/2026/04/2026-04-30.html"
    Then the sidebar month calendar is visible with header "Apr 2026"
    And the sidebar month calendar links day 30 to "calendar-test/2026/04/2026-04-30"
    And the sidebar month calendar marks day 30 as the active route
