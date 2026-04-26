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

  Scenario: A feed-enabled note whose query matches no notes does not crash the build (regression: #490)
    When I fetch "/empty-feed.xml"
    Then the response is a valid Atom feed

  Scenario: Relative links from <dir>/index.md resolve against <dir>/ (issue #608)
    When I open "/subfolder.html"
    Then the article link with text "sibling" has href containing "subfolder/sibling"

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
