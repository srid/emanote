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

  Scenario: Clicking a footnote ref inside a callout resolves to that callout's footnote body
    When I open "/footnotes.html"
    And I click the footnote ref with index "1" inside a callout
    Then the footnote popup contains "CALLOUT_FOOTNOTE_BODY"

  Scenario: Clicking a footnote ref inside an embedded note opens the popup with the embed's footnote body
    When I open "/footnotes.html"
    And I click the footnote ref with index "1" inside an embedded note
    Then the footnote popup contains "EMBED_FOOTNOTE_BODY"

  Scenario: The footnote list is hidden on screen but rendered in print mode
    When I open "/footnotes.html"
    Then no footnote list is visible on screen
    When the page is emulated as print media
    Then at least one footnote list is visible
    And the printed footnote list contains "PARENT_FOOTNOTE_BODY"

  Scenario: Plain callout renders as a non-foldable <div>
    When I open "/callouts.html"
    Then the callout with type "info" is non-foldable

  Scenario: Foldable+expanded callout renders as <details open> with body visible
    When I open "/callouts.html"
    Then the callout with type "tip" is foldable and initially expanded
    And the body of the callout with type "tip" is visible

  Scenario: Foldable+collapsed callout renders as <details> with body hidden
    When I open "/callouts.html"
    Then the callout with type "warning" is foldable and initially collapsed
    And the body of the callout with type "warning" is hidden

  Scenario: Clicking the summary of a foldable callout toggles its open state
    When I open "/callouts.html"
    And I click the summary of the callout with type "warning"
    Then the callout with type "warning" is foldable and initially expanded
    When I click the summary of the callout with type "warning"
    Then the callout with type "warning" is foldable and initially collapsed

  Scenario: A nested callout renders inside its parent's body
    When I open "/callouts.html"
    Then the callout with type "note" contains a nested callout with type "quote"
