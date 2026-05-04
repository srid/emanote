<!-- styles.tpl is the escape hatch for CSS that inline Tailwind can't
     express: @keyframes, :target, ::marker bodies that need multiple
     properties, decorative ::before/::after stacks, @media queries,
     font-feature-settings, and cross-cutting rules that apply to
     Pandoc-rendered HTML we don't author class-by-class.

     Prefer inline Tailwind in the templates (components/*.tpl) for
     anything expressible as utility classes — colors, spacing,
     typography, hover/dark/motion-reduce variants, before:content-[''],
     marker:*, align-super, etc. Use arbitrary values (e.g.
     [font-variant-numeric:oldstyle-nums]) before reaching for a new
     rule here. Only land in this file when the alternative would be a
     pile of [...] utilities so ugly that a CSS block reads cleaner. -->

<emanoteStaticUrl path="skylighting.css">
  <link rel="stylesheet" href="${url}" />
</emanoteStaticUrl>

<!-- Fonts are self-hosted under _emanote-static/fonts to keep the
     generated static site fully offline-capable. See the README in
     that directory for how to refresh. -->
<emanoteStaticUrl path="fonts/fonts.css">
  <link rel="stylesheet" href="${url}" />
</emanoteStaticUrl>

<style data-category="global-font">
  /* Prose: Lora (warm, variable, very readable book serif).
     UI chrome: Mona Sans (variable display+text sans, more characterful
     than Inter/Space Grotesk; GitHub uses it as their display face).
     Code: Space Mono (pairs stylistically with the prose serif at body
     sizes; the slab terminals echo Lora's serifs). */
  :root {
    --font-serif: 'Lora', ui-serif, Georgia, 'Times New Roman', serif;
    --font-sans: 'Mona Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    --font-mono: 'Space Mono', ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
  }

  body {
    font-family: var(--font-serif);
    line-height: 1.7;
  }

  h1, h2, h3, h4, h5, h6 {
    font-family: var(--font-sans);
    line-height: 1.2;
    font-weight: 600;
  }
  /* Negative tracking is a display-typography move; below ~24px it
     makes Mona Sans glyphs touch and reduces legibility. Scope to the
     larger headings only. */
  h1, h2, h3, h4 {
    letter-spacing: -0.02em;
  }

  #sidebar, #breadcrumbs, #toc, #footer,
  #backlinks, #backlinks-margin, #backlinks-bottom, #right-panel,
  .callout-title,
  ema\:metadata, section[class*="font-mono"] {
    font-family: var(--font-sans);
  }

  code, pre, kbd, samp, .font-mono {
    font-family: var(--font-mono);
  }

  /* Tables — Pandoc emits standard <table> HTML with no classes, so we
     style by element. Scoped under `main` to avoid hitting tables in
     chrome surfaces (sidebar tree, footnote popover, etc). */
  main table {
    width: 100%;
    border-collapse: collapse;
    margin: 1.5rem 0;
    font-size: 0.95rem;
  }
  main thead {
    border-bottom: 2px solid var(--color-gray-200);
    text-align: left;
  }
  main thead th {
    padding: 0.5rem 0.75rem;
    font-weight: 600;
    color: var(--color-gray-900);
    letter-spacing: -0.01em;
  }
  main tbody tr {
    border-bottom: 1px solid var(--color-gray-100);
  }
  main tbody tr:hover {
    background-color: var(--color-gray-50);
  }
  main tbody td {
    padding: 0.5rem 0.75rem;
    vertical-align: top;
  }
  .dark main thead {
    border-bottom-color: var(--color-gray-700);
  }
  .dark main thead th {
    color: var(--color-gray-100);
  }
  .dark main tbody tr {
    border-bottom-color: var(--color-gray-800);
  }
  .dark main tbody tr:hover {
    background-color: rgb(255 255 255 / 0.03);
  }
</style>


<style data-category="error">
  /* Heist error element */
  strong.error {
    color: lightcoral;
    font-size: 90%;
    font-family: monospace;
  }
</style>

<style data-category="kbd">
  /* <kbd> styling — flat with a subtle baseline, theme-variable colours */
  kbd {
    background-color: var(--color-gray-100);
    border: 1px solid var(--color-gray-300);
    border-bottom-width: 2px;
    border-radius: 4px;
    color: var(--color-gray-800);
    display: inline-block;
    font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
    font-size: 0.85em;
    line-height: 1.2;
    padding: 2px 6px;
    white-space: nowrap;
  }

  .dark kbd {
    background-color: var(--color-gray-800);
    border-color: var(--color-gray-700);
    color: var(--color-gray-100);
  }
</style>

<style data-category="callout">
  /* To prevent overemphasis of link styles in callout titles */
  .callout .callout-title a {
    color: inherit;
    text-decoration: underline;
  }

  /* Foldable callouts (Obsidian [!type]+ / [!type]-): suppress the native
     <details>/<summary> disclosure marker since we render our own chevron,
     and rotate the chevron based on open state. */
  details.callout-foldable > summary {
    list-style: none;
  }

  details.callout-foldable > summary::-webkit-details-marker {
    display: none;
  }

  details.callout-foldable[open] > summary .callout-fold-marker svg {
    transform: rotate(180deg);
  }

  /* Nested callouts: drop the outer block margin so the inner callout
     hugs the body of its parent rather than introducing a paragraph gap.
     Both foldable (<details>) and non-foldable (<div>) outer callouts wrap
     their body in `.callout-content`, so a single selector covers both. */
  .callout .callout-content > .callout:last-child {
    margin-bottom: 0;
  }
</style>

<style data-category="lists">
  /* Tighten nested list margins so a bulleted sub-list inside a numbered
     item doesn't push its parent's siblings apart. */
  main li > ul,
  main li > ol {
    margin-top: 0.25rem;
    margin-bottom: 0.25rem;
  }
</style>

<style data-category="task-list">
  /* Task-list items: Pandoc emits the task wrapper as a regular <li>
     with a checkbox SVG (.--ema-checkbox) followed by a <span> of
     inlines. We drop the disc marker and absolutely position the
     checkbox in the marker slot, so task text shares the same
     left edge as plain bullet text in the same list — mixed
     task/non-task lists then read as one column instead of two
     subtly-misaligned ones. */
  main li:has(> svg.--ema-checkbox) {
    list-style: none;
    position: relative;
  }
  main li:has(> svg.--ema-checkbox) > svg.--ema-checkbox {
    position: absolute;
    left: -1.5rem;
    top: 0.3em;
  }
</style>

<style data-category="sidebar-tree">
  /* Folgezettel depth rails: nested tree levels get a subtle left border */
  #sidebar .pl-2 .pl-2 {
    border-left: 1px solid var(--color-gray-200);
    margin-left: 0.25rem;
  }

  .dark #sidebar .pl-2 .pl-2 {
    border-left-color: var(--color-gray-800);
  }
</style>

<style data-category="toc">
  a.--ema-toc {
    background-color: transparent !important;
  }

  /* Visual hierarchy by depth: each nested level (H3, H4, ...) is
     progressively smaller, lighter, and more muted so a long TOC
     stays scannable at a glance. Top-level (H2) items keep full
     weight and contrast. */
  #toc ul a.--ema-toc {
    font-size: 0.875rem;
    font-weight: 500;
    color: var(--color-gray-700);
  }

  .dark #toc ul a.--ema-toc {
    color: var(--color-gray-300);
  }

  #toc ul ul a.--ema-toc {
    font-size: 0.8125rem;
    font-weight: 400;
    color: var(--color-gray-600);
  }

  .dark #toc ul ul a.--ema-toc {
    color: var(--color-gray-400);
  }

  #toc ul ul ul a.--ema-toc {
    font-size: 0.78125rem;
    color: var(--color-gray-500);
  }

  .dark #toc ul ul ul a.--ema-toc {
    color: var(--color-gray-500);
  }

  #toc ul ul ul ul a.--ema-toc {
    font-size: 0.75rem;
    color: var(--color-gray-400);
  }

  .dark #toc ul ul ul ul a.--ema-toc {
    color: var(--color-gray-600);
  }

  /* Active TOC item — neutral grey since TOC entries map to headings
     in the prose, which render in plain text. Keeping primary out of
     here lets the wikilink / title strip own the primary palette and
     stand out as the page's only "destination" affordance. */
  #toc a.--ema-toc.toc-item-active {
    font-weight: 600;
    background-color: var(--color-gray-100) !important;
    color: var(--color-gray-900) !important;
  }

  .dark #toc a.--ema-toc.toc-item-active {
    background-color: var(--color-gray-800) !important;
    color: var(--color-gray-100) !important;
  }
</style>

<style data-category="global-improvements">
  /* Link improvements */
  a {
    text-decoration-thickness: 1px;
    text-underline-offset: 2px;
  }

  /* Focus states for accessibility */
  a:focus-visible, button:focus-visible {
    outline: 2px solid currentColor;
    outline-offset: 2px;
    border-radius: 2px;
  }

  /* Code block enhancements */
  pre {
    position: relative;
  }

  pre code {
    display: block;
    line-height: 1.6;
  }

  /* Copy button for code blocks */
  .code-copy-button {
    position: absolute;
    top: 0.5rem;
    right: 0.5rem;
    padding: 0.5rem;
    background: rgba(255, 255, 255, 0.9);
    border: 1px solid #d1d5db;
    border-radius: 0.375rem;
    color: #374151;
    cursor: pointer;
    opacity: 0;
    transition: opacity 0.15s ease, background-color 0.15s ease;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .code-copy-button svg {
    width: 1rem;
    height: 1rem;
  }

  .dark .code-copy-button {
    background: rgba(31, 41, 55, 0.9);
    border-color: #4b5563;
    color: #d1d5db;
  }

  pre:hover .code-copy-button {
    opacity: 1;
  }

  .code-copy-button:hover {
    background: rgba(255, 255, 255, 1);
  }

  .dark .code-copy-button:hover {
    background: rgba(31, 41, 55, 1);
  }

  .code-copy-button:active {
    transform: scale(0.95);
  }
</style>

<!-- Theme toggle and code-copy behaviors moved to
     _emanote-static/js/{theme-toggle,code-copy}.js — see issue #643. -->

<style data-category="external-link">
  /* External/mail link glyphs — drawn with mask-image so they inherit the
     link's currentColor (one rule for both light and dark themes). */
  a[data-linkicon="external"] {
    padding-right: 1.1em;
  }

  a[data-linkicon="external"]::after {
    content: "";
    display: inline-block;
    width: 0.7em;
    height: 0.7em;
    margin-left: 0.15em;
    margin-right: -1.1em;
    vertical-align: baseline;
    background-color: currentColor;
    opacity: 0.55;
    -webkit-mask: var(--icon-external) no-repeat center / contain;
    mask: var(--icon-external) no-repeat center / contain;
  }

  a[data-linkicon="external"][href^="mailto:"]::after {
    width: 0.9em;
    height: 0.9em;
    -webkit-mask: var(--icon-mail) no-repeat center / contain;
    mask: var(--icon-mail) no-repeat center / contain;
  }

  :root {
    --icon-external: url('data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20"><g style="stroke:black;stroke-width:1;fill:none"><line x1="5" y1="5" x2="5" y2="14"/><line x1="14" y1="9" x2="14" y2="14"/><line x1="5" y1="14" x2="14" y2="14"/><line x1="5" y1="5" x2="9" y2="5"/><line x1="10" y1="2" x2="17" y2="2"/><line x1="17" y1="2" x2="17" y2="9"/><line x1="10" y1="9" x2="17" y2="2"/></g></svg>');
    --icon-mail: url('data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="black" stroke-width="2"><path stroke-linecap="round" stroke-linejoin="round" d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"/></svg>');
  }
</style>

<style data-category="page-load-reveal">
  /* One orchestrated entrance on first paint: sidebar fades in; main
     content fades and drifts up ~8px with a small stagger. Pure CSS so
     it runs pre-paint without JS; gated on prefers-reduced-motion. */
  @media (prefers-reduced-motion: no-preference) {
    @keyframes ema-fade-in {
      from { opacity: 0; }
      to   { opacity: 1; }
    }
    @keyframes ema-rise-in {
      from { opacity: 0; transform: translateY(8px); }
      to   { opacity: 1; transform: none; }
    }
    #sidebar {
      animation: ema-fade-in 400ms ease-out both;
    }
    main {
      animation: ema-rise-in 480ms 60ms ease-out both;
    }
  }
</style>
