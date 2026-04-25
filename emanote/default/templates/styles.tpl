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

<link rel="stylesheet" href="${ema:emanoteStaticLayerUrl}/skylighting.css" />

<!-- Fonts are self-hosted under _emanote-static/fonts to keep the
     generated static site fully offline-capable. See the README in
     that directory for how to refresh. -->
<link rel="stylesheet" href="${ema:emanoteStaticLayerUrl}/fonts/fonts.css" />

<style data-category="global-font">
  /* Font option C (trial):
     Prose: Lora (warm, variable, very readable book serif).
     UI chrome: Space Grotesk (geometric with quirks — personal-webpage energy).
     Code: Space Mono (pairs stylistically with Space Grotesk). */
  :root {
    --font-serif: 'Lora', ui-serif, Georgia, 'Times New Roman', serif;
    --font-sans: 'Space Grotesk', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    --font-mono: 'Space Mono', ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
  }

  body {
    font-family: var(--font-serif);
    line-height: 1.7;
  }

  h1, h2, h3, h4, h5, h6 {
    font-family: var(--font-sans);
    letter-spacing: -0.02em;
    line-height: 1.2;
    font-weight: 600;
  }

  #sidebar, #breadcrumbs, #toc, #footer, #backlinks, .callout-title,
  ema\:metadata, section[class*="font-mono"] {
    font-family: var(--font-sans);
  }

  code, pre, kbd, samp, .font-mono {
    font-family: var(--font-mono);
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
  /* Task-list items: swap the disc bullet for flex-aligned checkbox + body. */
  main li:has(> svg.--ema-checkbox) {
    list-style: none;
    display: flex;
    gap: 0.5rem;
    align-items: baseline;
    margin-left: -1rem;
  }

  main li:has(> svg.--ema-checkbox) > span {
    flex: 1;
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

  /* Active TOC item — uses theme primary palette so per-site theme overrides
     flow through without hardcoding a colour here. The selector is scoped
     under #toc so it beats the per-depth color rules above. */
  #toc a.--ema-toc.toc-item-active {
    font-weight: 600;
    border-left: 3px solid var(--color-primary-500);
    padding-left: calc(0.5rem - 3px) !important;
    background-color: var(--color-primary-50) !important;
    color: var(--color-primary-700) !important;
  }

  .dark #toc a.--ema-toc.toc-item-active {
    background-color: var(--color-primary-950) !important;
    color: var(--color-primary-300) !important;
    border-left-color: var(--color-primary-400);
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

<script>
  // Theme toggle: persist choice in localStorage; the early-load script in
  // base.tpl applies it before first paint so there's no FOUC on reload.
  (function () {
    window.emanote = window.emanote || {};
    window.emanote.theme = {
      toggle: function () {
        var root = document.documentElement;
        var isDark = root.classList.toggle('dark');
        root.style.colorScheme = isDark ? 'dark' : 'light';
        try { localStorage.setItem('emanote-theme', isDark ? 'dark' : 'light'); } catch (e) {}
        // Mermaid reads system colour-scheme at init; reload so any diagrams re-render.
        if (document.querySelector('.mermaid')) window.location.reload();
      }
    };
  })();
</script>

<script>
  // Add copy buttons to code blocks
  document.addEventListener('DOMContentLoaded', function() {
    const copyIcon = '<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z" /></svg>';
    const checkIcon = '<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" /></svg>';

    document.querySelectorAll('pre code').forEach(function(codeBlock) {
      const pre = codeBlock.parentElement;

      // Create copy button
      const button = document.createElement('button');
      button.className = 'code-copy-button';
      button.innerHTML = copyIcon;
      button.setAttribute('aria-label', 'Copy code to clipboard');
      button.setAttribute('title', 'Copy code');

      button.addEventListener('click', function() {
        const text = codeBlock.textContent;
        navigator.clipboard.writeText(text).then(function() {
          button.innerHTML = checkIcon;
          button.setAttribute('title', 'Copied!');
          setTimeout(function() {
            button.innerHTML = copyIcon;
            button.setAttribute('title', 'Copy code');
          }, 2000);
        }).catch(function(err) {
          console.error('Copy failed:', err);
          button.setAttribute('title', 'Copy failed');
          setTimeout(function() {
            button.innerHTML = copyIcon;
            button.setAttribute('title', 'Copy code');
          }, 2000);
        });
      });

      pre.appendChild(button);
    });
  });
</script>

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