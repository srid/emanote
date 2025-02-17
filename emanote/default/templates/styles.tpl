<style data-category="error">
  /* Heist error element */
  strong.error {
    color: lightcoral;
    font-size: 90%;
    font-family: monospace;
  }
</style>

<style data-category="kbd">
  /* <kbd> styling */
  kbd {
    background-color: #eee;
    border-radius: 3px;
    border: 1px solid #b4b4b4;
    box-shadow:
      0 1px 1px rgba(0, 0, 0, 0.2),
      0 2px 0 0 rgba(255, 255, 255, 0.7) inset;
    display: inline-block;
    line-height: 1;
    padding: 2px 4px;
    white-space: nowrap;
  }
</style>

<style data-category="callout">
  /* To prevent overemphasis of link styles in callout titles */
  .callout .callout-title a {
    color: inherit;
    text-decoration: underline;
  }
</style>

<style data-category="toc">
  a.--ema-toc:not(.toc-item-active) {
    background-color: #ffffff !important;
  }
</style>

<style data-category="external-link">
  /* External link icon */
  a[data-linkicon=""]::after {
    content: ""
  }

  a[data-linkicon=none]::after {
    content: ""
  }

  a[data-linkicon="external"] {
    padding-right: 24px;
  }

  a[data-linkicon="external"]::after {
    margin-right: -24px;
    content: url('data:image/svg+xml,\
      <svg xmlns="http://www.w3.org/2000/svg" height="0.7em" viewBox="0 0 20 20"> \
        <g style="stroke:gray;stroke-width:1"> \
          <line x1="5" y1="5" x2="5" y2="14" /> \
          <line x1="14" y1="9" x2="14" y2="14" /> \
          <line x1="5" y1="14" x2="14" y2="14" /> \
          <line x1="5" y1="5" x2="9" y2="5"  /> \
          <line x1="10" y1="2" x2="17" y2="2"  /> \
          <line x1="17" y1="2" x2="17" y2="9" /> \
          <line x1="10" y1="9" x2="17" y2="2" style="stroke-width:1.0" /> \
        </g> \
      </svg>');
  }

  a[data-linkicon="external"][href^="mailto:"]::after {
    content: url('data:image/svg+xml,\
        <svg \
          xmlns="http://www.w3.org/2000/svg" \
          height="0.7em" \
          fill="none" \
          viewBox="0 0 24 24" \
          stroke="gray" \
          stroke-width="2"> \
          <path \
            stroke-linecap="round" \
            stroke-linejoin="round" \
            d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z" /> \
        </svg>');
  }
</style>