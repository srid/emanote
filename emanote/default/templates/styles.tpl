<style data-category="global-font">
  /* Refined typography with system font stack */
  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
    line-height: 1.7;
    letter-spacing: 0.01em;
  }

  h1, h2, h3, h4, h5, h6 {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
    letter-spacing: -0.02em;
    line-height: 1.3;
    font-weight: 700;
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
  /* <kbd> styling */
  kbd {
    background-color: #f7f7f7;
    border-radius: 6px;
    border: 1px solid #d1d5db;
    box-shadow:
      0 1px 2px rgba(0, 0, 0, 0.1),
      0 2px 0 0 rgba(255, 255, 255, 0.8) inset;
    display: inline-block;
    line-height: 1.2;
    padding: 3px 6px;
    white-space: nowrap;
    font-size: 0.9em;
  }

  /* Dark mode kbd styling */
  .dark kbd {
    background-color: #374151;
    border: 1px solid #4b5563;
    box-shadow:
      0 1px 2px rgba(0, 0, 0, 0.4),
      0 2px 0 0 rgba(255, 255, 255, 0.08) inset;
    color: #f3f4f6;
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
  a.--ema-toc {
    background-color: transparent !important;
  }

  /* Active TOC item styling */
  a.--ema-toc.toc-item-active {
    font-weight: 600;
    border-left: 3px solid #3b82f6;
    padding-left: calc(0.5rem - 3px) !important;
  }

  /* Light mode active state */
  a.--ema-toc.toc-item-active {
    background-color: #eff6ff !important;
    color: #1d4ed8 !important;
  }

  /* Dark mode active state */
  .dark a.--ema-toc.toc-item-active {
    background-color: #172554 !important;
    color: #93c5fd !important;
    border-left-color: #3b82f6;
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

  .dark a[data-linkicon="external"]::after {
    content: url('data:image/svg+xml,\
      <svg xmlns="http://www.w3.org/2000/svg" height="0.7em" viewBox="0 0 20 20"> \
        <g style="stroke:lightgray;stroke-width:1"> \
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

  .dark a[data-linkicon="external"][href^="mailto:"]::after {
    content: url('data:image/svg+xml,\
        <svg \
          xmlns="http://www.w3.org/2000/svg" \
          height="0.7em" \
          fill="none" \
          viewBox="0 0 24 24" \
          stroke="lightgray" \
          stroke-width="2"> \
          <path \
            stroke-linecap="round" \
            stroke-linejoin="round" \
            d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z" /> \
        </svg>');
  }
</style>