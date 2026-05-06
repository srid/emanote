// Copy-to-clipboard button injected into every <pre> with a child <code>.
// Uses morph.onElement for newly-added blocks and morph.onMorph for Ema
// route changes that preserve the <pre>/<code> node while removing the
// injected sibling button from the DOM.

import { onElement, onMorph } from '@emanote/morph';
import { text } from '@emanote/i18n';

const COPY_ICON =
  '<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z" /></svg>';
const CHECK_ICON =
  '<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" /></svg>';

// WeakMap keeps the per-button reset timer out of the DOM node itself —
// no expando, no name-collision risk with other code touching the button.
const flashTimers = new WeakMap();

function scanCodeBlocks() {
  document.querySelectorAll('pre').forEach(ensureCopyButton);
}

function scanCodeBlocksAfterMorph() {
  scanCodeBlocks();
  requestAnimationFrame(scanCodeBlocks);
}

function ensureCopyButton(pre) {
  if (!codeBlockFor(pre)) return;

  const buttons = pre.querySelectorAll(':scope > .code-copy-button');
  const button = buttons[0] || document.createElement('button');
  buttons.forEach((extra, index) => {
    if (index > 0) extra.remove();
  });

  button.type = 'button';
  button.className = 'code-copy-button';
  resetButton(button);

  if (!button.dataset.emanoteCopyButtonWired) {
    button.dataset.emanoteCopyButtonWired = '1';
    button.addEventListener('click', () => {
      const codeBlock = codeBlockFor(pre);
      navigator.clipboard.writeText(codeBlock?.textContent || '').then(
        () => flash(button, CHECK_ICON, text('copied', 'Copied!')),
        (err) => {
          console.error('Copy failed:', err);
          flash(button, COPY_ICON, text('copyFailed', 'Copy failed'));
        },
      );
    });
  }

  if (!button.parentElement) {
    pre.appendChild(button);
  }
}

function codeBlockFor(pre) {
  return pre.querySelector(':scope > code');
}

function resetButton(button) {
  const pending = flashTimers.get(button);
  if (pending) clearTimeout(pending);
  button.innerHTML = COPY_ICON;
  button.setAttribute('aria-label', text('copyCodeToClipboard', 'Copy code to clipboard'));
  button.setAttribute('title', text('copyCode', 'Copy code'));
  flashTimers.delete(button);
}

function flash(button, icon, title) {
  // Cancel any pending reset on this same button — otherwise a click
  // landing inside the 2s window from a prior click overwrites the new
  // state (most visibly: an error message replaced by a fresh copy icon
  // before the user can read it).
  const pending = flashTimers.get(button);
  if (pending) clearTimeout(pending);
  button.innerHTML = icon;
  button.setAttribute('title', title);
  flashTimers.set(button, setTimeout(() => {
    button.innerHTML = COPY_ICON;
    button.setAttribute('title', text('copyCode', 'Copy code'));
    flashTimers.delete(button);
  }, 2000));
}

onElement('pre', ensureCopyButton);
onMorph(scanCodeBlocksAfterMorph);
