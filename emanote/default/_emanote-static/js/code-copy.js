// Copy-to-clipboard button injected into every <pre> with a child <code>.
// Uses morph.onElement so blocks added by Ema's live-server patches get
// buttons too — the prior inline IIFE only wired buttons present at
// DOMContentLoaded.

import { onElement } from './morph.js';

const COPY_ICON =
  '<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z" /></svg>';
const CHECK_ICON =
  '<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" /></svg>';

onElement('pre > code', (codeBlock) => {
  const pre = codeBlock.parentElement;
  const button = document.createElement('button');
  button.className = 'code-copy-button';
  button.innerHTML = COPY_ICON;
  button.setAttribute('aria-label', 'Copy code to clipboard');
  button.setAttribute('title', 'Copy code');

  button.addEventListener('click', () => {
    navigator.clipboard.writeText(codeBlock.textContent).then(
      () => flash(button, CHECK_ICON, 'Copied!'),
      (err) => {
        console.error('Copy failed:', err);
        flash(button, COPY_ICON, 'Copy failed');
      },
    );
  });

  pre.appendChild(button);
});

// WeakMap keeps the per-button reset timer out of the DOM node itself —
// no expando, no name-collision risk with other code touching the button.
const flashTimers = new WeakMap();

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
    button.setAttribute('title', 'Copy code');
    flashTimers.delete(button);
  }, 2000));
}
