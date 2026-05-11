// Note focus mode: expand the central note area without entering browser
// fullscreen. State lives per tab and is re-applied after Ema morphs the DOM.

import { ready, onMorph } from '@emanote/morph';
import { text } from '@emanote/i18n';

const STORAGE_KEY = 'emanote-note-focus';
const ROOT_CLASS = 'emanote-note-focus';
const BUTTON_SELECTOR = 'button[data-emanote-note-focus-toggle]';

const MAXIMIZE_ICON =
  '<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 3H5a2 2 0 00-2 2v3m0 8v3a2 2 0 002 2h3m8-18h3a2 2 0 012 2v3m0 8v3a2 2 0 01-2 2h-3" /></svg>';
const RESTORE_ICON =
  '<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 9H5m0 0V5m0 4l5-5m5 5h4m0 0V5m0 4l-5-5M9 15H5m0 0v4m0-4l5 5m5-5h4m0 0v4m0-4l-5 5" /></svg>';

window.emanote = window.emanote || {};

let enabled = readStored(window.emanote.noteFocus?.enabled === true);

function readStored(fallback) {
  try {
    const stored = sessionStorage.getItem(STORAGE_KEY);
    return stored === null ? fallback : stored === 'true';
  } catch (_) {
    return fallback;
  }
}

function persist() {
  try {
    sessionStorage.setItem(STORAGE_KEY, enabled ? 'true' : 'false');
  } catch (_) {}
}

function setEnabled(next) {
  enabled = Boolean(next);
  persist();
  apply();
}

function toggle() {
  setEnabled(!enabled);
}

function apply() {
  document.documentElement.classList.toggle(ROOT_CLASS, enabled);
  document.querySelectorAll(BUTTON_SELECTOR).forEach(updateButton);
}

function updateButton(button) {
  const label = enabled
    ? text('restoreNoteArea', 'Restore note layout')
    : text('maximizeNoteArea', 'Maximize note area');

  button.innerHTML = enabled ? RESTORE_ICON : MAXIMIZE_ICON;
  button.setAttribute('aria-label', label);
  button.setAttribute('title', label);
  button.setAttribute('aria-pressed', enabled ? 'true' : 'false');
}

document.addEventListener('click', (event) => {
  const target = event.target instanceof Element ? event.target : null;
  const button = target?.closest(BUTTON_SELECTOR);
  if (!button) return;

  event.preventDefault();
  toggle();
});

window.emanote.noteFocus = {
  get enabled() {
    return enabled;
  },
  setEnabled,
  toggle,
  apply,
};

ready(apply);
onMorph(() => requestAnimationFrame(apply));
