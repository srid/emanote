// Note focus mode: expand the central note area without entering browser
// fullscreen. State lives per tab and is re-applied after Ema morphs the DOM.

import { ready, onMorph } from '@emanote/morph';
import { text } from '@emanote/i18n';

const STORAGE_KEY = 'emanote-note-focus';
const ROOT_CLASS = 'emanote-note-focus';
const BUTTON_SELECTOR = 'button[data-emanote-note-focus-toggle]';

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

  const maximizeIcon = button.querySelector('[data-emanote-note-focus-icon="maximize"]');
  const restoreIcon = button.querySelector('[data-emanote-note-focus-icon="restore"]');
  if (maximizeIcon) maximizeIcon.hidden = enabled;
  if (restoreIcon) restoreIcon.hidden = !enabled;
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
