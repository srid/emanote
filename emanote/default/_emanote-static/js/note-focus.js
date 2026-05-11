// Note focus mode: expand the central note area without entering browser
// fullscreen. State lives per tab and is re-applied after Ema morphs the DOM.

import { ready, onMorph } from '@emanote/morph';
import { text } from '@emanote/i18n';

const STORAGE_KEY = 'emanote-note-focus';
const ROOT_CLASS = 'emanote-note-focus';
const BUTTON_SELECTOR = 'button[data-emanote-note-focus-toggle]';

window.emanote = window.emanote || {};

function readStored(fallback) {
  try {
    const stored = sessionStorage.getItem(STORAGE_KEY);
    return stored === null ? fallback : stored === 'true';
  } catch (err) {
    console.warn('[emanote] note focus preference not readable; using current page state', err);
    return fallback;
  }
}

function persist(next) {
  try {
    sessionStorage.setItem(STORAGE_KEY, next ? 'true' : 'false');
  } catch (err) {
    console.warn('[emanote] note focus preference not persisted; focus may reset on reload', err);
  }
}

function isEnabled() {
  return document.documentElement.classList.contains(ROOT_CLASS);
}

function setEnabled(next) {
  const enabled = Boolean(next);
  document.documentElement.classList.toggle(ROOT_CLASS, enabled);
  persist(enabled);
  updateButtons(enabled);
}

function toggle() {
  setEnabled(!isEnabled());
}

function apply() {
  const enabled = readStored(isEnabled());
  document.documentElement.classList.toggle(ROOT_CLASS, enabled);
  updateButtons(enabled);
}

function updateButtons(enabled) {
  document.querySelectorAll(BUTTON_SELECTOR).forEach((button) => updateButton(button, enabled));
}

function updateButton(button, enabled) {
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
    return isEnabled();
  },
  setEnabled,
  toggle,
  apply,
};

ready(apply);
onMorph(() => requestAnimationFrame(apply));
