// Theme toggle: persists choice in localStorage. The early-load script in
// base.tpl applies the stored choice before first paint to avoid FOUC; this
// module owns the runtime toggle invoked by the theme button.
//
// Exposed as window.emanote.theme.toggle so the inline onclick attributes
// in sidebar.tpl and layouts/default.tpl keep working without a refactor.
// Keep the storage key in sync with base.tpl's FOUC script.
const STORAGE_KEY = 'emanote-theme';

window.emanote = window.emanote || {};
window.emanote.theme = {
  toggle() {
    const root = document.documentElement;
    const isDark = root.classList.toggle('dark');
    root.style.colorScheme = isDark ? 'dark' : 'light';
    try {
      localStorage.setItem(STORAGE_KEY, isDark ? 'dark' : 'light');
    } catch (e) {}
    // Mermaid reads system colour-scheme at init; reload so any diagrams re-render.
    if (document.querySelector('.mermaid')) window.location.reload();
  },
};
