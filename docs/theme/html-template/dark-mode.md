---
slug: dark-mode
---

# Dark Mode

Emanote ships with a manual light/dark theme toggle. The user's choice is persisted in `localStorage` and applied before first paint, so there's no flash of the wrong theme on reload. If the user has never toggled, the site falls back to the OS `prefers-color-scheme` preference.

## How it works

- The toggle button lives in the sidebar (next to the search icon), and also in the top-right corner of the no-sidebar "neuron-style" layout.
- Clicking the toggle flips a `.dark` class on `<html>` and stores the choice in `localStorage` under the key `emanote-theme`.
- An inline `<script>` in `base.tpl` runs in the document head before any stylesheets, reads `localStorage` (falling back to `prefers-color-scheme`), and adds the `.dark` class immediately — this prevents the dark-flash-on-light-reload problem that plagues most naïve implementations.
- All theme-dependent styling — including the stork search dialog and skylighting syntax highlighting — keys off the `.dark` class rather than the `prefers-color-scheme` media query, so everything tracks the manual toggle.

## Customization

### Adding Dark Mode Classes

When customizing templates, use Tailwind's `dark:` variant:

```html
<div class="bg-white dark:bg-gray-900 text-gray-900 dark:text-gray-100">
  Content that adapts to dark mode
</div>
```

### CSS Custom Styles

For custom CSS, target the `.dark` class that gets added to the HTML root:

```css
.my-element {
  background-color: #ffffff;
  color: #1f2937;
}

.dark .my-element {
  background-color: #1f2937;
  color: #f3f4f6;
}
```

Avoid `@media (prefers-color-scheme: dark)` in new code — it won't respond to the manual toggle. If you're porting CSS from elsewhere that uses the media query, convert it to a `.dark` class selector.

### Mermaid diagrams

Mermaid reads the active colour scheme at initialization, so toggling the theme triggers a full-page reload when a Mermaid diagram is present — just enough to re-initialize the renderer with the new palette.
