---
slug: fonts
---

# Fonts and Typography

Emanote ships with three self-hosted fonts served out of the bundled static layer (no third-party fetch at page load):

- [Lora](https://fonts.google.com/specimen/Lora) — prose
- [Space Grotesk](https://fonts.google.com/specimen/Space+Grotesk) — UI chrome (sidebar, breadcrumbs, TOC, backlinks) and headings
- [Space Mono](https://fonts.google.com/specimen/Space+Mono) — inline code and code blocks

The woff2 files plus a generated `fonts.css` live under `_emanote-static/fonts/`, and `templates/styles.tpl` links them via `${ema:emanoteStaticLayerUrl}/fonts/fonts.css`. Generated static sites therefore work fully offline.

The theme colour (set via `template.theme` — see [[yaml-config|YAML configuration]]) shows up in the note title, wikilinks, TOC accents, and backlink cards rather than in full-bleed body backgrounds.

## Changing the Font Family

To use a different font, create a `templates/styles.tpl` file in your notebook and override the global font styling (in [[layer|default layer]]):

```html
<style data-category="global-font">
  :root {
    --font-serif: 'Your Serif', ui-serif, Georgia, serif;
    --font-sans: 'Your Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
    --font-mono: 'Your Mono', ui-monospace, SFMono-Regular, Menlo, monospace;
  }
</style>
```

The default theme reads these three variables — `--font-serif` for body prose, `--font-sans` for UI chrome, and `--font-mono` for code — so setting them is enough to swap fonts everywhere.

## Using Google Fonts {#google}

Create `templates/styles.tpl` with both the font import and the styling:

```html
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">

<style data-category="global-font">
  :root {
    --font-sans: 'Inter', sans-serif;
  }
</style>
```

Using Google Fonts means each page load fetches CSS and woff2 from `fonts.gstatic.com`. If you want your generated static site to work fully offline, self-host instead (below).

## Self-hosting Custom Fonts {#custom}

To self-host your own fonts:

1. Create a `_emanote-static/fonts/` directory in your notebook
2. Add your font files (`.ttf`, `.woff2`, etc.)
3. Create `templates/styles.tpl` with `@font-face` declarations:

```html
<style data-category="custom-fonts">
  @font-face {
    font-family: 'YourFont';
    src: url('${ema:emanoteStaticLayerUrl}/fonts/YourFont.woff2') format('woff2');
    font-weight: 400;
    font-style: normal;
  }

  :root {
    --font-sans: 'YourFont', sans-serif;
  }
</style>
```
