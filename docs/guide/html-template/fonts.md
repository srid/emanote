---
slug: fonts
---

# Fonts and Typography

Emanote uses a **system font stack** as the default font family throughout the site. This provides excellent performance and native appearance across all platforms without requiring any font downloads. If you want to customize the typography:

## Changing the Font Family

To use a different font, create a `templates/styles.tpl` file in your notebook and override the global font styling (in [[layer|default layer]]):

```html
<style data-category="global-font">
  /* Replace system fonts with your preferred font */
  body {
    font-family: 'Your Font Name', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
  }
</style>
```

## Using Google Fonts {#google}

Create `templates/styles.tpl` with both the font import and the styling:

```html
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">

<style data-category="global-font">
  body {
    font-family: 'Inter', sans-serif;
  }
</style>
```

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

  body {
    font-family: 'YourFont', sans-serif;
  }
</style>
```
