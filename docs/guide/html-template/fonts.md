---
slug: fonts
---

# Fonts and Typography

Emanote uses **Maven Pro** as the default font family throughout the site. The font files are self-hosted in the default layer to keep sites self-contained. If you want to customize the typography:

## Changing the Font Family

To use a different font, create a `templates/styles.tpl` file in your notebook and override the global font styling:

```html
<style data-category="global-font">
  /* Replace Maven Pro with your preferred font */
  body {
    font-family: 'Your Font Name', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
  }
</style>
```

## Using Google Fonts

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

## Self-hosting Custom Fonts

To self-host your own fonts like Maven Pro:

1. Create a `_emanote-static/fonts/` directory in your notebook
2. Add your font files (`.ttf`, `.woff2`, etc.)
3. Create a CSS file with `@font-face` declarations
4. Import it in your `templates/styles.tpl`

See the [Maven Pro font setup](https://github.com/srid/emanote/tree/master/emanote/default/_emanote-static/fonts) in the default layer for reference.
