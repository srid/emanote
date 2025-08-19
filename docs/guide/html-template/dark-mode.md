---
slug: dark-mode
---

# Dark Mode

Emanote includes built-in dark mode support using Tailwind CSS. The dark mode feature automatically detects and follows your system's color scheme preference.

## Features

- **Automatic Detection**: Respects your system's dark mode preference by default
- **Tailwind Integration**: Uses Tailwind CSS's `prefers-color-scheme` media query strategy
- **Comprehensive Coverage**: All page elements adapt to dark mode including content, navigation, and UI components

## Controlling Dark Mode

Dark mode automatically follows your system's color scheme preference. To manually toggle dark mode, we recommend using a browser extension like:

- **Chrome**: [Dark Mode Toggle](https://chromewebstore.google.com/detail/chrome-dark-mode-toggle/idnbggfpadjhjicgjmhlpeilafaplnhd?hl=en)
  - Note: You may need to enable the "Experimental Web Platform Features" flag at `chrome://flags` for the extension to work properly
- **Firefox**: Similar extensions available in Firefox Add-ons

These extensions override the system preference and allow you to toggle dark mode for any website.

## Customization

### Adding Dark Mode Classes

When customizing templates, use Tailwind's `dark:` prefix to add dark mode variants:

```html
<div class="bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100">
  Content that adapts to dark mode
</div>
```

### CSS Custom Styles

For custom CSS, target the `.dark` class that gets added to the HTML root:

```css
/* Light mode styles */
.my-element {
  background-color: #ffffff;
  color: #1f2937;
}

/* Dark mode styles */
.dark .my-element {
  background-color: #1f2937;
  color: #f3f4f6;
}
```

## Technical Implementation

The dark mode functionality is implemented through:

1. **Media Query Detection**: Uses `@media (prefers-color-scheme: dark)` to detect system preference
2. **Tailwind CSS**: Uses Tailwind's `dark:` prefix utilities that respond to the media query
3. **Automatic Switching**: No JavaScript required - works purely with CSS media queries