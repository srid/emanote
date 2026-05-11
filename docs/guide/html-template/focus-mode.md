---
slug: focus-mode
---

# Focus mode

The default [[html-template]] includes a note focus button at the top of the note area. It expands the central note column within the current browser window by hiding side chrome:

- [[sidebar]]
- [[right-panel]]
- the narrow-screen bottom strip that mirrors right-panel [[backlinks]] and timeline content
- [[uptree]]

Focus mode does not use the browser fullscreen API and does not maximize the browser window itself. In live preview, Ema hot reloads keep the note focused after the page DOM is patched. The layout is restored only when you click the same note focus button again.
