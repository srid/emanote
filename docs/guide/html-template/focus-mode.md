---
slug: focus-mode
---

# Focus mode

The default [[html-template]] includes a note focus button at the top of the note area. It expands the central note column within the current browser window by hiding side chrome:

- [[sidebar]]
- [[right-panel]]
- the narrow-screen bottom strip that mirrors right-panel [[backlinks]] and timeline content

The button is shown only when the page renders side chrome that focus mode can collapse. It is hidden on narrow screens, and pages that only use [[uptree]] without sidebar or right-panel do not show it. Focus mode does not hide the uptree.

Focus mode does not use the browser fullscreen API and does not maximize the browser window itself. In live preview, Ema hot reloads keep the note focused after the page DOM is patched. The layout is restored only when you click the same note focus button again.
