---
slug: math
---

# Math

Emanote renders `$...$` and `$$...$$` to **MathML at build time** by default via [`texmath`](https://hackage.haskell.org/package/texmath). Modern browsers (Firefox, Safari, Chrome ≥109) render MathML natively, so the page ships no math JS bundle.

### Demo

When $a \ne 0$, there are two solutions to $ax^2 + bx + c = 0$ and they are
$$x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}.$$

## Opting out

If you prefer KaTeX's typography or need to support a very old browser, disable static rendering in your site's `index.yaml`:

```yaml
emanote:
  staticMath: false
```

Then enable a client-side renderer per page (or globally via `page.headHtml` in your root `index.yaml`).

### MathJax

```yaml
page:
  headHtml: |
    <snippet var="js.mathjax" />
```

The `js.mathjax` snippet is shipped in the default config.

### KaTeX

Paste the KaTeX loader directly into `page.headHtml` — Emanote's default config no longer defines a `js.katex` snippet:

```yaml
page:
  headHtml: |
    <link rel="stylesheet"
          href="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css"
          crossorigin="anonymous">
    <script defer
            src="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js"
            crossorigin="anonymous"></script>
    <script defer
            src="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/contrib/auto-render.min.js"
            crossorigin="anonymous"
            onload="renderMathInElement(document.body);"></script>
```

