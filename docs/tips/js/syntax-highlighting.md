# Syntax Highlighting

In order to enable syntax highlighting, you must use a client-side JavaScript highlighter, such as [PrismJS](https://prismjs.com/), and add it to `page.headHtml` of [[yaml-config]] (if adding to all or multiple routes) or Markdown frontmatter (if adding to a single route):

```yaml
page:
  headHtml: |
    <link href="https://cdnjs.cloudflare.com/ajax/libs/prism/1.28.0/themes/prism-tomorrow.min.css" rel="stylesheet" />
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.28.0/prism.min.js" data-manual></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.28.0/plugins/autoloader/prism-autoloader.min.js"></script>
    <script>
    for (var element of document.querySelectorAll('pre > code[class*="language"]:not(.language-mermaid)')) {
        Prism.highlightElement(element, false);
    }
    </script>
```

Or, using the alias from the default layer's `index.yaml`:

```yaml
page:
  headHtml: |
    <snippet var="js.prism" />
```

PrismJS will by default collide with [[mermaid]], but both above snippets uses the [filter-highlight-all](https://prismjs.com/plugins/filter-highlight-all/) PrismJS plugin avoid this collision.

An alias for [highlight.js](https://highlightjs.org/) also exists, which can be useful especially if PrismJS and Mermaid keep on being troublesome together. The highlight.js package works better with Mermaid out of the box.

```yaml
page:
  headHtml: |
    <snippet var="js.highlightjs" />
```

Bear in mind that when using highlight.js you must manually add language support. Prism.js in contrast provides an autoload feature.

## Example using PrismJS:

### Python

```python
def fib(n):
    a, b = 0, 1
    while a < n:
        print(a, end=' ')
        a, b = b, a+b
    print()
fib(1000)
```

### Haskell

```haskell
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

