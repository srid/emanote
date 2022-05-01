# Syntax Highlighting

In order to enable syntax highlighting, you must use a client-side JavaScript highlighter, such as [PrismJS](https://prismjs.com/), and add it to `page.headHtml` of [[yaml-config]] (if adding to all or multiple routes) or Markdown frontmatter (if adding to a single route):

```yaml
page:
  headHtml: |
    <link href="https://cdn.jsdelivr.net/npm/prismjs@1.23.0/themes/prism-tomorrow.css" rel="stylesheet" />
    <script src="https://cdn.jsdelivr.net/combine/npm/prismjs@1.23.0/prism.min.js,npm/prismjs@1.23.0/plugins/autoloader/prism-autoloader.min.js"></script>
```

Or, using the alias from the default layer's `index.yaml`:

```yaml
page:
  headHtml: |
    <snippet var="js.prism" />
```

An alias for highlight.js also exists, especially as highlight.js works better with [[mermaid]] than PrismJS:

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

