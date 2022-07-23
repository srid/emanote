# Syntax Highlighting

In order to enable syntax highlighting, you must use a client-side JavaScript highlighter, such as [PrismJS](https://prismjs.com/), and add it to `page.headHtml` of [[yaml-config]] (if adding to all or multiple routes) or Markdown frontmatter (if adding to a single route):

```yaml
page:
  headHtml: |
    <link href="https://cdnjs.cloudflare.com/ajax/libs/prism/1.28.0/themes/prism-tomorrow.min.css" rel="stylesheet" />
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.28.0/prism.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.28.0/plugins/autoloader/prism-autoloader.min.js"></script>

# Alternatively use the shorthand snippet alias instead:
page:
  headHtml: |
    <snippet var="js.prism" />
```

:::{.sticky-note .text-lg}
**Note:** If using PrismJS together with [[mermaid]] then you need to use the `<snippet var="js.prism" />`[^js.prism-source] alias as it contains additional code to prevent PrismJS from interfering with Mermaid.
:::

[^js.prism-source]: Source code for the `<snippet var="js.prism" />` alias can be found in the <https://github.com/EmaApps/emanote/blob/master/default/index.yaml> file, under the `js:` YAML map

An alias for [highlight.js](https://highlightjs.org/) also exists, which can be useful especially if PrismJS and Mermaid keep on being troublesome together. The highlight.js package works better with Mermaid out of the box compared to PrismJS.

```yaml
page:
  headHtml: |
    <snippet var="js.highlightjs" />
```

Bear in mind that when using highlight.js you must manually add language support. Prism.js in contrast provides an autoload feature.

## Example using PrismJS

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

