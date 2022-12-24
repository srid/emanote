---
page:
  headHtml: |
    <snippet var="js.highlightjs" />

---
# Syntax Highlighting

In order to enable syntax highlighting, you must use a client-side JavaScript highlighter, such as [highlight.js](https://highlightjs.org/) by adding it to `page.headHtml` of [[yaml-config]] (if adding to all or multiple routes) or Markdown frontmatter (if adding to a single route). Emanote already provides a snippet, so you may directly include the following in your `index.yaml` (assuming you are enabling it on all routes):

```yaml
page:
  headHtml: |
    <snippet var="js.highlightjs" />
```

(Source code for the `<snippet var="js.highlightjs" />` alias can be found in the <https://github.com/srid/emanote/blob/master/default/index.yaml> file, under the `js:` YAML map)

Bear in mind that when using highlight.js you must manually add language support. The above snippet includes Haskell by default; otherwise, it is normally added as:


```yaml
page:
  headHtml: |
    <snippet var="js.highlightjs" />
    <with var="js">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/${value:highlightjs-ver}/languages/haskell.min.js"></script>
    </with>
```

(The `highlightjs-ver` variable also comes from the default `index.yaml`)

## Example (highlight.js)

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

