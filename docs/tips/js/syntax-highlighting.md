---
page:
  headHtml: |
    <snippet var="js.highlightjs" />

---
# Syntax Highlighting

In order to enable syntax highlighting, you must use a client-side JavaScript highlighter, such as [highlight.js](https://highlightjs.org/) by adding it to `page.headHtml` of [[yaml-config|YAML configuration]] or Markdown frontmatter. Emanote already provides a snippet, so you may directly include the following in your `index.yaml` (assuming you are enabling it on all routes):

```yaml
page:
  headHtml: |
    <snippet var="js.highlightjs" />
```

> [!warning] 
> Bear in mind that when using highlight.js you must manually add language support. The above snippet includes Haskell and [Nix](https://nixos.asia) by default; otherwise, it is normally added as:
>
> ```yaml
> page:
>   headHtml: |
>     <snippet var="js.highlightjs" />
>     <with var="js">
>     <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/${value:highlightjs-ver}/languages/haskell.min.js"></script>
>     </with>
> ```
> 
> (The `highlightjs-ver` variable comes from the default [`index.yaml`](https://github.com/srid/emanote/blob/master/emanote/default/index.yaml).)

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

## Prism

A predefined snippet also exists for another syntax highlighter called [Prism](https://prismjs.com/). To use it add the following to `page.headHtml` of [[yaml-config|YAML configuration]] or Markdown frontmatter.

```yaml
page:
  headHtml: |
    <snippet var="js.prism" />
```

> [!warning] Prism does not cooperate well with Emanote's live preview mode.