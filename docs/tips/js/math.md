---
page:
  headHtml: |
    <snippet var="js.highlightjs" />
    <snippet var="js.mathjax" />

---

# Math

[MathJax](https://www.mathjax.org) can be used to render Math formulas.  For example, $a^2 + b ^ 2 = c$.

To enable it, add the following to `page.headHtml` of [[yaml-config|YAML configuration]] or Markdown frontmatter.

```yaml
page:
  headHtml: |
    <snippet var="js.mathjax" />
```

[KaTeX](https://katex.org/) can be used as an alternative to MathJax. Just like MathJax, it renders math specified between dollar signs.

To enable it:

```yaml
page:
  headHtml: |
    <snippet var="js.katex" />
```

## Demo

When $a \ne 0$, there are two solutions to $ax^2 + bx + c = 0$ and they are
$$x = {-b \pm \sqrt{b^2-4ac} \over 2a}.$$
