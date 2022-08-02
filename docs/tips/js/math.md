---
page:
  headHtml: |
    <snippet var="js.prism" />
    <snippet var="js.mathjax" />
    <snippet var="js.stork-search" />

---

# Math

[MathJax](https://www.mathjax.org) can be used to render Math formulas.  For example, $a^2 + b ^ 2 = c$.

1. Add the following to your `page.headHtml`, either in frontmatter or `index.yaml` (see [[yaml-config]])
    ```yaml
    page:
      headHtml: |
        <script>
          window.MathJax = {
            startup: {
              ready: () => {
                MathJax.startup.defaultReady();
              }
            }
          };
        </script>
        <script async="" id="MathJax-script" src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
    ```

## Demo

When $a \ne 0$, there are two solutions to $ax^2 + bx + c = 0$ and they are
$$x = {-b \pm \sqrt{b^2-4ac} \over 2a}.$$
