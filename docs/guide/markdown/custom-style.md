---
tags: [emanote/syntax/demo]

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
---

# Custom CSS styling

Parts of your Markdown may be styled using custom CSS classes provided by TailwindCSS.

The [attributes extension](https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/attributes.md) provides the ability to set CSS classes on inline or block level elements of Markdown. You can also specify a "class map" in [[yaml-config|index.yaml]], the default value of which provides some builtin-in styles.

## Built-in styles

Emanote provides some built-in styles.

### sticky-note

:::{.sticky-note}
**Rapidly build modern websites without ever leaving your HTML.**

Tailwind CSS is a highly customizable, low-level CSS framework that gives you
all of the *building blocks* you need to build bespoke designs without any
annoying opinionated styles you have to fight to override.
:::

You should expect the above text to appear styled like a yellow sticky note, because the default [[yaml-config|index.yaml]] specifies a "sticky-note" class, which rewrites to a list of Tailwind classes, and that class in turn is (re)used in Markdown notes.

### highlight-block

:::{.highlight-block}
A portion of Markdown that is highlighted compared to the rest
:::

## Advanced styling

Using [fenced_divs](https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/fenced_divs.md) with [attributes](https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/attributes.md) extension, you can wrap parts of your Markdown using a [div], and then style it en masse. For example, to [[embed|embed multiple notes]] in a "matrix" arrangement[^mob] you can make use of CSS grids as provided by Tailwind's classes.

[div]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div

:::{class="grid grid-flow-row grid-cols-1 gap-0 p-3 bg-gray-500 lg:grid-cols-2"}
![[examples]]

![[start]]

![[file-links]]
:::


[^mob]: If you are viewing this page on mobile or smaller screens, the embedded notes will be stacked on top of one another because we use Tailwind's [responsive classes](https://tailwindcss.com/docs/responsive-design). Incidentally, we use the `{class=".."}` syntax, rather than the `{.someClass}` syntax, only because the former is [more lenient](https://github.com/jgm/commonmark-hs/issues/76) in accepting non-standard class names, such as the Tailwind responsive classes (eg. `lg:grid-cols-2`).

## External link icons

A link whose address begins with an [URI scheme](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier#Syntax) component is considered an external link. Emanote displays an icon next to an external hyperlink if its description contains some text, including inline code and math formulas. (A common non-example is a hyperlink containing only an image in the description.) The heuristic is intended to make the site look good in most cases. This behaviour can be overriden by setting the value of the `data-linkicon` attribute of a link:
  * use `{data-linkicon=external}` in order to force displaying the icon next to the link;
  * use `{data-linkicon=none}` or `{data-linkicon=""}` to prevent displaying the icon next to the link.

Note that the attribute can also be used to display the icons in the template parts of the website (like the footer or the sidebar) or in raw HTML parts of the document.

The displayed icon may depend on the link properties (e.g. the actual URI scheme). This is customized using CSS. By default, Emanote displays a different icon if the URI scheme component is `mailto:`. Check the [`default/templates/base.tpl`](https://github.com/EmaApps/emanote/blob/master/default/templates/base.tpl) template for details.

* No special styling:
  * [the emanote repo](https://github.com/EmaApps/emanote)
  * [why the external link symbol ![[external-link-icon.svg]] is not in Unicode](https://www.unicode.org/alloc/nonapprovals.html)
  * [$e^{i \pi} + 1 = 0$](https://en.wikipedia.org/wiki/Euler%27s_identity)
  * [`(>>=) :: forall a b. m a -> (a -> m b) -> m b`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:-62--62--61-)
  * [![[hello-badge.svg]]](https://emanote.srid.ca)
  * mother_hetchel@weaversguild.com
* `{data-linkicon=none}` used to suppress the icon:
  * Water's formula is [H](https://en.wikipedia.org/wiki/Hydrogen){data-linkicon=none}₂[O](https://en.wikipedia.org/wiki/Oxygen){data-linkicon=none}.
  * A 90's style hyperlink:
    :::{.center}
    [➡➡➡ **CLICK HERE!!!** ⬅⬅⬅](https://emanote.srid.ca){class="shadow-lg border-8 rounded-md bg-yellow-400 border-red-200" data-linkicon=none}
* `{data-linkicon=external}` used to show the icon
  * [![[pIqaD.svg]]](https://en.wikipedia.org/wiki/Klingon_scripts){data-linkicon=external}
