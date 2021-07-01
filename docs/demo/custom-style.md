---
tags: [emanote/syntax/demo]
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

### hightlight-inline

You can highlight ==inline text== as well.

## Advanced styling 

Using [fenced_divs](https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/fenced_divs.md) with [attributes](https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/attributes.md) extension, you can wrap parts of your Markdown using a [div], and then style it en masse. For example, to [[embed|embed multiple notes]] in a "matrix" arrangement[^mob] you can make use of CSS grids as provided by Tailwind's classes. 

[div]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div

:::{class="grid grid-flow-row grid-cols-1 gap-0 p-3 bg-gray-500 lg:grid-cols-2"}
![[examples]]

![[start]]

![[file-links]]
:::


[^mob]: If you are viewing this page on mobile or smaller screens, the embedded notes will be stacked on top of one another because we use Tailwind's [responsive classes](https://tailwindcss.com/docs/responsive-design). Incidentally, we use the `{class=".."}` syntax, rather than the `{.someClass}` syntax, only because the former is [more lenient](https://github.com/jgm/commonmark-hs/issues/76) in accept non-standard class names, such as the Tailwind responsive classes (ie. `lg:grid-cols-2`).