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

## Advanced styling 

Using [fenced_divs extension](https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/fenced_divs.md), you can wrap parts of your Markdown using a [div], and then style it en masse. For example, to [[embed|embed multiple notes]] in a "matrix" arrangement you can make use of CSS grids as provided by Tailwind's classes. 

[div]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div

:::{.grid .grid-cols-2 .grid-flow-row .gap-0 .p-3 .bg-gray-500}
![[examples]]

![[start]]

![[file-links]]
:::


