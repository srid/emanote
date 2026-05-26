---
slug: custom-style
tags: [emanote/syntax/demo]
date: 2022-01-01
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

## Customizing Default Styling and Templates

Emanote allows you to customize the appearance of HTML components without the need for
manual styling each time. This can be accomplished by overriding default templates in your
notebook.

To start customizing, create a templates directory in your notebook. From there, you can
override any templates you wish by copying them from Emanote's default templates into your
notebook's templates directory. For example, if you want to customize the default pandoc
styling, you can copy the [pandoc.tpl](https://github.com/srid/emanote/blob/master/emanote/default/templates/components/pandoc.tpl)
file from Emanote's GitHub repository into your templates/components directory and edit it
accordingly.

> [!info] How it works
> This customization process works through a "union" of the default layer (provided by Emanote) and your notebook's layer. Essentially, it's similar to the `unionfs` concept - both the default layer and your notebook are union-mounted in Haskell using [srid/unionmount](https://github.com/srid/unionmount). This way, you only need to copy and modify the specific files you want to override, without affecting the rest of the default templates.

Several users have successfully implemented this customization approach in their projects.
Refer to the following examples for inspiration:

- [srid/srid](https://github.com/srid/srid/tree/master/templates)
- [TheNeikos/hemera.systems](https://github.com/TheNeikos/hemera.systems/tree/master/content/templates)
- [gil0mendes](https://gitlab.com/gil0mendes/website/-/tree/live/content/templates)
- [ChenghaoMou/chenghaomou](https://github.com/ChenghaoMou/chenghaomou.github.io/tree/master/templates)

For additional information and discussion on this topic, check out
[this discussion on GitHub](https://github.com/srid/emanote/discussions/438).

[^mob]: If you are viewing this page on mobile or smaller screens, the embedded notes will be stacked on top of one another because we use Tailwind's [responsive classes](https://tailwindcss.com/docs/responsive-design). Incidentally, we use the `{class=".."}` syntax, rather than the `{.someClass}` syntax, only because the former is [more lenient](https://github.com/jgm/commonmark-hs/issues/76) in accepting non-standard class names, such as the Tailwind responsive classes (eg. `lg:grid-cols-2`).

