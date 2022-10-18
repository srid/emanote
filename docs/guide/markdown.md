---
order: -100
page:
  description: "Various non-standard Markdown features supported by Emanote"
---

# Markdown :writing_hand:

Emanote notes are primarily written in **Markdown** format, but [[orgmode]] is also supported in a basic form. A tutorial is [available here](https://commonmark.org/help/tutorial/). Below we shall highlight some of the commonmark extensions that Emanote supports on top of standard Mardown syntax.

## Wiki Links

You can link to a note by placing the filename (without extension) inside double square brackets. For example, `[[neuron]]` links to the file `neuron.md` and it will be rendered as [[neuron]]. Note that it is using the title of the note automatically;
you can specify a custom title as `[[neuron|Moving off neuron]]` which renders as [[neuron|Moving off neuron]] or even force use of filename with `[[neuron|neuron]]` which renders as [[neuron|neuron]].

Broken links render differently, for example: [[Foo bar]] (if a wiki-link) or [Foo bar](foo-bar.md) (if a regular Markdown link).

### Anchors 

Wiki-links [do not yet](https://github.com/srid/emanote/discussions/105) support anchor links, but they work for regular links ([example link](./markdown.md#lists)).

## Emojis

:smile:

:runner: :ant:

See [list of available emojis](https://gist.github.com/rxaviers/7360908) for reference.

## Footnotes

https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/footnotes.md

Demo: Checkout this note[^1] and this other note[^2] as both are footnotes. You may also reuse[^1] footnotes.

[^1]: First footnote example
[^2]: Second footnote example. Footnotes *within*[^1] footnotes are not handled.


## Task lists

- [x] A task that was done
- [ ] A task that is to be done.
- [ ] Task with *Markdown* and links (eg: [[lua-filters]])
- A list item with no task marker

Tasks can also be written outside of list context, such as paragraphs:

[ ] This is a task on its own paragraph.

[x] Here we have the next paragraph.

Unchecked tasks will appear in the task index available at [/-/tasks](-/tasks).

## Definition lists

https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/definition_lists.md

Fruits
: Apples
: Oranges

Animal Foods
: Eggs
: Diary
: Offal
: Muscle meat

## Lists

Simple lists,

- Apple
- Orange
- Mango

Lists with sub-lists,

- Muscle meat
- Offal
  - Liver
  - Heart
- Misc
  - Bone Marrow
  - Cartillage
  - Skin

List items can contain multiple block elements (eg: paragraph),

- [Meat](https://www.diagnosisdiet.com/full-article/meat) is the only nutritionally complete food
- Animal foods contain all of the protein, fat, vitamins and minerals that humans need to function.

  They contain absolutely everything we need in just the right proportions.
- In contrast to vegetables, meat does not contain any “anti-nutrients”

Ordered lists,

1. Be happy
1. Be harmless
1. Be naive

## Tables

| Category      | Favourite      |
| ------------- | -------------- |
| Web Browser   | [Brave]        |
| Search Engine | [Brave Search] |
| Chat          | [Element]      |

(Note that wiki links with a custom text must have their [pipe escaped](https://github.com/srid/emanote/issues/113#issuecomment-894808721) when used inside tables.)

[Brave]: https://brave.com/
[Brave Search]: https://search.brave.com/
[Element]: https://element.io/

## Hash Tags

Add Twitter-like hashtags anywhere in Markdown file. They can also be added to the YAML frontmatter. Hash tags can also be "hierarchical", for instance: #emanote/syntax/demo

## Highlighting

You can highlight any ==inline text== by wraping them in `==` (ie. `==inline text==`).[^prop] The CSS style for highlighted inlines can be specified in [[custom-style|index.yaml]]. Regular Markdown syntax, including emojis, can be mixed in with highlighted inlines to ==🍓 give a **distinction** on top== of it all.

[^prop]: See original proposal for this syntax [here](https://talk.commonmark.org/t/highlighting-text-with-the-mark-element/840).

## More extensions

:::{.flex-row .space-y-8}
![[syntax-highlighting]]

[[mermaid]]
:::
