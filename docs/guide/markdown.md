---
order: -100
page:
  description: "Various non-standard Markdown features supported by Emanote"
date: 2021-01-01
---

# Markdown :writing_hand:

Emanote notes are primarily written in **Markdown** format, but [[orgmode]] is also supported in a basic form. A tutorial is [available here](https://commonmark.org/help/tutorial/). Below we shall highlight some of the commonmark extensions that Emanote supports on top of standard Mardown syntax.

{#wikilink}
## Wiki Links

You can link to a note by placing the filename (without extension) inside double square brackets. For example, `[[neuron]]` links to the file `neuron.md` and it will be rendered as [[neuron]]. Note that it is using the title of the note automatically;
you can specify a custom title as `[[neuron|Moving off neuron]]` which renders as [[neuron|Moving off neuron]] or even force use of filename with `[[neuron|neuron]]` which renders as [[neuron|neuron]].

### Structural links

See [[folgezettel]] for a special type of wiki-link used to define the [[sidebar]] (and [[uptree]]) heirarchy.

### Anchors 

Wiki-links [do not yet](https://github.com/srid/emanote/discussions/105) support anchor links, but they work for regular links ([example link](./markdown.md#lists)).

### Broken links

Broken links render differently, for example: [[Foo bar]] (if a wiki-link) or [Foo bar](foo-bar.md) (if a regular Markdown link).

### Ambiguous links

Ambiguous wiki-links are disambiguated by selecting the one that shares the closest ancestor.[^ambig]

[^ambig]: This particular selection process [was choosen](https://github.com/srid/emanote/pull/498) in particular to allow combining multiple notebooks (with similar note filenames) at the top-level.


## Emojis

:smile:

:runner: :ant:

See [list of available emojis](https://gist.github.com/rxaviers/7360908) for reference.

## Footnotes

https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/footnotes.md

Demo: Checkout this note[^1] and this other note[^2] as both are footnotes. You may also reuse[^1] footnotes.

[^1]: First footnote example
[^2]: Second footnote example. Footnotes *within*[^1] footnotes are not handled.


{#tasks}
## Task lists

- [x] A task that was done
- [ ] A task that is to be done.
- [ ] Task with *Markdown* and links (eg: [[lua-filters]])
- A list item with no task marker

Tasks can also be written outside of list context, such as paragraphs:

[ ] This is a task on its own paragraph.

[x] Here we have the next paragraph.

Unchecked tasks will appear in the task index available at [/-/tasks](-/tasks).

{#dl}
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

Add Twitter-like hashtags anywhere in Markdown file. They can also be added to the [[yaml-config|YAML frontmatter]]. Hash tags can also be "hierarchical", for instance: #emanote/syntax/demo

## Highlighting

You can highlight any ==inline text== by wraping them in `==` (ie. `==inline text==`).[^prop] The CSS style for highlighted inlines can be specified in [[custom-style|index.yaml]]. Regular Markdown syntax, including emojis, can be mixed in with highlighted inlines to ==🍓 give a **distinction** on top== of it all.

[^prop]: See original proposal for this syntax [here](https://talk.commonmark.org/t/highlighting-text-with-the-mark-element/840).

## Callouts

Emanote supports [Obsidian-style callouts](https://help.obsidian.md/Editing+and+formatting/Callouts).[^callout] To customize their structure and styling, change `callout.tpl` (and `base.tpl`) in [[html-template|HTML templates]].

[^callout]: Not all of Obsidian spec may yet be supported. See https://github.com/srid/emanote/issues/465 for details.

> [!note]
> This is a note callout
> 
> Lorem **ipsum** dolor sit *amet*, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

> [!info]
> This is an info callout
> 
> Lorem **ipsum** dolor sit *amet*, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

> [!tip] Callouts can have *custom* titles
> Like this one.
>
> Lorem **ipsum** dolor sit *amet*, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

> [!warning]
>
> Lorem **ipsum** dolor sit *amet*, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

> [!failure]
>
> Lorem **ipsum** dolor sit *amet*, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

> [!quote]
>
> Lorem **ipsum** dolor sit *amet*, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

> [!quote] Richard [said](https://www.actualfreedom.com.au/library/topics/pce.htm),
>
> A PCE is when one’s sense of identity temporarily vacates the throne and apperception occurs. [Apperception](https://www.actualfreedom.com.au/sundry/frequentquestions/FAQ38.htm) is the mind’s perception of itself … it is a pure awareness.

Callouts also work with [[orgmode]] syntax.

{#hanchor}
## Heading anchors

You can use the following syntax to override the default heading anchors:

```markdown
{#head}
## Some heading
```

On default theme, an anchor is displayed when you hover on the heading allowing you to copy the link to the heading. Here are all heading levels for comparison:

### Level 3

#### Level 4

##### Level 5

###### Level 6

## More extensions

:::{.flex-row .space-y-8}
![[syntax-highlighting]]

[[mermaid]]
:::
