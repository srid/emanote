---
slug: callout
---

# Callouts

Emanote supports [Obsidian-style callouts](https://help.obsidian.md/Editing+and+formatting/Callouts).[^callout] 

[^callout]: Not all of Obsidian spec may yet be supported. See https://github.com/srid/emanote/issues/465 for details.

## Demo

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
> A **PCE** is when one’s sense of _identity_ temporarily vacates the throne and apperception occurs. [Apperception](https://www.actualfreedom.com.au/sundry/frequentquestions/FAQ38.htm) is the mind’s perception of itself … it is a pure awareness.

Callouts also work with [[orgmode]] syntax.

## Custom callouts {#custom}

To add a new custom callout named `foo` (viz.: `[!foo] ...`), create a `/templates/filters/callout/foo.tpl` file in your [[html-template|templates]] folder. You can also change the layout and styling of existing callout types in [`/templates/filters/callout/*.tpl`](https://github.com/srid/emanote/tree/master/emanote/default/templates/filters/callout).
