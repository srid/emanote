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

## Customizing callouts {#custom}

To customize their structure and styling, change `callout.tpl` (and `base.tpl`) in [[html-template|HTML templates]]. Want to add additional callout types to Emanote? See [this PR](https://github.com/srid/emanote/pull/571) as example.