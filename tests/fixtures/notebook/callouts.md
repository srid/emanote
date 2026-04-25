# Callouts fixture

This fixture exercises the four callout shapes the renderer must distinguish:
plain (non-foldable), foldable+expanded, foldable+collapsed, and nested.

Each top-level callout uses a distinct type so step definitions can target
it via `[data-callout="<type>"]` without ambiguity.

> [!info] Plain
> Plain non-foldable body. Marker: PLAIN_BODY

> [!tip]+ Foldable expanded
> Foldable initially-expanded body. Marker: EXPANDED_BODY

> [!warning]- Foldable collapsed
> Foldable initially-collapsed body. Marker: COLLAPSED_BODY

> [!note] Outer
> Outer note body before nested. Marker: OUTER_BODY
>
> > [!quote] Inner
> > Inner quote body. Marker: NESTED_INNER_BODY
