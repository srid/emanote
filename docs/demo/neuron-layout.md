---
template:
  name: /templates/layouts/note
page:
  headHtml: |
    <snippet var="js.prism" />
    <snippet var="js.stork-search-note" />
---

# Neuron-like layout

Emanote comes with two built-in template layouts. The default layout is called "book", but you may select the "note" layout (which mimics [Neuron](https://neuron.zettel.page/)) by adding the following to your [[yaml-config|index.yaml]],

```yml
template:
  name: /templates/layouts/note
```

The "note" layout includes the [[uptree]] of Neuron based on folgezettel links as well as directory layout. Note that all top-level notes are automatically made a folgezettel branch of the root note (index), such that the "home" link appears on top in the uplink tree of all notes. As a demo, the page you are viewing is rendered using the "note" layout (whilst leaving the rest of the site to use the default "book" layout.)
