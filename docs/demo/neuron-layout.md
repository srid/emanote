---
template:
  name: /templates/layouts/note

page:
  headHtml: |
    <link href="https://cdn.jsdelivr.net/npm/prismjs@1.23.0/themes/prism-tomorrow.css" rel="stylesheet" />
    <script src="https://cdn.jsdelivr.net/combine/npm/prismjs@1.23.0/prism.min.js,npm/prismjs@1.23.0/plugins/autoloader/prism-autoloader.min.js"></script>
---

# Neuron-like layout

Emanote comes with two built-in template layouts. The default layout is called "book", but you may select the "note" layout (which mimics [Neuron](https://neuron.zettel.page/)) by adding the following to your [[yaml-config|index.yaml]],

```yml
template:
  name: /templates/layouts/note
```

The "note" layout includes the [uplink tree of Neuron](https://neuron.zettel.page/uplink-tree) based on folgezettel links as well as directory layout. As a demo, the page you are viewing is rendered using the "note" layout (whilst leaving the rest of the site to use the default "book" layout.)
