---
slug: templates
---

# HTML Templates

Unlike [[neuron|Neuron]], Emanote's generated HTML can be **completely** customized. Emanote uses the Heist HTML template system that supports non-trivial constructs like recursion; see [here](http://snapframework.com/docs/tutorials/heist) for a tutorial on its syntax. 

The default HTML templates are stored in the [[layer|default layer]], which can you override either totally or in subset. 

## Default Layout

Emanote includes a default layout that includes a [[sidebar]], but can be customized to mimic [[neuron-layout]]. You may also write your own HTML layout from scratch, as long as you specify that template in [[yaml-config]] for the notes in question. For eg., [`templates/home.tpl`](https://github.com/srid/srid/blob/master/templates/home.tpl) is how https://srid.ca homepage is generated, because its [`index.md`](https://raw.githubusercontent.com/srid/srid/master/index.md) specifies this template as `template.name` in its YAML frontmatter (which could also be `index.yaml`).

## Features

```query
children:.
```
