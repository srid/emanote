---
short-title: JavaScript
slug: js
---

# JavaScript behaviours

Improve your Emanote website using custom JavaScript code. Emanote provides some predefined behaviours, like syntax highlighting or rendering of mathematical formulas. They can be accessed by including appropriate snippets in `page.headHtml` or `page.bodyHtml` of [[yaml-config|YAML configuration files]] (if adding to all or multiple routes) or Markdown frontmatter (if adding to a single route). The source code for the snippets can be found in the default [`index.yaml`](https://github.com/srid/emanote/blob/master/emanote/default/index.yaml) under the `js:` YAML map.

```query
path:./*
```
