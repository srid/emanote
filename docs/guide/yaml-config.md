---
order: -97
template:
  theme: green
pandoc:
  rewriteClass:
    greenery: bg-green-100 text-green-700 font-bold text-3xl
tags: [emanote/yaml/demo]
---

# YAML configuration

Configure your site metadata, rendering configuration and such using YAML configuration. Create a `foo.yaml` alongside `foo.md` or `foo/` folder, and those settings apply only to that route. The YAML structure is the same as your Markdown frontmatter, and vice-versa. Settings in the YAML frontmatter apply onto that Markdown route only; whereas settings in an individual .yaml file apply to that entire sub-route tree. Emanote does a deep-merge of the parent YAML configurations, so you can have children override only what's necessary. This is sometimes known as ["data cascade"](https://www.11ty.dev/docs/data-cascade/). The final merged YAML structure is passed to the HTML templates, of which you have full rendering control over.

Notice how this page's sidebar colorscheme has [changed to green]{.greenery}? View [the source of this page](https://github.com/srid/emanote/blob/master/docs/guide/yaml-config.md) to see the magic involved. That CSS greenery you just saw too comes from YAML.

## Examples

- https://github.com/srid/emanote-template/blob/master/index.yaml
