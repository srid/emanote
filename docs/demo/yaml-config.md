---
template:
  theme: green
pandoc:
  rewriteClass:
    greenery: bg-green-100 text-green-700 font-bold text-3xl
---

# YAML configuration

Configure your site metadata, rendering configuration and such using YAML configuration. Create a `foo.yaml` alongside `foo.md` or `foo/` folder, and those settings apply only to that node. The YAML structure is the same as your Markdown frontmatter, and vice-versa. Settings in the YAML frontmatter apply onto that Markdown; whereas settings in an individual .yaml file apply to that entire sub-route. Emanote does a deep-merge of the parent YAML configurations, so you can have children override only what's necessary. 

Notice how this page's sidebar colorscheme has [changed to green]{.greenery}? View the source of this file to see the magic involved. That CSS greenery you just saw too comes from YAML.

For another example, notice how the demo section of this website has a different base site title (see `demo.yaml` for the underlying configuration).