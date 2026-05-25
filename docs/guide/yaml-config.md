---
slug: yaml-config
order: -97
template:
  theme: green
pandoc:
  rewriteClass:
    greenery: bg-green-100 text-green-700 font-bold text-3xl
tags: [emanote/yaml/demo]
---

# YAML configuration

Configure your site metadata, rendering configuration and such using YAML configuration. Create a `foo.yaml` alongside `foo.md` (see [[folder-note]]) or `foo/` folder, and those settings apply only to that route. The YAML structure is the same as your Markdown frontmatter, and vice-versa. Settings in the YAML frontmatter apply onto that Markdown route only; whereas settings in an individual .yaml file apply to that entire sub-route tree. Emanote does a deep-merge of the parent YAML configurations, so you can have children override only what's necessary. This is sometimes known as ["data cascade"](https://www.11ty.dev/docs/data-cascade/). The final merged YAML structure is passed to the HTML templates, of which you have full rendering control over.

Notice how this page's sidebar colorscheme has [changed to green]{.greenery}? View [the source of this page](https://github.com/srid/emanote/blob/master/docs/guide/yaml-config.md) to see the magic involved. That CSS greenery you just saw too comes from YAML.

>[!tip] Using in HTML templates
> You can reference the YAML frontmatter config from [[html-template]]. See [here](https://github.com/srid/emanote/discussions/131#discussioncomment-1382189) for details.

## Cascade merge semantics

When a child route's frontmatter or YAML overlaps with values from parent YAMLs, Emanote merges them along three rules:

- **Objects** are deep-merged by key — the child overrides individual nested fields without touching siblings.
- **Arrays** concatenate (with deduplication) — `tags: [team-doc]` in `folder.yaml` plus `tags: [internal-note]` on a child note yields `[team-doc, internal-note]`.
- **Scalars** right-win — the most-specific value (the leaf) overrides ancestors.

The array rule is what makes a parent YAML's `tags` survive to its children even when those children declare their own.

## Special properties

- `page.image`: The image to use for the page. This is used for the [[ogp]] meta tag `og:image` meta tag. If not specified, the first image in the page is used. Relative URLs are automatically rewritten to absolute URLs if `page.siteUrl` is non-empty.

- `page.lang`: The language tag for the rendered HTML page. Emanote also uses this value to choose the default template chrome strings, so a site can render its built-in navigation and helper text in a non-English language. The default templates ship English and French strings. Regional tags fall back through their base language and then English; for example, `fr-CA` uses `template.i18n.fr` first and `template.i18n.en` for any missing keys.

- `date`: The note timestamp. This is used to order note chronologically, such as for the timeline [[query|query]].
  The value can be set from the filename if it begins with `YYYY-MM-DD`, which is useful for including the date in the note URL.
  In case of conflict, the date from the YAML configuration takes priority.

You can override or add template strings under `template.i18n`:

```yaml
page:
  lang: fr
template:
  i18n:
    fr:
      home: Accueil
```

See [[i18n|the internationalisation demo]] and its [[i18n/i18n.fr|French child page]]
for a side-by-side example of `page.lang` selecting the default template chrome.

## Examples

- https://github.com/srid/srid/blob/master/index.yaml
- https://github.com/srid/emanote-template/blob/master/index.yaml
