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

Configure your site metadata, rendering configuration and such using YAML configuration. Create a `foo.yaml` alongside `foo.md` (see [[folder-note]]) or `foo/` folder, and those settings apply only to that route. The YAML structure is the same as your Markdown frontmatter, and vice-versa. Settings in the YAML frontmatter apply onto that Markdown route only; whereas settings in an individual .yaml file apply to that entire sub-route tree. Emanote does a deep-merge of the parent YAML configurations, so you can have children override only what's necessary. This is sometimes known as ["data cascade"](https://www.11ty.dev/docs/data-cascade/). The final merged YAML structure is passed to the HTML templates, of which you have full rendering control over.

Notice how this page's sidebar colorscheme has [changed to green]{.greenery}? View [the source of this page](https://github.com/srid/emanote/blob/master/docs/guide/yaml-config.md) to see the magic involved. That CSS greenery you just saw too comes from YAML.

>[!tip] Using in HTML templates
> You can reference the YAML frontmatter config from [[html-template]]. See [here](https://github.com/srid/emanote/discussions/131#discussioncomment-1382189) for details.

## Special properties

- `page.image`: The image to use for the page. This is used for the [[ogp]] meta tag `og:image` meta tag. If not specified, the first image in the page is used. Relative URLs are automatically rewritten to absolute URLs if `page.siteUrl` is non-empty.

- `date`: The note timestamp. This is used to order note chronologically, such as for the timeline [[query|query]].
  The value can be set from the filename if it begins with `YYYY-MM-DD`, which is useful for including the date in the note URL.
  In case of conflict, the date from the YAML configuration takes priority.

- `next`, `prev`: Lists of wikilinks to be used for custom navigation. When set, the following template is available:

```
<ema:has:prev>
  <div class="flex-1 p-4 mt-8 bg-gray-100 rounded">
    <span class="mb-2 text-xl font-semibold text-gray-500">Next:
      <ema:note:prev>
        <a class="text-${theme}-600 mavenLinkBold hover:bg-${theme}-50 mr-2"
           href="${nav:url}">
          <nav:title />
        </a>
      </ema:note:prev>
    </span>
  </div>
</ema:has:prev>
```

## Examples

- https://github.com/srid/srid/blob/master/index.yaml
- https://github.com/srid/emanote-template/blob/master/index.yaml
