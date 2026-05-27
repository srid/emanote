---
slug: ogp
---

# Open Graph Protocol (OGP)

The [Open Graph protocol](https://ogp.me/) specifies what goes in `<meta>` attributes of the HTML which in turn generates "preview cards" for social sites and apps like X.com, Facebook, Slack, etc.

![](https://ogp.me/logo.png)

Emanote generates OGP meta tags for each note, using the following rules:

| Meta tag         | How the value is determined          |
| ---------------- | ------------------------------------ |
| `og:title`       | The title of the note                |
| `og:description` | The first paragraph of the note[^wl] |
| `og:site_name`   | The site name from [[yaml-config\|page.siteTitle]] |
| `og:type` | `website` |
| `og:image` | The first image[^img] in the note, if any; otherwise, use the `image` [[yaml-config\|YAML metadata]] |

## Twitter

[Twitter Card](https://developer.twitter.com/en/docs/twitter-for-websites/cards/overview/abouts-cards) style can be set using `page.twitter.card` [[yaml-config|YAML metadata]]. It it set to `summary_large_image` by default.

[^wl]: Wikilinks like `[[foo]]` render *as is* (due to a limitation). However, `[[foo|some text]]` will render as `some text` (the link text). If you do not wish to have wikilink syntax appearing in page description, specify a custom like in the second example.

[^img]: Unfortunately, embed wikilinks (eg.: `![[foo.jpeg]]`) are not recognized here. You should use regular links, eg.: `![](foo.jpeg)`.
