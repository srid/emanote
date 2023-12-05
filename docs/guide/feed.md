# ATOM feed

An atom feed can be generated for a given note [[query|query]] by adding the following to the [[yaml-config|note metadata]]:

~~~markdown
---
feed:
  enable: true
---

# My blog

```query
path:blog/*
```
~~~

If the note is named `blog.md`, then the feed will be available at `blog.xml`.

> [!note] `feed.siteUrl`
> You must set `feed.siteUrl` (see below) in the [[yaml-config|index.yaml]] for the feed to be generated.

## Example

An example is available in the [[query]] note. You can access its feed at [query.xml](query.xml).

## Configuration

Here are the supported settings:

- `feed.siteUrl`: the site url, default to global setting from the [[yaml-config|index.yaml]].
- `feed.title`: the feed title, default to the note title.
- `feed.limit`: the maximum number of notes to include in the feed.

The feed is constructed from the first and only query of the note.
