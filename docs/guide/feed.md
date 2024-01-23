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

> [!note] `page.siteUrl`
> You must set `page.siteUrl` (see below) in the [[yaml-config|index.yaml]] for the feed to be generated.

## Example

An example is available in the [[query]] note. You can access its feed at [query.xml](query.xml).

## Configuration

Here are the supported settings:

- `feed.title`: the feed title, default to the note title.
- `feed.limit`: the maximum number of notes to include in the feed.

The feed is constructed from the first and only query of the note.
