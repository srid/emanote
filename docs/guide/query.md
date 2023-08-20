---
order: -95
feed:
  enable: true
  title: Feed Demo - Emanote
---

# Obsidian-style queries

Emanote supports [Obsidian-style embed queries](https://help.obsidian.md/Plugins/Search#Embed+search+results+in+a+note). See [\#10](https://github.com/srid/emanote/issues/10) for details.

Both `*` and `**` are supported in the path patterns. See [System.FilePattern](https://hackage.haskell.org/package/filepattern-0.1.3/docs/System-FilePattern.html) for exact semantics.

## Examples

Some examples are provided below:

### List notes in current folder

~~~markdown
```query
path:./*
```
~~~

### List notes in an arbitrary folder

~~~markdown
```query
path:foo/bar/*
```
~~~

### List notes by a tag

~~~markdown
```query
tag:#foo
```
~~~

### List notes by a tag pattern

~~~markdown
```query
tag:foo/*/qux
```
~~~

## Timeline queries

Queries can be rendered as a timeline by using the `timeline` code block attribute.

~~~markdown
```query {.timeline}
tag:emanote/syntax/**
```
~~~

This will use the `date` [[yaml-config|frontmatter]] metadata to sort the results, as well as display the date alongside it. A live demo of that snippet above is presented below:

```query {.timeline}
tag:emanote/syntax/**
```
