# Use config files in top level directory to specify overlays

Emanote has a powerful overlay system which enables different directories to be merged; this enables
sharing data and configuration across projects. This functionality is not yet exposed to the user.

## Motivation

Exposing this functionality to the user addresses the following use cases.

### Configuration overrides

Suppose I have a site which uses a different base url in development than in production. Overlaying
performs a structural merge on `.yaml` files, so, supposing `dev` is a directory that contains an
`index.yaml` with development-specific parameters and `prod` contains the site with the production
parameters, let's refer to the result of overlaying `dev` on top of `prod` as `prod+dev`. Running
`emanote gen` on `prod` produces HTML ready to deploy to production; running `emanote gen` on
`prod+dev` produces HTML ready to deploy to development. Notably, `prod` required no changes, and
`dev` required almost no configuration.

This proposal discusses how the user should represent things like `prod+dev`.

### Composition

It is relatively common to want to develop a site that builds upon another, or conversely to
develop a site (or component thereof) that is meant to be built upon.

#### Theming

- I want to publish a theme, but I want users to be able to customize it.
- I use `emanote` to develop multiple sites, but I want a consistent theme across all of them.

#### Content

- I want to have a public notes repository, but I also want a private one, which has more connections.
- I want to build upon other peoples public notes repositories, so I reference them and make
  connections

There are two things of note about the examples in which content (and not just theme) is overlayed:

- There may be zettel links in one direction, but there shouldn't be any in the other
- In the resulting generated site, there _should_ be backlinks that are not present in the
  underlying site

## Proposal

Each emanote site will have an optional configuration file (e.g. `emanote.dhall`), which contains a
list of layer locations. This proposal is agnostic as to what is considered a valid layer location
and will use local filesystem paths in examples. The layer locations will be read, and their
configuration files may have further layer locations. This will continue until there are no more new
locations (this system will need to detect cycles).

## Examples

Suppose we have three sites, `A`, `B`, and `C`. `C` includes `B` in its layer list, and `B` includes
`A`. If we execute `emanote` inside `C`, the resulting live site includes notes and configuration
from `A`, `B`, and `C`.

## Alternatives

### Exhaustive layers

We can require that the list of layers is exhaustive. This means we only need to look at the
configuration file for the current site, but that you need to copy a site's configuration file if
you're building on it, which hampers reuse.

If we require an exhaustive list of layers, we could do away with the config file entirely and take
them on the command line. This makes reuse even more difficult, since you may not know all the
transitive dependencies of a site, making it impossible to build correctly.

### Existing config

We can use an existing configuration file, like `index.yaml`. This has the following problems:

- It's possible for a layer to change it, so it's unnatural to put configuration about selecting
  layers in it
- It's not semantically distinct from other yaml files, but layers would only make sense at the top
  level
- `index.yaml` arguably configures the site and is (indirectly) an input to `ema`, while this
  belongs in a file for configuring `emanote` specifically.

## Costs and drawbacks

- Generating the list of layers is not trivial (though not too difficult)
- Since it's implicit, it will need to be debuggable, but the logs should be good enough for now
- Maintaining the list for hot-reload becomes tricky if a site changes its layers. For now, I think
  we shouldn't try to hot-reload in that case
- If layer locations can include urls, there are potentially security concerns with doing unexpected
  remote access
