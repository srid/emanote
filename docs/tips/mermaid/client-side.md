---
slug: mermaid/client-side
mermaid:
  static: false
page:
  bodyHtml: |
    <snippet var="js.mermaid" />
---

# Client-side Mermaid rendering

The default Mermaid path in Emanote — see [[../mermaid]] — produces inline SVG at build time. That's the recommended approach: works offline, no client-side JavaScript, no network dependency. Use this page only if your build environment can't ship `mmdc`, or you specifically need browser-side rendering for interactive features (e.g. live `prefers-color-scheme` reaction).

To switch a page (or a whole site, via `index.yaml`) to client-side rendering, add the `mermaid.static: false` opt-out and the bundled `js.mermaid` snippet to its frontmatter (or [[../../guide/yaml-config|YAML configuration]]):

```yaml
mermaid:
  static: false
page:
  bodyHtml: |
    <snippet var="js.mermaid" />
```

`mermaid.static: false` tells Emanote to leave `mermaid` code blocks alone so the JavaScript snippet can find them; the snippet then loads `mermaid.js` from a CDN and renders every block in place.

This page itself is configured exactly that way — every diagram below is rendered live in your browser.

Trade-offs versus the default static path:

- Requires network access at view time (CDN).
- Diagrams aren't visible to search engines, screen readers, or offline readers until the JavaScript runs.
- Adds a runtime parsing cost on every page load.
- Honors `prefers-color-scheme` (the snippet reloads the page when the OS toggle flips).

## Demo

The same diagrams as [[../mermaid]], rendered client-side instead.

### Graph diagram

```mermaid
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph TD;
    A-->B;
    A-->C;
    B-->D;
    C-->D;
```

### State diagram

```mermaid
stateDiagram
    direction LR
    [*] --> A
    A --> B
    B --> C
    state B {
      direction LR
      a --> b
    }
    B --> D
```

### GANTT diagram

```mermaid
gantt
    dateFormat  YYYY-MM-DD
    title       Adding GANTT diagram functionality to mermaid
    excludes    weekends

    section A section
    Completed task            :done,    des1, 2014-01-06,2014-01-08
    Active task               :active,  des2, 2014-01-09, 3d
    Future task               :         des3, after des2, 5d

    section Critical tasks
    Completed task in the critical line :crit, done, 2014-01-06,24h
    Implement parser and jison          :crit, done, after des1, 2d
    Create tests for parser             :crit, active, 3d

    section Documentation
    Describe gantt syntax               :active, a1, after des1, 3d
    Add gantt diagram to demo page      :after a1  , 20h
```

### Layout (elk)

```mermaid
---
config:
  layout: elk
---
graph TD;
    A-->B;
    A-->C;
    B-->D;
    C-->D;
```
