---
slug: mermaid/client-side
---

# Client-side Mermaid rendering

The default Mermaid path in Emanote — see [[../mermaid]] — produces inline SVG at build time. That's the recommended approach: works offline, no client-side JavaScript, no network dependency. Use this page only if your build environment can't ship `mmdc`, or you specifically need browser-side rendering for interactive features.

To switch a page (or a whole site, via `index.yaml`) to client-side rendering, add the bundled `js.mermaid` snippet to `page.bodyHtml` in [[../../../guide/yaml-config|YAML configuration]] or Markdown frontmatter:

```yaml
page:
  bodyHtml: |
    <snippet var="js.mermaid" />
```

The snippet loads `mermaid.js` from a CDN and initialises it against any `<pre class="mermaid">…</pre>` blocks on the page. **You need to disable build-time rendering for those blocks**, otherwise Emanote will replace the source with SVG before the JavaScript ever sees it. (No knob exists yet — see [#653](https://github.com/srid/emanote/issues/653) for the planned `mermaid.renderer: server | client | off` config.)

Trade-offs versus the default static path:

- Requires network access at view time (CDN).
- Diagrams aren't visible to search engines, screen readers, or offline readers until the JavaScript runs.
- Adds a runtime parsing cost on every page load.
- Theming (`prefers-color-scheme: dark`) requires a full page reload, which the snippet wires up.
