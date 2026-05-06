---
paths:
  - "docs/**/*.md"
---

# Emanote Documentation Pages

When adding a new documentation page under `docs/`, give it an explicit simple `slug` in frontmatter. Prefer a short, stable slug that matches the intended public route segment, such as `i18n`, `i18n.fr`, `flake-module`, or `yaml-config`; do not rely on filenames or other implementation-shaped paths to define the public route.

Connect new docs pages to the surrounding documentation with wikilinks. Link to the nearest relevant guide pages such as `[[yaml-config]]`, `[[html-template]]`, or feature-specific pages instead of leaving the page as an isolated note. Prefer wikilinks over raw relative Markdown links for internal docs references.
