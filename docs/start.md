---
order: 1
---

# Getting Started

- [[install|Install Emanote]]
- Use your existing notebook, or create one from [[emanote-template]].
  - If using [[emanote-template]], open the notebook in [[vscode]] and install the recommended extensions.
  - [[emanote-template]] also includes the GitHub Pages workflow for static site deployment.
- Run `PORT=8080 emanote` in terminal after `cd`'ing to that notebook folder; this will launch the live server.
  - Or, if you only want to generate the HTML files (for deployment), run `mkdir /tmp/output; emanote gen /tmp/output`.

From here, either visit [[guide]] or [[demo]].