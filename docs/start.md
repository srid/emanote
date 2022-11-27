---
order: 1
---

# Getting Started

Follow these steps to get started with Emanote.

1. [[install|Install Emanote]]
1. Use your existing notebook, or create one from [[emanote-template]][^gh].
    - If using [[emanote-template]], open the notebook in [[vscode]] and install the recommended extensions.
1. Run `emanote run --port=8080` (or just `emanote`) in terminal after `cd`'ing to that notebook folder; this will launch the live server.
     - Or, if you only want to generate the HTML files (for deployment), run `mkdir /tmp/output; emanote gen /tmp/output`.
2. Visit [[guide]] to learn more about Emanote, or [[examples]] to get inspired first.[^h]


[^gh]: [[emanote-template]] also includes the GitHub Pages workflow for static site deployment.
[^h]: If you are [Haskell](https://srid.ca/haskell) developer, see [[architecture]].