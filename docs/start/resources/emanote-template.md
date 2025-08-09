---
slug: emanote-template
---

# emanote-template

[emanote-template][gh] is a Git repository that comes up with ready-made settings for editing notes in [[vscode]] and publishing to GitHub Pages (using the [[flake-module|the Nix flake module]]).

If you already have a GitHub account, click ["Use this template"][gh] to use it right away. Otherwise, simply download it [as a zip file](https://github.com/srid/emanote-template/archive/refs/heads/master.zip) and then unzip it.

Open the notebook in [[vscode]] and install the recommended extensions.

## Nix template

A Nix flake template is also provided if you want to do this in your existing notebook. Run:

```bash
nix flake init -t github:srid/emanote
```

This add a `flake.nix` file.  You can follow emanote-template's README from this point.

[gh]: https://github.com/srid/emanote-template