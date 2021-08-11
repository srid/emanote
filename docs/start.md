---
order: 1
---

# Getting Started

WIP, but for now:

- Follow the [README on GitHub](https://github.com/srid/emanote#emanote) to install Emanote
- Use your existing notebook, or create one from [[emanote-template]].
  - If using [[emanote-template]], open the notebook in [[vscode]] and install the recommended extensions.
- Run `PORT=8080 emanote` on that notebook
  - Or `mkdir /tmp/output; emanote gen /tmp/output` to generate static files

### Other ways to obtain Emanote

If you do not wish to use Nix, you may use Emanote in one of the following ways:

## Pre-built executable

You may try the self-contained executable bundle on Linux or Windows Subsystem for Linux (WSL). They are built off the latest sources in GitHub Actions CI. For macOS, see the Docker section below.

1. Click the latest (passed) entry in [CI workflows](https://github.com/srid/emanote/actions?query=branch%3Amaster)
1. Download "emanote-binary" (a zip file), and extract it.
1. Run the binary in the zip file, as `./emanote --version` to verify that everything works
   - The first time you run this, it will take a few seconds to bootstrap.
   - This binary is produced using the [experimental nix bundle](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-bundle.html) feature. Do not interrupt it (Ctrl+C) during its *first* run; doing so might break that binary permanently on that system.

## Docker image

[[emanote-template]] uses the official Docker image, which can be used as

```
docker run -v $PWD:/data sridca/emanote emanote --layers "/data" gen /data/output.docker
```