---
order: 100
---

# Known Issues

## Unicode issues

If the generated site (or the Nix build) does not use the expected Unicode characters, you can try setting the `LC_ALL=C.UTF-8` environment variable before running emanote. See [\#125](https://github.com/EmaApps/emanote/issues/125) for details.