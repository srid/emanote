---
slug: container
---

# Using Container

Emanote provides a standalone container.

Start the live server by mounting your notebook (here `./docs`) to `/site`:
```sh
podman run -it --rm -p 8080:8080 -v ./docs:/site:z ghcr.io/srid/emanote run -p 8080
```

Build the static pages:
```sh
podman run -it --rm -v ./docs:/site:z -v ./output:/output:z ghcr.io/srid/emanote gen /output
```
