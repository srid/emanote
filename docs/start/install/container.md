---
slug: container
---

# Using Container

Emanote provides a standalone container.

Start the live server by mounting your notebook (here `./docs`) to `/site`:
```sh
podman run -it --rm -p 8080:8080 -v ./docs:/site:z ghcr.io/srid/emanote run -p 8080 -h 0.0.0.0
```

Then open http://localhost:8080


Build the static pages:
```sh
mkdir /output
podman run -it --rm -v ./docs:/site:z -v ./output:/output:z --tmpfs /tmp:mode=1777 ghcr.io/srid/emanote gen /output
```
