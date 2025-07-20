---
slug: container
---

# Using Emanote with Containers

Emanote provides a pre-built container image that allows you to run Emanote without installing it locally. This is perfect for quick setups, CI/CD pipelines, or when you prefer containerized workflows.

## Container Image

The official Emanote container image is available at:
- **Registry**: `ghcr.io/srid/emanote`
- **Supported architectures**: `x86_64`, `aarch64`

## Prerequisites

You'll need either Docker or Podman installed on your system:
- [Docker](https://docs.docker.com/get-docker/)
- [Podman](https://podman.io/getting-started/installation) (recommended for rootless containers)

## Running the Live Server

To start Emanote's live development server with hot reloading:

### Using Podman (Recommended)

```sh
podman run -it --rm \
  -p 8080:8080 \
  -v ./docs:/site:z \
  -e LANG=C.UTF-8 \
  ghcr.io/srid/emanote run -p 8080 -h 0.0.0.0
```

### Using Docker

```sh
docker run -it --rm \
  -p 8080:8080 \
  -v ./docs:/site \
  -e LANG=C.UTF-8 \
  ghcr.io/srid/emanote run -p 8080 -h 0.0.0.0
```

**What this does:**
- Mounts your notebook directory (`./docs`) to `/site` inside the container
- Exposes port 8080 for the web interface
- Enables hot reloading when you edit files
- Sets proper locale for Unicode support

Once running, open http://localhost:8080 in your browser to view your site.

## Building Static Sites

To generate static HTML files for deployment:

### Create Output Directory

First, create a directory for the generated files:

```sh
mkdir -p ./output
```

### Generate Static Files

#### Using Podman

```sh
podman run -it --rm \
  -v ./docs:/site:z \
  -v ./output:/output:z \
  -e LANG=C.UTF-8 \
  --tmpfs /tmp:mode=1777 \
  ghcr.io/srid/emanote gen /output
```

#### Using Docker

```sh
docker run -it --rm \
  -v ./docs:/site \
  -v ./output:/output \
  -e LANG=C.UTF-8 \
  --tmpfs /tmp:mode=1777 \
  ghcr.io/srid/emanote gen /output
```

**What this does:**
- Mounts your source directory (`./docs`) as read-only
- Mounts your output directory (`./output`) for generated files
- Creates a temporary filesystem for build artifacts
- Generates optimized static HTML, CSS, and JavaScript files
