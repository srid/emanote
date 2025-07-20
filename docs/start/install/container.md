---
slug: container
---

# Using Emanote with Containers

Emanote provides a pre-built container image that allows you to run Emanote without installing it locally. This is perfect for quick setups, CI/CD pipelines, or when you prefer containerized workflows.

## Container Image

The official Emanote container image is available at:
- **Registry**: `ghcr.io/srid/emanote`
- **Supported architectures**: `x86_64`, `aarch64`
- **Working directory**: `/notebook` (container starts in this directory)

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
  -v ./docs:/notebook:z \
  -e LANG=C.UTF-8 \
  ghcr.io/srid/emanote run -p 8080 -h 0.0.0.0
```

### Using Docker

```sh
docker run -it --rm \
  -p 8080:8080 \
  -v ./docs:/notebook \
  -e LANG=C.UTF-8 \
  ghcr.io/srid/emanote run -p 8080 -h 0.0.0.0
```

**What this does:**
- Mounts your notebook directory (`./docs`) to `/notebook` inside the container
- Sets `/notebook` as the working directory (where Emanote commands run)
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
  -v ./docs:/notebook:z \
  -v ./output:/output:z \
  -e LANG=C.UTF-8 \
  --tmpfs /tmp:mode=1777 \
  ghcr.io/srid/emanote gen /output
```

#### Using Docker

```sh
docker run -it --rm \
  -v ./docs:/notebook \
  -v ./output:/output \
  -e LANG=C.UTF-8 \
  --tmpfs /tmp:mode=1777 \
  ghcr.io/srid/emanote gen /output
```

**What this does:**
- Mounts your source directory (`./docs`) to `/notebook` (the container's working directory)
- Mounts your output directory (`./output`) for generated files
- Creates a temporary filesystem for build artifacts
- Generates optimized static HTML, CSS, and JavaScript files

## Understanding Container Paths

The Emanote container is configured with `/notebook` as its working directory. This means:

- When you mount your local directory (e.g., `./docs`) to `/notebook`, Emanote commands run from that location
- All relative paths in Emanote configuration and commands are resolved from `/notebook`
- No need to specify the notebook path in Emanote commands - it automatically uses the current working directory

For example, when running `emanote run`, the container automatically serves content from `/notebook` without requiring additional path arguments.
