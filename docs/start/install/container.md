---
slug: container
---

# Container

Emanote provides [a pre-built container image][ghcr] that allows you to run Emanote without installing it locally. This is perfect for quick setups, CI/CD pipelines, or when you prefer containerized workflows.

## Container Image

The official Emanote container image is available at:
- **Registry**: [`ghcr.io/srid/emanote`][ghcr]
- **Supported architectures**: `x86_64` (amd64), `aarch64` (arm64) - automatically detects your platform
- **Working directory**: `/notebook` (container starts in this directory)

The container images are built as multi-architecture manifests, so Docker/Podman will automatically pull the correct image for your platform (Intel/AMD64 or ARM64).

[ghcr]: https://ghcr.io/srid/emanote

## Prerequisites

You'll need [Podman](https://podman.io/getting-started/installation) installed on your system[^1].

[^1]: Docker will also work, but Podman is recommended for rootless containers and better security.

## Multi-Architecture Support

Emanote container images support both Intel/AMD64 and ARM64 architectures (including Apple Silicon Macs and ARM-based servers). When you pull the image, the container runtime automatically selects the correct architecture for your platform - no special flags or tags needed.

## Running the Live Server

To start Emanote's live development server with hot reloading:

```sh
podman run -it --rm \
  -p 8080:8080 \
  -v ./docs:/notebook:z \
  ghcr.io/srid/emanote run -p 8080 -h 0.0.0.0
```

**What this does:**
- Mounts your notebook directory (`./docs`) to `/notebook` inside the container
- Sets `/notebook` as the working directory (where Emanote commands run)
- Exposes port 8080 for the web interface
- Enables hot reloading when you edit files
- Uses UTF-8 locale (built into the container image)

Once running, open http://localhost:8080 in your browser to view your site.

## Building Static Sites

To generate static HTML files for deployment:

### Create Output Directory

First, create a directory for the generated files:

```sh
mkdir -p ./output
```

### Generate Static Files

```sh
podman run -it --rm \
  -v ./docs:/notebook:z \
  -v ./output:/output:z \
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
