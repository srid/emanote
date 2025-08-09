---
slug: flake-module
---

# Using as `flake-parts` module

Emanote provides a [flake-parts](https://flake.parts/) module that makes it easy to build or run locally Emanote sites in your Nix flakes. This is the recommended approach for projects using flake-parts.

## Basic Setup

Add the following to your `flake.nix` assuming your Markdown content lives under the `./content` subdirectory:

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    emanote.url = "github:srid/emanote";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      imports = [
        inputs.emanote.flakeModule
      ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        emanote = {
          sites = {
            "my-site" = {
              layers = [
                { path = ./content; pathString = "./content"; }
              ];
            };
          };
        };
      };
    };
}
```

This creates:

| Flake Output | Description | Command |
|--------|-------------|---------|
| `packages.my-site` | Static website build | `nix build .#my-site` |
| `apps.my-site` | Emanote live server | `nix run .#my-site` |
| `checks.my-site` | HTML validation with htmlproofer | `nix flake check` |

### Workflow

Run the Emanote live server for a specific site:
```bash
nix run .#my-site
```

Build the static site:
```bash
nix build .#my-site
```

Run all checks:
```bash
nix flake check
```

## Configuration Options

Each site in `emanote.sites.<name>` supports these options:

### Basic Options

- **`layers`** (required): List of content layers to include in the site
  - `path`: Nix path to the layer directory
  - `pathString`: String representation for the CLI
- **`package`**: Emanote package to use (default: `inputs'.emanote.packages.default`)
- **`port`**: Development server port (default: random port)
- **`basePath`**: Top-level directory path for deployment (default: root)
- **`allowBrokenInternalLinks`**: Allow broken internal links (default: `false`)
- **`check`**: Enable htmlproofer checks (default: `true`)
- **`extraConfig`**: Additional YAML configuration merged into `index.yaml`

### Example with All Options {#eg}

```nix
emanote = {
  sites = {
    "docs" = {
      package = config.packages.default;
      layers = [
        { path = ./docs; pathString = "./docs"; }
        { path = ./shared; pathString = "./shared"; }
      ];
      port = 8080;
      basePath = "documentation";
      allowBrokenInternalLinks = true;
      check = false;  # Disable htmlproofer for this site
      extraConfig = {
        template = {
          urlStrategy = "pretty";
          theme = "red";
        };
        pandoc = {
          rewriteClass = {
            "callout-note" = "bg-blue-100";
          };
        };
      };
    };
  };
};
```

## Multiple Sites {#multi}

You can define multiple sites in the same flake:

```nix
emanote = {
  sites = {
    "blog" = {
      layers = [{ path = ./blog; pathString = "./blog"; }];
      extraConfig.template.urlStrategy = "pretty";
    };
    "docs" = {
      layers = [
        { path = ./documentation; pathString = "./documentation"; }
      ];
      allowBrokenInternalLinks = true;
      check = false;  # Skip checks for docs site
    };
    "wiki" = {
      layers = [
        { path = ./wiki; pathString = "./wiki"; }
        { path = ./shared-assets; pathString = "./shared-assets"; }
      ];
      port = 9000;
    };
  };
};
```

This creates outputs for each site:

| Flake Output | Description |
|--------|-------------|
| `packages.{blog,docs,wiki}` | Static website builds |
| `apps.{blog,docs,wiki}` | Live development servers |
| `checks.{blog,wiki}` | HTML validation with htmlproofer (docs excluded due to `check = false`) |



## Template Example

See [[emanote-template]] for a complete working example.