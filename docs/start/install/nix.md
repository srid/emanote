---
slug: nix
---

# Advanced Nix Usage

For advanced Nix users who want deeper integration with the Nix ecosystem.

## Basic Installation

```sh
nix profile install github:srid/emanote
```

## Using as `flake-parts` module

See [[flake-module]]# for detailed documentation on using Emanote's flake-parts module.

## Using Emanote as a Home Manager service

[Home Manager][home-manager] is a
Nix-based personal configuration manager. If you use Home Manager,
then Emanote has a [module][] that can be imported into your
configuration.

Merge or import this config into your `~/.config/nixpkgs/home.nix`:
```nix
{ config, ... }:
let
  emanote = import (builtins.fetchTarball "https://github.com/srid/emanote/archive/master.tar.gz");
in {
  imports = [ emanote.homeManagerModule ];
  services.emanote = {
    enable = true;
    # host = "127.0.0.1"; # default listen address is 127.0.0.1
    # port = 7000;        # default http port is 7000
    notes = [
      "/home/user/notes"  # add as many layers as you like
    ];
    package = emanote.packages.${builtins.currentSystem}.default;
  };
}
```

Re-apply your home-manager configuration the usual way (e.g. `home-manager switch`).

You will then have an `emanote` command in your profile, and a background
service running a live-preview of your notes (systemd on Linux, launchd on macOS).

On Linux:
```sh
$ home-manager switch
...
$ systemctl --user status emanote.service
● emanote.service - Emanote web server
     Loaded: loaded (/nix/store/i1af5hdydwcf7y0r55n7fd67dnw5habd-home-manager-files/.config/systemd/user/emanote.service; enabled; vendor preset: enabled)
     Active: active (running) since Tue 2021-11-02 17:17:04 AWST; 17h ago
   Main PID: 1705303 (emanote)
      Tasks: 26 (limit: 38220)
     Memory: 38.3M
        CPU: 2.884s
     CGroup: /user.slice/user-1000.slice/user@1000.service/app.slice/emanote.service
             └─1705303 /nix/store/9hj2cwk1jakfws0d1hpwa221kcni3j45-emanote-0.3.12.1/bin/emanote --layers /nix/store/hr7wp1xvqn48b8gy16sdq6k2csrvr8c1-emanote-config;/home/user/notes
```

On macOS:
```sh
$ home-manager switch
...
$ launchctl list | grep emanote
-	0	org.nix-community.home.emanote
$ tail ~/Library/Logs/emanote.log
```

[home-manager]: https://nixos.asia/en/home-manager
[module]: https://github.com/srid/emanote/blob/master/nix/modules/home/emanote.nix
