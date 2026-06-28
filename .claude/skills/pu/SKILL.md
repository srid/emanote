---
name: pu
description: >-
  Provision and drive a `pu` box — an Incus container used as
  a clean Linux host for CI, builds, and evidence capture. Use when you need to
  run something on a fresh remote box instead of the user's machine: `nix run` a
  build, run CI against a real host, capture screenshots/video off-machine, or
  reproduce on a pristine environment. Covers create/connect/scp/destroy, running
  remote commands, copying artifacts back, and the no-egress failure mode.
  Triggers on "pu box", "spin up a box", "run this on a box", "ephemeral host",
  "pu create/connect/destroy".
---

# pu — on-demand Incus boxes

`pu` hands out Linux containers. Each box is a clean NixOS host with Nix
+ flakes, reachable over SSH through `pu`'s own proxy. Use one whenever work should
run **off the user's machine** — a CI run, a `nix run` build, evidence capture —
so nothing local is at risk and the environment is reproducible. A box can be short-lived
(spin up, use, `destroy`) or kept around long-term — the lifetime is yours to choose.

## Lifecycle

```sh
pu create "$host"          # create; writes ~/.pu-state/$host/ssh_config
pu list                    # NAME + LOCATION (the physical host it landed on)
pu connect "$host"         # interactive ssh
pu connect "$host" -- CMD  # run CMD on the box and return
pu destroy "$host"         # tear down — always do this when finished
```

Name is positional. Pick a descriptive, collision-free name (e.g. `app-pr-42-evidence`).

`pu connect` is the reliable way in — it reads `~/.pu-state/$host/ssh_config` itself, so it
needs no setup. Bare `ssh "$host"` works **only** if you've added `Include
~/.pu-state/*/ssh_config` to `~/.ssh/config` (optional, often not set up); otherwise use
`pu connect`, or pass the config explicitly: `ssh -F ~/.pu-state/$host/ssh_config "$host"`.

## Run commands on the box

```sh
# One-shot
pu connect "$host" -- 'uname -a'

# Background a long-running server (nohup so it survives the SSH session)
pu connect "$host" -- "nohup nix run github:owner/app -- --port 8080 >/tmp/app.log 2>&1 &"

# Poll until it's healthy
pu connect "$host" -- 'until curl -sf http://127.0.0.1:8080/health; do sleep 2; done'
```

The box has its **own loopback** — bind servers to `127.0.0.1` on whatever port you
like; there is no clash with anything on the user's machine.

## Copy artifacts back

`pu connect` is SSH, so `scp` works against the box's generated config:

```sh
scp -F ~/.pu-state/"$host"/ssh_config "$host":/tmp/out.png /tmp/out.png
```

## Failure mode: no outbound network

A box occasionally lands on a host with broken egress — DNS and even raw-IP TCP
time out, so `nix run github:...` hangs on "Resolving timed out". This is
host-specific, not your fault. **Probe egress first; if it fails, destroy and
recreate** (a fresh box usually lands on a healthy host):

```sh
pu connect "$host" -- 'timeout 15 curl -sS -o /dev/null -w "%{http_code}\n" https://api.github.com' \
  || { echo "no egress — recreating"; pu destroy "$host"; pu create "$host"; }
```

If retries keep landing on dead hosts, capture diagnostics for the admin — the box's
`LOCATION` from `pu list`, `/etc/resolv.conf`, `ip route`, and a `/dev/tcp` connect
test to the gateway and to a raw IP — and hand them over (e.g. a gist).
