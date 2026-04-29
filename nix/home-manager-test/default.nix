{ emanote, home-manager, pkgs }:

pkgs.testers.runNixOSTest {
  name = "emanote-home-manager-module";

  nodes.machine = { ... }: {
    imports = [
      home-manager.nixosModules.home-manager
    ];

    users.users.alice = {
      isNormalUser = true;
      initialPassword = "pass";
    };

    services.getty.autologinUser = "alice";

    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.users.alice = {
      imports = [
        emanote.homeManagerModule
      ];

      services.emanote = {
        enable = true;
        notes = [ "/home/alice/notes" ];
        systemdTarget = "default.target";
      };

      # release-25.05 evaluates with this repo's pinned nixpkgs; newer
      # Home Manager branches currently require newer lib functions.
      home.enableNixpkgsReleaseCheck = false;
      home.stateVersion = "24.11";
    };

    systemd.tmpfiles.rules = [
      "d /home/alice/notes 0755 alice users - -"
      "f /home/alice/notes/index.md 0644 alice users - Hello"
    ];

    system.stateVersion = "24.11";
  };

  testScript = ''
    machine.wait_for_unit("multi-user.target")
    machine.wait_until_succeeds("systemctl is-active user@1000.service", timeout=60)
    machine.wait_until_succeeds(
        "runuser -u alice -- env XDG_RUNTIME_DIR=/run/user/1000 DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus systemctl --user is-active emanote.service",
        timeout=60,
    )
    machine.wait_until_succeeds(
        "curl --fail --silent --max-time 5 http://127.0.0.1:7000/ > /dev/null",
        timeout=60,
    )
  '';
}
