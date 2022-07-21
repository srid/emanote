# Synchronizing notes using Syncthing

[Syncthing](https://docs.syncthing.net/) is an open-source and superior[^sup] Dropbox alternative that provides end-to-end and full-sync functionality on Windows, Mac, Linux and Android (but not iPhone).

## Mobile editing

On Android[^ios] you can use the [Obsidian](https://play.google.com/store/apps/details?id=md.obsidian&hl=en&gl=US) app to edit your notes synced by Syncthing. ==Changes made on desktop and mobile propagate both ways automatically.==

## Dealing with data loss

Data loss is not uncommon when working with syncthing. For this reason, it is recommended to *also* track your notebook in a Git repository. Assuming you commit often (via [[vscode]] extensions like [this](https://marketplace.visualstudio.com/items?itemName=alfredbirk.git-add-commit-push)), if any file goes missing you can always recover it from Git. 

Git also acts as backup if you push your repository to a remote location (GitHub or a private server via SSH).

[^sup]: "superior" ... because when using Dropbox, Android phones (unlike desktop computers) cannot have automatic full-sync of files on disk.

[^ios]: Obsidian can also synchronize notes between iOS and macOS [via iCloud](https://help.obsidian.md/Getting+started/Sync+your+notes+across+devices).
