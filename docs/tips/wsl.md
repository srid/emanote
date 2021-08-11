# Using on Windows

To work with Markdown notes using Emanote on Windows, follow these steps.

1. Setup Windows Subsystem for Linux (WSL2)
2. Install Visual Studio Code
3. (Optional) Run Syncthing from WSL

## Setup WSL2

- Follow [these instructions](https://docs.microsoft.com/en-us/windows/wsl/install-win10) to install WSL2 as well as Ubuntu. 
- [[install|Install Emanote]] in Ubuntu.[^term]
- Run `git clone https://github.com/srid/emanote-template.git` to make a local copy of [[emanote-template]] in your Ubuntu instance
- Run `emanote` inside the emanote-template directory, and make sure you that you can access the webpage in Windows.
- Press <kbd>Ctrl+C</kbd> to exit Emanote.

[^term]: You might find the new [Windows Terminal](https://docs.microsoft.com/en-us/windows/terminal/get-started) pleasant to work with.

## Install Visual Studio Code

- Install [[vscode]] *natively* on Windows (not WSL).
- Open VSCode and install the [Remote Development](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack) extension
- Press `Ctrl+Shift+P` and select `Remote-WSL: Open folder in WSL`
- Open the aforementioned [[emanote-template]] local copy
- Press `Ctrl+Backtick` to open Ubuntu Terminal inside VSCode, and in the terminal run `emanote`.
- Access the URL it shows, and make sure that you can view the notebook in your native Windows browser.
- Finally, open a Markdown file and make a change to it, while making sure the the web browser updates in real-time.

## Syncthing

This step is optional. For best experience with Emanote, we expect your notebook to live inside WSL (not Windows), due to a [WSL limitation](https://github.com/microsoft/WSL/issues/4739). Therefore, if you want to synchronize your files using [Syncthing](https://syncthing.net/) (an open-source Dropbox alternative that provides end-to-end and full-sync functionality on Android as well), you should install it on WSL Ubuntu, and not natively on Windows.

If you are on Ubuntu (WSL), simply go to https://apt.syncthing.net and install Syncthing. Then run `syncthing` to run the syncthing server. You might have to change the IP address in `~/.config/syncthing/config.xml` (from 127.0.0.1 to 0.0.0.0) in order to expose the service to Windows, in case WSL doesn't automatically forward it.

You can use [Task Scheduler](https://askubuntu.com/a/1178910) to automatically launch WSL Syncthing on Windows logon.