---
slug: images
---

# Adding Images

>[!tip]
> The extension uses the traditional `![]()` syntax to link to the image, but you may switch to using the [[embed|wiki-link embedding syntax]] (`![[]]`) as it has the advantage of not needing to specify the full path to the image (thus allowing you to the move the image around under the notebook without breaking referring links). [[emanote-template]] is already configured to do this by default.

If your image is already copied in the OS clipboard---many screenshoting tools[^scr] already provide this capability---and if you use [[vscode]], you can use the [Paste Image](https://marketplace.visualstudio.com/items?itemName=mushan.vscode-paste-image) extension to *directly paste* it in your Markdown note. This does the following,

- Save the copied image to your project directory
- Add a link to it in the currently active Markdown file

![](https://raw.githubusercontent.com/mushanshitiancai/vscode-paste-image/master/res/vscode-paste-image.gif){.center}

It basically automates the workflow of having to manually move the image file to your notebook, and then writing the Markdown image syntax to link to it.


[^scr]: On Linux, you may use [GNOME screenshot](https://help.gnome.org/users/gnome-help/stable/screen-shot-record.html.en) or (if using a tiling window manager) [maim](https://github.com/naelstrof/maim).