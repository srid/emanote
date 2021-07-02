# Adding Images

:::{.note}
Note that the extension uses the traditional `![]()` syntax to link to the image, but you may switch to using the [[embed|wiki-link embedding syntax]] (`![[]]`) as it has the advantage of not needing to specify the full path to the image (thus allowing you to the move the image around under the notebook without breaking referring links).
:::

If your image is already copied in the OS clipboard---many screenshoting tools[^scr] already provide this capability---and if you use [[vscode]], you can use the [Paste Image](https://marketplace.visualstudio.com/items?itemName=mushan.vscode-paste-image) extension to *directly paste* it in your Markdown note. This does the following,


- Save the copied image to your project directory
- Add a link to it in the currently active Markdown file

It basically automates the workflow of manually moving the file to your notebook, and then writing the Markdown image syntax to link to it.


[^scr]: On Linux, you may use [GNOME screenshot](https://help.gnome.org/users/gnome-help/stable/screen-shot-record.html.en) or (if using a tiling window manager) [maim](https://github.com/naelstrof/maim).