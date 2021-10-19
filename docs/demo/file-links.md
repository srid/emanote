# File WikiLinks

`[[..]]` style wikilinks can link to not only Markdown files, but also to any *other* files.

For example, 

- Here is a link to some text file: [[Sample.txt]]
- You can also specify the full, or subset of, the path: [[demo/Sample.txt]]
- Of course, a custom link text may also be specified: [[Sample.txt|Some text file]]
- All of this is equivalent to [normal linking](demo/Sample.txt).

In the live server, links to static files will open in new browser tab.

If a wikilink refers to a display-able resource like image or video, it will appear [[embed|embedded]] in the generated HTML.
