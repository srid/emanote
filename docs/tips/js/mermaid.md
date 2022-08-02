---
page:
  headHtml: |
    <snippet var="js.highlightjs" />
    <snippet var="js.mermaid" />
    <snippet var="js.stork-search" />
---

# Mermaid Diagrams

[Mermaid](https://mermaid-js.github.io/mermaid/#/) lets you create diagrams and visualizations using text and code. You can define these diagrams in your Markdown code blocks. 

1. Add the following to your `page.headHtml`, either in frontmatter or `index.yaml` (see [[yaml-config]])
    ```yaml
    page:
      headHtml: |
        <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
        <script>
          mermaid.initialize({startOnLoad:false});
          mermaid.init(undefined,document.querySelectorAll(".mermaid"));
        </script>
    ```
1. Add a code block with `mermaid` language
    ~~~markdown
    ```mermaid
    %%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
    graph TD;
        A-->B;
        A-->C;
        B-->D;
        C-->D;
    ```
    ~~~

The result will be:

```mermaid {.nohighlight}
%%{init: {'theme': 'forest', "flowchart" : { "curve" : "basis" } } }%%
graph TD;
    A-->B;
    A-->C;
    B-->D;
    C-->D;
```

Note that mermaid.js will have trouble operating with PrismJS [[syntax-highlighting]], but it works well with highlight.js (which this note uses).

## More examples

```mermaid {.nohighlight}
stateDiagram
    direction LR
    [*] --> A
    A --> B
    B --> C
    state B {
      direction LR
      a --> b
    }
    B --> D
```

```mermaid {.nohighlight}
gantt
    dateFormat  YYYY-MM-DD
    title       Adding GANTT diagram functionality to mermaid
    excludes    weekends
    %% (`excludes` accepts specific dates in YYYY-MM-DD format, days of the week ("sunday") or "weekends", but not the word "weekdays".)

    section A section
    Completed task            :done,    des1, 2014-01-06,2014-01-08
    Active task               :active,  des2, 2014-01-09, 3d
    Future task               :         des3, after des2, 5d
    Future task2              :         des4, after des3, 5d

    section Critical tasks
    Completed task in the critical line :crit, done, 2014-01-06,24h
    Implement parser and jison          :crit, done, after des1, 2d
    Create tests for parser             :crit, active, 3d
    Future task in critical line        :crit, 5d
    Create tests for renderer           :2d
    Add to mermaid                      :1d

    section Documentation
    Describe gantt syntax               :active, a1, after des1, 3d
    Add gantt diagram to demo page      :after a1  , 20h
    Add another diagram to demo page    :doc1, after a1  , 48h

    section Last section
    Describe gantt syntax               :after doc1, 3d
    Add gantt diagram to demo page      :20h
    Add another diagram to demo page    :48h
```
