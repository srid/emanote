---
pandoc:
  filters:
    render:
      html:
        - lua-filters/diagram.lua
---

# Diagram filter — cetz

```cetz
#import "@preview/cetz:0.3.4": canvas, draw

#canvas({
  import draw: *
  line((-2, 0), (-0.5, 0), stroke: 0.7pt + gray)
  for x in (-1.7, -1.3, -0.9) {
    line((x, -0.15), (x, 0.15), stroke: 1.5pt + gray)
  }
  rect((0.95, -0.5), (1.05, 0.5))
  content((-1.3, -0.6), [periodicity])
  content((1.0, -0.8), [no duration])
})
```
