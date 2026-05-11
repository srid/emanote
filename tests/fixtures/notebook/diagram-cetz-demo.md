---
pandoc:
  filters:
    render:
      html:
        - lua-filters/diagram.lua
---

# Diagram filter — cetz

```cetz
#cetz.canvas({
  import cetz.draw: *
  circle((0, 0), radius: 1.6)
  circle((0, 0), radius: 1.0)
  circle((0, 0), radius: 0.4)
  content((0, 1.4), [bad])
  content((0, 0.8), [good])
  content((0, 0.2), [excellent])
})
```
