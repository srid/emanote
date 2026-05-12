---
pandoc:
  filters:
    render:
      html:
        - lua-filters/diagram.lua
---

# Diagram filter — d2

```d2
grid-rows: 2

start: "where I started"
b1: "side branch"
b2: "another side branch"
back: "snap back"

start -> b1: meander
b1 -> b2: meander
b2 -> back: notice
```
