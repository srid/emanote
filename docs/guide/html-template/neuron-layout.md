---
template:
  uptree: 
    enable: true
  sidebar: 
    enable: false
  breadcrumbs: 
    enable: false
  base:
    containerClass: container mx-auto max-w-prose
---

# Neuron-like layout

Emanote's builtin [[html-template|HTML template]] layout can be configured to toggle UX features on or off. The default configuration includes [[sidebar]] and [[breadcrumbs]] but hides the [[uptree]]. To mimic the layout used by [Neuron](https://neuron.zettel.page/), turn off sidebar and breadcrumbs while enabling the uptree. Add this to your [[yaml-config]]:

```yml
template:
  uptree: 
    enable: true
  sidebar: 
    enable: false
  breadcrumbs: 
    enable: false
  base:
    containerClass: container mx-auto max-w-prose
```

This includes the [[uptree]] of Neuron based on folgezettel links as well as directory layout. Note that all top-level notes are automatically made a folgezettel branch of the root note (index), such that the "home" link appears on top in the uplink tree of all notes. 

> [!tip] Demo
> As a demo, the very page you are viewing is rendered using this template configuration (whilst leaving the rest of the site to use the default configuration)

## In lieu of sidebar

In the absence of [[sidebar]], you may use the [[query|folgezettel children query]] to show the children of the current note:

```query
children:.
```