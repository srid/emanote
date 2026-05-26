---
slug: cyclic-embed-demo
tags: [emanote/syntax/demo]
short-title: Cyclic embed demo
date: 2026-04-30
---

# Cyclic embed demo

This note exists only to demonstrate Emanote's cyclic-embed detection
([`![[..]]`](embed.md)). The line below is a self-embed of this same
note — Emanote stops at the point the loop would close and emits a
placeholder instead of expanding the embed forever.

![[cyclic-embed-demo]]
