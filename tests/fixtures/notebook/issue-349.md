---
slug: issue-349
---

# URL-bearing link labels (issue #349)

Each line below should render as exactly **one** `<a>`. The autolink
extension in `commonmark-hs` would otherwise wrap the embedded URL/email
in a nested `Link`, splitting these into two-or-more anchors.

[https://issue349-case1.example.com](https://issue349-case1.example.com#anchor)

[case2@issue349.example.com](mailto:case2@issue349.example.com?subject=hi)

[A sentence linking to https://issue349-case3a.example.com and https://issue349-case3b.example.com](https://issue349-case3-target.example.com)

[**https://issue349-case4.example.com**](https://issue349-case4-target.example.com)
