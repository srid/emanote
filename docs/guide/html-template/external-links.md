---
page:
  headHtml: |
    <snippet var="js.mathjax" />
---

# External links

A link whose address begins with an [URI scheme](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier#Syntax) component is considered an **external link**. 

## Heuristic 

Emanote displays an icon next to an external hyperlink if its description contains some text, including inline code and math formulas.[^noneg]  The heuristic is intended to make the site look good in most cases. 

### Overriding the heuristic

This behaviour can be overriden by setting the value of the `data-linkicon` [data attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*) of a link:

| Attribute to use                               | Description                                    |
| ---------------------------------------------- | ---------------------------------------------- |
| `{data-linkicon=external}`                     | *Force* displaying the icon next to the link   |
| `{data-linkicon=none}` or `{data-linkicon=""}` | *Prevent* displaying the icon next to the link |

[^noneg]: A common non-example is a hyperlink containing only an image in the description.

Note that the attribute can also be used to display the icons in the [[html-template|HTML templates]] (like the footer or the sidebar) or in raw HTML of [[markdown]].

## URL properties

The displayed icon may depend on the link properties (e.g. the actual URI scheme). This is [[custom-style|customized using CSS]]. By default, Emanote displays a different icon if the URI scheme component is `mailto:`. Check the <https://github.com/srid/emanote/tree/master/emanote/default/templates/base.tpl> of [[html-template|HTML template]] for details.

## Demo

* Default styling:
  * [the emanote repo](https://github.com/srid/emanote)
  * [why the external link symbol ![[external-link-icon.svg]] is not in Unicode](https://www.unicode.org/alloc/nonapprovals.html)
  * [$e^{i \pi} + 1 = 0$](https://en.wikipedia.org/wiki/Euler%27s_identity)
  * [`(>>=) :: forall a b. m a -> (a -> m b) -> m b`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:-62--62--61-)
  * [![[hello-badge.svg]]](https://emanote.srid.ca)
  * mother_hetchel@weaversguild.com
* `{data-linkicon=none}` used to suppress the icon:
  * Water's formula is [H](https://en.wikipedia.org/wiki/Hydrogen){data-linkicon=none}₂[O](https://en.wikipedia.org/wiki/Oxygen){data-linkicon=none}.
  * A 90's style hyperlink:
    [➡➡➡ **CLICK HERE!!!** ⬅⬅⬅](https://emanote.srid.ca){class="shadow-lg border-8 rounded-md bg-yellow-400 border-red-200" data-linkicon=none}
* `{data-linkicon=external}` used to forcefully show the icon
  * [![[pIqaD.svg]]](https://en.wikipedia.org/wiki/Klingon_scripts){data-linkicon=external}
