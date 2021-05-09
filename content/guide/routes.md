---
order: 2
---
# Working with routes

Ema gives you the freedom to use any Haskell type for representing your site routes. You don't need complicated rewrite rules. Routes are best represented using what are known as *sum  types* (or ADT, short for *Abstract Data Type*). Here's an example of a route type:

```haskell
data Route 
  = Index
  | About
```

This type represents two routes pointing to -- the index page (`/`) and the about page (`/about`). Designing the route type is only half the job; you will also need to tell Ema how to convert it to / from the browser URL. We will explain this in the next section.

## Advanced example

Here's one possible way to design the route type for a weblog site,

```haskell
data Route
  = Home 
  | Blog BlogRoute
  | Tag TagRoute

data BlogRoute
  = BlogIndex
  | BlogPost Ema.Slug

data TagRoute
  = TagListing
  | Tag Tag

newtype Tag = Tag Text
```

Defining *hierarchical routes* like this is useful if you want to render *parts* of your HTML as being common to only a subset of your site, such as adding a blog header to all blog pages, but not to tag pages.

{.last}
[Next]{.next}, with our model and routes in place, [we will make them work with Ema](guide/class.md) by defining their static site behaviour.