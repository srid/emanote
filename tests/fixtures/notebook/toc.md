# TOC scroll-spy fixture

Three short sections, each padded with a tall spacer so the next
heading is well below the viewport at 1280×720. Scrolling a target
heading to the top of the viewport puts exactly that section in the
IntersectionObserver's active band (rootMargin: -80px 0px -60% 0px),
which lets the test assert which TOC link is highlighted.

## Apple

Section A body.

<div style="height: 1500px"></div>

## Banana

Section B body.

<div style="height: 1500px"></div>

## Cherry

Section C body.

<div style="height: 1500px"></div>
