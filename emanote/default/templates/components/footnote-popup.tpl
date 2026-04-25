<!-- Footnote popup CSS escape hatch. The behavior itself lives in
     _emanote-static/js/footnote-popup.js (see issue #643); the Popover
     ::backdrop pseudo-element is the one bit that has no Tailwind
     variant, so it stays inline here next to the markup contract. -->

<style data-category="footnote-popup">
  #emanote-footnote-popover::backdrop { background: transparent; }
</style>
