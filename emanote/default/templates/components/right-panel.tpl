<!-- Right panel: TOC + slim backlinks list, attached to the right of the
     main panel at md+ as a sibling of the sidebar inside #container.
     Mirrors sidebar's chrome (bg, border, sticky+scroll, width tiers)
     so the two read as a symmetric pair across the prose card.

     Below md, this panel is hidden; backlinks-bottom.tpl renders the
     same backlinks attached to the bottom of the same card via the
     container's flex-col stacking. The Heist split below keeps the
     panel from rendering as an empty gray column on pages that have
     neither a TOC nor backlinks. -->
<!-- Note on overflow: the right-panel intentionally does NOT set
     overflow-y:auto (unlike the sidebar). The backlink hover-flyout in
     backlinks-margin.tpl uses position:absolute with right:100% to
     escape leftward over the prose; an overflow:auto ancestor would
     clip it. We trade internal panel scrolling for working flyouts —
     fine for typical TOCs, and the page itself still scrolls. -->
<bind tag="rightPanelClass">flex-shrink hidden leading-relaxed lg:block lg:sticky lg:top-0 lg:max-h-screen lg:w-52 lg:min-w-52 xl:w-72 xl:min-w-72 bg-gray-50 dark:bg-gray-950 lg:border-l border-gray-200 dark:border-gray-800</bind>
<ema:has:toc>
  <aside id="right-panel" class="${rightPanelClass}">
    <div class="px-4 py-5 text-gray-800 dark:text-gray-200 space-y-8">
      <apply template="toc" />
      <apply template="backlinks-margin" />
    </div>
  </aside>
  <else />
  <ema:note:backlinks:nodaily>
    <aside id="right-panel" class="${rightPanelClass}">
      <div class="px-4 py-5 text-gray-800 dark:text-gray-200">
        <apply template="backlinks-margin" />
      </div>
    </aside>
  </ema:note:backlinks:nodaily>
</ema:has:toc>
