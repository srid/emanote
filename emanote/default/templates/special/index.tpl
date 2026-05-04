<apply template="/templates/special/base">
  <bind tag="special-main">
    <!-- /-/all is a flat-but-grouped index of every note. Unlike the sidebar
         (where the chip palette is reserved for the active item), every
         entry here IS a "linked note", so all leaves get the wikilink chip
         palette via tree-link-rest-class. The active-node bind inside
         sidebar-tree.tpl never fires (no current page), so the chip rest
         class becomes the universal link style. -->
    <bind tag="tree-link-rest-class">bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 font-semibold tracking-tight hover:bg-primary-100/80 dark:hover:bg-primary-900/70 transition-colors</bind>
    <div class="pl-2">
      <ema:route-tree>
        <apply template="components/sidebar-tree" />
      </ema:route-tree>
    </div>
  </bind>
</apply>