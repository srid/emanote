<apply template="/templates/special/base">
  <bind tag="special-main">
    <!-- /-/all is a flat-but-grouped index of every note. Unlike the sidebar
         (where the chip palette is reserved for the active item), every
         entry here IS a "linked note", so each leaf gets the wikilink text
         color + weight via tree-link-rest-class. The bg-fill is dropped at
         rest (it would be a wall of blue against the wide special-page
         column) and surfaces only on hover. -->
    <bind tag="tree-link-rest-class">text-primary-600 dark:text-primary-300 font-semibold tracking-tight hover:bg-primary-50/70 dark:hover:bg-primary-950/50 transition-colors</bind>
    <div class="pl-2">
      <ema:route-tree>
        <apply template="components/sidebar-tree" />
      </ema:route-tree>
    </div>
  </bind>
</apply>