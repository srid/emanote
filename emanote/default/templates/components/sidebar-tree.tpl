<!-- Variable bindings for this tree. The active item shares the wikilink
     chip palette (bg-primary-50/70, text-primary-600, font-semibold,
     tracking-tight) so "you are here" reads as the same family as
     "this links to another note" — one primary palette, one weight. -->
<bind tag="link-class">hover:bg-primary-50/70 dark:hover:bg-primary-950/50 hover:text-primary-600 dark:hover:text-primary-300 transition-colors</bind>
<node:active>
  <bind tag="link-class">font-semibold tracking-tight bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 hover:bg-primary-100/80 dark:hover:bg-primary-900/70 transition-colors</bind>
  <else />
  <node:activeTree>
    <has-current-route>
      <bind tag="link-class">font-semibold tracking-tight hover:bg-primary-50/70 dark:hover:bg-primary-950/50 hover:text-primary-600 dark:hover:text-primary-300 transition-colors</bind>
    </has-current-route>
  </node:activeTree>
</node:active>

<has-children>
  <!-- Demoted toggle: light-gray chevron rather than a filled circle.
       The eye lands on the title now, not on the toggle. -->
  <bind tag="icon">
    <tree:open>
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"
        class="${iconSize} inline text-gray-400 dark:text-gray-500" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round">
        <polyline points="6 9 12 15 18 9" />
      </svg>
      <else />
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"
        class="${iconSize} inline text-gray-400 dark:text-gray-500" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round">
        <polyline points="9 6 15 12 9 18" />
      </svg>
    </tree:open>
  </bind>
  <else />
  <!-- Leaf node marker: a tiny dot centered in the toggle column.
       Less visual weight than a file/document icon — the chevron
       column already disambiguates folder vs leaf by presence of
       the chevron, and leaves don't need their own glyph competing
       with the title for attention. -->
  <bind tag="icon">
    <span class="${iconSize} inline-flex items-center justify-center" aria-hidden="true">
      <span class="block w-1 h-1 rounded-full bg-gray-300 dark:bg-gray-700"></span>
    </span>
  </bind>
</has-children>

<!-- Rendering of this tree -->
<div class="pl-2">
  <!-- Node's rootLabel-->
  <div class="flex items-center my-1.5 space-x-2 justify-left">
    <span class="flex-shrink-0">
      <tree:open>
        <icon />
        <else />
        <has-children>
          <a href="${node:url}" title="View folgezettel children" class="hover:opacity-70 transition-opacity">
            <icon />
          </a>
          <else />
          <icon />
        </has-children>
      </tree:open>
    </span>
    <a class="${link-class} rounded-md px-2 py-1 truncate flex-1" title="${node:text}" href="${node:url}">
      <node:text />
    </a>
    <tree:open>
      <else />
      <node:terminal>
        <else />
        <span class="text-xs text-gray-400 dark:text-gray-500 flex-shrink-0" title="${tree:childrenCount} children inside">
          <tree:childrenCount />
        </span>
      </node:terminal>
    </tree:open>
  </div>

  <!-- Node's children forest, displayed only on active trees
    TODO: Use <details> to toggle visibility?
  -->
  <tree:open>
    <children>
      <apply template="sidebar-tree" />
    </children>
  </tree:open>
</div>