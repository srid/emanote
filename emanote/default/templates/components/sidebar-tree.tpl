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
  <bind tag="icon">
    <node:active>
      <svg class="${iconSize} inline" fill="none" stroke="currentColor" viewBox="0 0 24 24"
        xmlns="http://www.w3.org/2000/svg">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
          d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z">
        </path>
      </svg>
      <else />
      <svg class="${iconSize} inline" fill="none" stroke="currentColor" viewBox="0 0 24 24"
        xmlns="http://www.w3.org/2000/svg">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
          d="M7 21h10a2 2 0 002-2V9.414a1 1 0 00-.293-.707l-5.414-5.414A1 1 0 0012.586 3H7a2 2 0 00-2 2v14a2 2 0 002 2z">
        </path>
      </svg>
    </node:active>
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