<!-- Variable bindings for this tree-->
<bind tag="link-class">hover:bg-${theme}-50 dark:hover:bg-${theme}-950 hover:text-${theme}-700 dark:hover:text-${theme}-300 transition-colors</bind>
<node:active>
  <bind tag="link-class">font-semibold bg-${theme}-50 dark:bg-${theme}-950 text-${theme}-700 dark:text-${theme}-300 hover:bg-${theme}-100 dark:hover:bg-${theme}-900 transition-colors</bind>
  <else />
  <node:activeTree>
    <has-current-route>
      <bind tag="link-class">font-semibold hover:bg-${theme}-50 dark:hover:bg-${theme}-950 hover:text-${theme}-700 dark:hover:text-${theme}-300 transition-colors</bind>
    </has-current-route>
  </node:activeTree>
</node:active>

<has-children>
  <bind tag="icon">
    <tree:open>
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"
        class="${iconSize} inline text-gray-700 dark:text-gray-300" fill="currentColor">
        <path fill-rule="evenodd"
          d="M12 2.25c-5.385 0-9.75 4.365-9.75 9.75s4.365 9.75 9.75 9.75 9.75-4.365 9.75-9.75S17.385 2.25 12 2.25Zm3 10.5a.75.75 0 0 0 0-1.5H9a.75.75 0 0 0 0 1.5h6Z"
          clip-rule="evenodd" />
      </svg>
      <else />
      <svg xmlns="http://www.w3.org/2000/svg" class="${iconSize} inline text-gray-500 dark:text-gray-400"
        viewBox="0 0 24 24" fill="currentColor">
        <path fill-rule="evenodd"
          d="M12 2.25c-5.385 0-9.75 4.365-9.75 9.75s4.365 9.75 9.75 9.75 9.75-4.365 9.75-9.75S17.385 2.25 12 2.25ZM12.75 9a.75.75 0 0 0-1.5 0v2.25H9a.75.75 0 0 0 0 1.5h2.25V15a.75.75 0 0 0 1.5 0v-2.25H15a.75.75 0 0 0 0-1.5h-2.25V9Z"
          clip-rule="evenodd" />
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