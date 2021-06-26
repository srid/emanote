<!-- Variable bindings for this tree-->
<node:active>
  <bind tag="link-class">font-bold text-${theme}-600 hover:underline</bind>
  <else />
  <tree:open>
    <has-children>
      <bind tag="link-class">font-bold hover:underline</bind>
      <else />
      <bind tag="link-class">hover:underline</bind>
    </has-children>
    <else />
    <bind tag="link-class">hover:underline</bind>
  </tree:open>
</node:active>

<has-children>
  <bind tag="icon">
    <tree:open>
      <svg xmlns="http://www.w3.org/2000/svg" class="${iconSize} inline text-gray-700"
        viewBox="0 0 20 20" fill="currentColor">
        <path fill-rule="evenodd"
          d="M2 6a2 2 0 012-2h4l2 2h4a2 2 0 012 2v1H8a3 3 0 00-3 3v1.5a1.5 1.5 0 01-3 0V6z"
          clip-rule="evenodd" />
        <path d="M6 12a2 2 0 012-2h8a2 2 0 012 2v2a2 2 0 01-2 2H2h2a2 2 0 002-2v-2z" />
      </svg>
      <else />
      <svg xmlns="http://www.w3.org/2000/svg" class="${iconSize} inline text-gray-500"
        viewBox="0 0 20 20" fill="currentColor">
        <path d="M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z" />
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
  <div class="flex items-center my-2 space-x-2 justify-left">
    <icon />
    <a class="${link-class} truncate" title="${node:text}" href="${node:url}">
      <node:text />
    </a>
    <tree:open>
      <else />
      <node:terminal>
        <else />
        <span class="text-gray-300" title="${tree:childrenCount} children inside">
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