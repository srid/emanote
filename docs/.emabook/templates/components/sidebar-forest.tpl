<bind tag="iconSize">w-4 h-4</bind>
<tree:active>
  <bind tag="children-class">bg-${theme}-100</bind>
  <else />
  <bind tag="children-class">hidden</bind>
</tree:active>
<div class="pl-2">
  <!-- Node's rootLabel-->
  <apply template="sidebar-forest-node">
    <node:active>
      <bind tag="link-class">font-bold text-${theme}-600 hover:text-black</bind>
      <else />
      <bind tag="link-class">hover:underline</bind>
    </node:active>

    <has-children>
      <bind tag="icon">
        <tree:active>
          <svg class="${iconSize} inline" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 19a2 2 0 01-2-2V7a2 2 0 012-2h4l2 2h4a2 2 0 012 2v1M5 19h14a2 2 0 002-2v-5a2 2 0 00-2-2H9a2 2 0 00-2 2v5a2 2 0 01-2 2z"></path></svg>
          <else />
          <svg class="${iconSize} inline" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"> <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z"></path> </svg>
        </tree:active>
      </bind>
      <else />
      <bind tag="icon">
        <node:active>
          <svg class="${iconSize} inline" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"></path></svg> 
          <else />
          <svg class="${iconSize} inline" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 21h10a2 2 0 002-2V9.414a1 1 0 00-.293-.707l-5.414-5.414A1 1 0 0012.586 3H7a2 2 0 00-2 2v14a2 2 0 002 2z"></path></svg>
        </node:active>
      </bind>
    </has-children>
  </apply>
  <!-- Node's children forest-->
  <div class="${children-class}">
    <children>
      <apply template="sidebar-forest" />
    </children>
  </div>
</div>