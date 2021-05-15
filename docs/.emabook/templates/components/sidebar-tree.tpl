<!-- TODO: A way to disable collapsing; ideally on per node basis.-->
<ema:route-tree>
  <tree class="pl-2" />
  <tree:inactive class="hidden" />
  <tree:active class="bg-${theme}-100" />
  <!-- Make this an inner splice, so as to render folder icon-->
  <item:parent class="my-2" />
  <item:terminal class="my-2 text-gray-600" />
  <link:active class="font-bold text-${theme}-600 hover:text-black" />
  <link:inactive class="hover:text-black" />
</ema:route-tree>