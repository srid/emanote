<!-- TODO: A way to disable collapsing; ideally on per node basis.-->
<ema:route-tree>
  <forest class="pl-2" inactive:class="hidden" active:class="bg-${theme}-100" />
  <!-- Make this an inner splice, so as to render folder icon-->
  <node parent:class="my-2" terminal:class="my-2 text-gray-600" />
  <link active:class="font-bold text-${theme}-600 hover:text-black"
    inactive:class="hover:bg-${theme}-200" />
</ema:route-tree>