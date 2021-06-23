<!-- TODO: DRY (same as query-default.tpl, except for date column)-->
<nav>
  <!-- This should be a grid -->
  <div class="grid grid-cols-12 gap-2 mb-8">
    <span class="col-span-3"></span>
    <header class="col-span-9 pb-2 mb-2 font-semibold text-gray-600 border-b-2">
      <query />
    </header>
    <result>
      <ema:note:metadata>
        <span class="col-span-3 pr-2 mr-2 text-right text-gray-600 border-r-2">
          <value var="date" />
        </span>
      </ema:note:metadata>
      <a class="col-span-9 text-${theme}-600 font-bold hover:underline" href="${ema:note:url}">
        <ema:note:title />
      </a>
    </result>
  </div>
</nav>