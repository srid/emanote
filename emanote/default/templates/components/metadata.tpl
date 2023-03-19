<ema:metadata>
  <section
    class="flex flex-wrap items-end justify-center my-4 space-x-2 space-y-2 font-mono text-sm">
    <with var="tags">
      <!-- FIXME: The use of -/tags is wrong, because we should use routeUrl using Ema's encoder 
        Perhaps Emanote should inject tagMetas with urls.
      -->
      <a title="Tag" class="px-1 bg-gray-100 rounded hover:bg-gray-50 hover:text-${theme}-500"
        href="-/tags/${value}${ema:urlStrategySuffix}">
        <!-- DoNotFormat -->
        #<value />
        <!-- DoNotFormat -->
      </a>
    </with>
  </section>
</ema:metadata>