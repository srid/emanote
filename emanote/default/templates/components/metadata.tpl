<section
  class="flex flex-wrap items-end justify-center my-4 space-x-2 space-y-2 font-mono text-sm">
  <ema:tagMetas>
    <ema:each-tagMeta>
      <a title="Tag" class="px-1 bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-200 rounded hover:bg-gray-50 dark:hover:bg-gray-600 hover:text-primary-500"
        href="${tag:url}">
        <!-- DoNotFormat -->
        #<tag:value />
        <!-- DoNotFormat -->
      </a>
    </ema:each-tagMeta>
  </ema:tagMetas>
</section>
