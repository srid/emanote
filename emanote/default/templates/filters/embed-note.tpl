<section title="Embedded note" class="p-4 mx-2 mb-2 bg-white dark:bg-gray-800 border-2 border-gray-200 dark:border-gray-700 rounded-lg shadow-inner">
  <header
    class="flex items-center justify-center text-2xl italic bg-${theme}-50 dark:bg-${theme}-900 rounded py-1 px-2 mb-3">
    <a href="${ema:note:url}">
      <ema:note:title />
    </a>
  </header>
  <div>
    <apply template="/templates/components/pandoc" />
  </div>
</section>