<div id="stork-search-container"
  class="hidden fixed w-screen h-screen inset-0 backdrop-filter backdrop-blur-sm">
  <div class="fixed w-screen h-screen inset-0" onclick="window.emanote.stork.toggleSearch()"></div>

  <div class="container mx-auto p-10 mt-10">
    <div class="stork-wrapper-flat container mx-auto">
      <input id="stork-search-input" data-stork="emanote-search" class="stork-input bg-white dark:bg-gray-800 text-black dark:text-white border-gray-300 dark:border-gray-600"
        placeholder="Search (Ctrl+K) ..." />
      <div data-stork="emanote-search-output" class="stork-output bg-white dark:bg-gray-800 text-black dark:text-white border-gray-300 dark:border-gray-600"></div>
    </div>
  </div>
</div>