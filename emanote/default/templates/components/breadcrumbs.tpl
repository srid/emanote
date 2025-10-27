<nav id="breadcrumbs" class="w-full text-gray-700 dark:text-gray-300 md:hidden">
  <div class="flex justify-left">
    <div class="w-full px-4 py-3 bg-white dark:bg-gray-900 border-b border-gray-200 dark:border-gray-800">
      <ul class="flex flex-wrap text-base">
        <li class="inline-flex items-center">
          <ema:metadata>
            <with var="template">
              <img style="width: 1rem;" src="${value:iconUrl}" alt="Site Icon" />
            </with>
          </ema:metadata>
        </li>
        <ema:breadcrumbs>
          <each-crumb>
            <li class="inline-flex items-center">
              <a class="px-1 font-bold" href="${crumb:url}">
                <crumb:title />
              </a>
              <svg fill="currentColor" viewBox="0 0 20 20" class="w-auto h-5 text-gray-400 dark:text-gray-500">
                <path fill-rule="evenodd"
                  d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                  clip-rule="evenodd"></path>
              </svg>
            </li>
          </each-crumb>
        </ema:breadcrumbs>
      </ul>
    </div>
    <button class="inline px-3 py-2 bg-white dark:bg-gray-900 border-b border-gray-200 dark:border-gray-800 outline-none cursor-pointer focus:outline-none hover:bg-gray-50 dark:hover:bg-gray-800 transition-colors"
      title="Search (Ctrl+K)" type="button" onclick="window.emanote.stork.toggleSearch()">
      <apply template="stork/stork-icon" />
    </button>
    <button
      class="inline px-3 py-2 text-white bg-${theme}-600 dark:bg-${theme}-700 outline-none cursor-pointer focus:outline-none hover:bg-${theme}-700 dark:hover:bg-${theme}-800 transition-colors"
      title="Toggle sidebar" type="button" onclick="toggleHidden('sidebar')">
      <svg xmlns="http://www.w3.org/2000/svg" class="w-4" fill="none" viewBox="0 0 24 24"
        stroke="currentColor">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
          d="M4 6h16M4 12h16M4 18h16" />
      </svg>
    </button>
    <script>
      function toggleHidden(elemId) {
        document.getElementById(elemId).classList.toggle("hidden");
      }
    </script>
  </div>
</nav>