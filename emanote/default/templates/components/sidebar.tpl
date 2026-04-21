<nav id="sidebar"
  class="flex-shrink hidden leading-relaxed md:block md:sticky md:top-0 md:max-h-screen md:overflow-y-auto md:w-52 md:min-w-52 xl:w-72 xl:min-w-72 bg-gray-50 dark:bg-gray-950 border-r border-gray-200 dark:border-gray-800">
  <div class="px-4 py-5 text-gray-800 dark:text-gray-200">
    <div class="flex items-center justify-between mb-4">
      <div id="site-logo" class="flex items-center space-x-2.5 min-w-0">
        <ema:metadata>
          <with var="template">
            <a href="${value:baseUrl}" title="Go to Home" class="flex-shrink-0">
              <!-- The style width attribute here is to prevent huge
                      icon from displaying at those rare occasions when Tailwind
                      hasn't kicked in immediately on page load
                      -->
              <img style="width: 1.25rem;"
                class="transition transform hover:scale-110 hover:opacity-80" src="${value:iconUrl}"
                alt="Site Icon" />
            </a>
            <a class="font-semibold text-base truncate hover:text-primary-600 dark:hover:text-primary-400 transition-colors" title="Go to Home" href="${value:baseUrl}">
              Home
            </a>
          </with>
        </ema:metadata>
      </div>

      <div id="indexing-links" class="flex flex-row space-x-2 text-gray-500 dark:text-gray-400 flex-shrink-0">
        <button title="Search (Ctrl+K)" class="p-2 rounded-md hover:bg-gray-100 dark:hover:bg-gray-800 hover:text-primary-600 dark:hover:text-primary-400 transition-colors cursor-pointer"
          onclick="window.emanote.stork.toggleSearch()">
          <apply template="stork/stork-icon" />
        </button>
        <button title="Toggle dark mode" class="p-2 rounded-md hover:bg-gray-100 dark:hover:bg-gray-800 hover:text-primary-600 dark:hover:text-primary-400 transition-colors cursor-pointer"
          onclick="window.emanote.theme.toggle()">
          <svg class="hidden dark:block" style="width: 1.125rem;" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z" />
          </svg>
          <svg class="block dark:hidden" style="width: 1.125rem;" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z" />
          </svg>
        </button>
      </div>
    </div>

    <ema:route-tree>
      <apply template="sidebar-tree" />
    </ema:route-tree>

  </div>
</nav>