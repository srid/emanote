<nav id="sidebar"
  class="flex-shrink hidden leading-relaxed md:block md:sticky md:top-0 md:max-h-screen md:overflow-y-auto md:w-52 xl:w-72 bg-gray-50 dark:bg-gray-950 border-r border-gray-200 dark:border-gray-800">
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
            <a class="font-semibold text-base truncate hover:text-${theme}-600 dark:hover:text-${theme}-400 transition-colors" title="Go to Home" href="${value:baseUrl}">
              Home
            </a>
          </with>
        </ema:metadata>
      </div>

      <div id="indexing-links" class="flex flex-row space-x-2 text-gray-500 dark:text-gray-400 flex-shrink-0">
        <a href="${ema:tagIndexUrl}" title="View tags" class="p-2 rounded-md hover:bg-gray-100 dark:hover:bg-gray-800 hover:text-${theme}-600 dark:hover:text-${theme}-400 transition-colors">
          <svg style="width: 1.125rem;" fill="none" stroke="currentColor"
            viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
              d="M7 7h.01M7 3h5c.512 0 1.024.195 1.414.586l7 7a2 2 0 010 2.828l-7 7a2 2 0 01-2.828 0l-7-7A1.994 1.994 0 013 12V7a4 4 0 014-4z">
            </path>
          </svg>
        </a>
        <a href="${ema:indexUrl}" title="Expand full tree" class="p-2 rounded-md hover:bg-gray-100 dark:hover:bg-gray-800 hover:text-${theme}-600 dark:hover:text-${theme}-400 transition-colors">
          <svg style="width: 1.125rem;" fill="none" stroke="currentColor"
            viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
              d="M4 8V4m0 0h4M4 4l5 5m11-1V4m0 0h-4m4 0l-5 5M4 16v4m0 0h4m-4 0l5-5m11 5l-5-5m5 5v-4m0 4h-4">
            </path>
          </svg>
        </a>
        <button title="Search (Ctrl+K)" class="p-2 rounded-md hover:bg-gray-100 dark:hover:bg-gray-800 hover:text-${theme}-600 dark:hover:text-${theme}-400 transition-colors cursor-pointer"
          onclick="window.emanote.stork.toggleSearch()">
          <apply template="stork/stork-icon" />
        </button>
      </div>
    </div>

    <ema:route-tree>
      <apply template="sidebar-tree" />
    </ema:route-tree>

  </div>
</nav>