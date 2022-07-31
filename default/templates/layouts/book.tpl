<apply template="base">
  <bind tag="head-main"></bind>
  <bind tag="body-main">
    <div class="container mx-auto">

      <apply template="components/breadcrumbs" />

      <div id="container"
        class="flex flex-nowrap flex-col md:flex-row bg-gray-50 md:mt-8 md:shadow-2xl md:mb-8">
        <!-- Sidebar column -->
        <nav id="sidebar"
          class="flex-shrink hidden leading-relaxed md:block md:sticky md:top-0 md:h-full md:w-48 xl:w-64">
          <apply template="components/stork-search" />
          <div class="px-2 py-2 text-gray-800">

            <div id="indexing-links" class="flex flex-row float-right p-2 space-x-2 text-gray-500">
              <a href="${ema:tagIndexUrl}" title="View tags">
                <svg style="width: 1rem;" class="hover:text-${theme}-700" fill="none"
                  stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                    d="M7 7h.01M7 3h5c.512 0 1.024.195 1.414.586l7 7a2 2 0 010 2.828l-7 7a2 2 0 01-2.828 0l-7-7A1.994 1.994 0 013 12V7a4 4 0 014-4z">
                  </path>
                </svg>
              </a>
              <a href="${ema:indexUrl}" title="Expand full tree">
                <svg style="width: 1rem;" class="hover:text-${theme}-700" fill="none"
                  stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                    d="M4 8V4m0 0h4M4 4l5 5m11-1V4m0 0h-4m4 0l-5 5M4 16v4m0 0h4m-4 0l5-5m11 5l-5-5m5 5v-4m0 4h-4">
                  </path>
                </svg>
              </a>
            </div>

            <div id="site-logo" class="pl-2">
              <div class="flex items-center my-2 space-x-2 justify-left">
                <a href="${ema:homeUrl}" title="Go to Home">
                  <ema:metadata>
                    <with var="template">
                      <!-- The style width attribute here is to prevent huge
                      icon from displaying at those rare occasions when Tailwind
                      hasn't kicked in immediately on page load 
                      -->
                      <img style="width: 1rem;"
                        class="transition transform hover:scale-110 hover:opacity-80"
                        src="${value:iconUrl}" />
                    </with>
                  </ema:metadata>
                </a>
                <a class="font-bold truncate" title="Go to Home" href="${ema:homeUrl}">
                  Home
                </a>
              </div>
            </div>

            <ema:route-tree>
              <apply template="components/sidebar-tree" />
            </ema:route-tree>

          </div>
        </nav>

        <!-- Main body column -->
        <div class="flex-1 w-full overflow-x-auto bg-white">
          <main class="px-4 py-4">
            <apply template="components/note-title" />
            <apply template="components/note-body" />
            <div class="flex flex-col lg:flex-row lg:space-x-2">
              <apply template="components/timeline" />
              <apply template="components/backlinks" />
            </div>
            <apply template="components/metadata" />
            <apply template="/templates/hooks/note-end" />
          </main>
        </div>
      </div>
      <apply template="components/footer" />
    </div>
  </bind>
</apply>
