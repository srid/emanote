<apply template="base">
  <bind tag="body-main">
    <div class="container mx-auto">

      <div id="site-header" class="grid grid-cols-12">
        <div id="site-logo" class="hidden col-span-3 pl-2 mt-2 space-x-2 md:block">
          <a href="">
            <ema:metadata>
              <with var="template">
                <img class="z-50 h-20 transition transform hover:scale-110 hover:opacity-80"
                  src="${value:iconUrl}" />
              </with>
            </ema:metadata>
          </a>
        </div>
        <div class="col-span-12 md:col-span-9">

          <apply template="components/breadcrumbs" />
        </div>
      </div>

      <div class="grid grid-cols-12 md:shadow-2xl md:rounded-lg md:mb-8">
        <!-- Sidebar column -->
        <nav id="sidebar"
          class="hidden leading-relaxed md:block md:col-span-3 md:sticky md:top-0 md:h-full bg-gray-50">

          <div class="px-2 pt-1 text-gray-800">
            <div id="indexing-links" class="flex flex-row float-right p-2 space-x-2 text-gray-500">
              <a href="@tags" title="View tags">
                <svg class="w-5 h-5 hover:text-${theme}-700" fill="none" stroke="currentColor"
                  viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                    d="M7 7h.01M7 3h5c.512 0 1.024.195 1.414.586l7 7a2 2 0 010 2.828l-7 7a2 2 0 01-2.828 0l-7-7A1.994 1.994 0 013 12V7a4 4 0 014-4z">
                  </path>
                </svg>
              </a>
              <a href="@index" title="Expand full tree">
                <svg class="w-5 h-5 hover:text-${theme}-700" fill="none" stroke="currentColor"
                  viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                    d="M4 8V4m0 0h4M4 4l5 5m11-1V4m0 0h-4m4 0l-5 5M4 16v4m0 0h4m-4 0l5-5m11 5l-5-5m5 5v-4m0 4h-4">
                  </path>
                </svg>
              </a>
            </div>
            <ema:route-tree>
              <apply template="components/sidebar-tree" />
            </ema:route-tree>
          </div>
        </nav>
        <!-- Main body column -->
        <div class="col-span-12 px-0 pt-3 bg-white md:col-span-9 ">
          <main class="px-2 md:pl-4">
            <apply template="components/note-body" />
            <apply template="components/backlinks" />
            <apply template="components/metadata" />
          </main>
        </div>
      </div>
      <apply template="components/footer" />
    </div>
  </bind>
</apply>