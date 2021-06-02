<apply template="base">
  <bind tag="body-main">
    <div class="container mx-auto xl:max-w-screen-lg">
      <!-- Header row-->
      <div class="grid grid-cols-12 px-2">
        <!-- Logo column -->
        <div class="hidden md:mr-4 md:block md:col-span-3">
          <div class="flex items-end h-full pl-2 mt-2 space-x-2">
            <a href="">
              <ema:metadata>
                <with var="template">
                  <img class="z-50 h-20 transition transform hover:scale-125 hover:opacity-80"
                    src="${value:iconUrl}" />
                </with>
              </ema:metadata>
            </a>
          </div>
        </div>
        <!-- Title column -->
        <div class="col-span-12 md:col-span-9">

          <div class="flex items-center justify-center">
            <h1 class="pb-2 mt-2 mb-2 text-6xl text-center">
              <ema:note:title />
            </h1>
          </div>
        </div>
      </div>

      <!-- Main row-->
      <div class="grid grid-cols-12 px-2">
        <!-- Sidebar column -->
        <div
          class="hidden overflow-x-auto md:mr-4 md:block md:col-span-3 md:sticky md:top-0 md:h-screen">
          <div class="pt-1 pb-2 rounded bg-${theme}-50">
            <ema:route-tree>
              <apply template="components/sidebar-tree" />
            </ema:route-tree>
          </div>
        </div>
        <!-- Main body column -->
        <div class="col-span-12 md:col-span-9">
          <apply template="components/pandoc" />
          <apply template="components/backlinks" />

          <apply template="components/breadcrumbs" />

          <note-meta>
            <div class="flex items-center justify-center mt-8 space-x-2 font-mono text-sm">
              <with var="tags">
                <a title="Tag" class="px-1 bg-gray-100 rounded">
                  <value />
                </a>
              </with>
            </div>
          </note-meta>

          <apply template="components/footer" />
        </div>
      </div>
    </div>
  </bind>
</apply>