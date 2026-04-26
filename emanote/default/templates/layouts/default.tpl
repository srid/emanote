<!-- DoNotFormat -->
<bind tag="containerClass"><ema:metadata><value var="template.base.containerClass" /></ema:metadata> <ema:has:sidebar>md:mt-8 <else/>mt-2 <ema:has:uptree>md:mt-4 <else/> md:mt-8 </ema:has:uptree> </ema:has:sidebar></bind>
<!-- DoNotFormat -->

<bind tag="storkSearchButtonTopRight">
  <div class="absolute -top-6 right-1 md:right-0 flex flex-row items-center justify-center space-x-2 text-gray-500 dark:text-gray-400">
    <button title="Search (Ctrl+K)" class="cursor-pointer" data-emanote-stork-toggle>
      <apply template="components/stork/stork-icon" />
    </button>
    <button title="Toggle dark mode" class="cursor-pointer hover:text-primary-600 dark:hover:text-primary-400 transition-colors"
      onclick="window.emanote.theme.toggle()">
      <svg class="hidden dark:block" style="width: 1.125rem;" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z" />
      </svg>
      <svg class="block dark:hidden" style="width: 1.125rem;" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z" />
      </svg>
    </button>
  </div>
</bind>


<apply template="base">
  <bind tag="head-main">
    <ema:has:uptree>
      <link rel="stylesheet" href="${ema:emanoteStaticLayerUrl}/inverted-tree.css" />
    </ema:has:uptree>
  </bind>

  <bind tag="body-main">
    <div class="${containerClass}">
      <ema:has:uptree>
        <apply template="components/note-uptree" />
      </ema:has:uptree>

      <ema:has:breadcrumbs>
        <apply template="components/breadcrumbs" />
      </ema:has:breadcrumbs>

      <ema:has:sidebar>
        <div id="container"
          class="flex flex-nowrap flex-col md:flex-row bg-white dark:bg-gray-900 md:shadow-md md:rounded-lg md:mb-8 md:border md:border-gray-200 dark:md:border-gray-800">
          <!-- Sidebar column -->
          <apply template="components/sidebar" />
          <!-- Main body column -->
          <apply template="components/body" />
        </div>
        <else />
        <div id="container" class="relative md:shadow-md md:rounded-lg md:mb-8 bg-white dark:bg-gray-900 md:border md:border-gray-200 dark:md:border-gray-800">
          <storkSearchButtonTopRight />
          <!-- Main body column -->
          <apply template="components/body" />
        </div>
      </ema:has:sidebar>
      <apply template="components/footer" />
    </div>
  </bind>
</apply>
