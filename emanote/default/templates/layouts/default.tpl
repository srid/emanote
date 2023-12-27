<!-- DoNotFormat -->
<bind tag="containerClass"><ema:metadata><value var="template.base.containerClass" /></ema:metadata> <ema:has:sidebar>md:mt-8 <else/>mt-2 <ema:has:uptree>md:mt-4 <else/> md:mt-8 </ema:has:uptree> </ema:has:sidebar></bind>
<!-- DoNotFormat -->

<bind tag="storkSearchButtonTopRight">
  <div class="absolute -top-6 right-1 md:right-0 flex flex-row items-center justify-center">
    <a title="Search (Ctrl+K)" class="cursor-pointer" onclick="window.emanote.stork.toggleSearch()">
      <apply template="components/stork/stork-icon" />
    </a>
  </div>
</bind>


<apply template="base">
  <bind tag="head-main">
    <style>
      /* For use in sidebar.tpl, as we cannot achieve this in tailwind itself! */
      #sidebar:hover {
        width: auto;
      }

      /* md:min-w-48  */
      @media (min-width: 768px) {
        #sidebar {
          min-width: 12rem;
        }
      }

      /* xl:min-w-64  */
      @media (min-width: 1280px) {
        #sidebar {
          min-width: 16rem;
        }
      }
    </style>

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
          class="flex flex-nowrap flex-col md:flex-row bg-gray-50 md:shadow-2xl md:mb-8">
          <!-- Sidebar column -->
          <apply template="components/sidebar" />
          <!-- Main body column -->
          <apply template="components/body" />
        </div>
        <else />
        <div id="container" class="relative md:shadow-2xl md:mb-8">
          <storkSearchButtonTopRight />
          <!-- Main body column -->
          <apply template="components/body" />
        </div>
      </ema:has:sidebar>

      <apply template="components/footer" />
    </div>
  </bind>
</apply>