<!-- DoNotFormat -->
<bind tag="containerClass"><ema:metadata><value var="template.layout.book.containerClass" /></ema:metadata></bind>
<!-- DoNotFormat -->

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
      <div class="md:mt-8">
        <ema:has:uptree>
          <apply template="components/note-uptree" />
        </ema:has:uptree>

        <ema:has:breadcrumbs>
          <apply template="components/breadcrumbs" />
        </ema:has:breadcrumbs>

        <div id="container"
          class="flex flex-nowrap flex-col md:flex-row bg-gray-50 md:shadow-2xl md:mb-8">

          <ema:has:sidebar>
            <!-- Sidebar column -->
            <apply template="components/sidebar" />
          </ema:has:sidebar>
          <!-- Main body column -->
          <apply template="components/body" />
        </div>
      </div>

      <apply template="components/footer" />
    </div>
  </bind>
</apply>