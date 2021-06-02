<apply template="base">
  <bind tag="body-main">
    <div class="container mx-auto xl:max-w-screen-lg">
      <main>
        <h2>index</h2>
        <ema:route-tree>
          <apply template="components/sidebar-tree" />
        </ema:route-tree>
      </main>
      <footer
        class="flex flex-col items-center justify-center my-8 space-x-4 text-center text-gray-500">
        <div class="flex items-center justify-center h-full pl-2 mt-2 space-x-2">
          <a href="https://note.ema.srid.ca" target="_blank">
            <ema:metadata>
              <with var="template">
                <img class="z-50 h-20 transition transform hover:scale-125 hover:opacity-80"
                  src="${value:iconUrl}" />
              </with>
            </ema:metadata>
          </a>
        </div>

      </footer>
    </div>
  </bind>
</apply>