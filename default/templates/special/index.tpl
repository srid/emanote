<apply template="base">
  <bind tag="body-main">
    <div class="container mx-auto lg:max-w-screen-md">
      <main class="mx-2">
        <h1 class="pb-2 mt-2 mb-2 text-6xl text-center">
          <ema:title />
        </h1>
        <apply template="experimental">
          <bind tag="dnum">50</bind>
        </apply>
        <div>
          <div class="float-right p-2 text-gray-500 hover:text-${theme}-700">
            <a href="/" title="Go to Home">
              <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"
                xmlns="http://www.w3.org/2000/svg">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                  d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6">
                </path>
              </svg>
            </a>
          </div>
          <div class="pt-1 pb-2 rounded bg-${theme}-50 pl-4">
            <ema:route-tree>
              <apply template="components/sidebar-tree" />
            </ema:route-tree>
          </div>
        </div>
      </main>
      <apply template="components/footer" />
    </div>
  </bind>
</apply>