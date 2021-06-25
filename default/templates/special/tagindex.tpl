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
            <ema:tagcrumbs>
              <nav id="tagcrumbs" class="w-full text-gray-700 ">
                <div class="flex justify-left">
                  <div class="w-full px-2 py-2 bg-gray-50">
                    <ul class="flex flex-wrap text-lg">
                      <ema:each-crumb>
                        <li class="inline-flex items-center">
                          <a class="px-1 font-bold" href="${ema:tagcrumb:url}">
                            <ema:tagcrumb:title />
                          </a>
                          <svg fill="currentColor" viewBox="0 0 20 20"
                            class="w-auto h-5 text-gray-400">
                            <path fill-rule="evenodd"
                              d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                              clip-rule="evenodd"></path>
                          </svg>
                        </li>
                      </ema:each-crumb>
                    </ul>
                  </div>
                </div>
              </nav>
            </ema:tagcrumbs>
            <div class="flex flex-row">
              <ema:childTags>
                <div>
                  <header>Child Tags</header>
                  <ema:each-childTag>
                    <div class="flex flex-col">
                      <a href="${ema:childTag:url}">
                        <!-- DoNotFormat -->
                        #<ema:childTag:title />
                        <!-- DoNotFormat -->
                      </a>
                    </div>
                  </ema:each-childTag>
                </div>
              </ema:childTags>
              <ema:notes>
                <div>
                  <h2 id="${ema:tag}" title="Tag"
                    class="py-1 font-mono font-bold text-gray-700 text-l">
                    <a href="-/tags/${ema:tag}">
                      <!-- DoNotFormat -->
                      #<ema:tag />
                      <!-- DoNotFormat -->
                    </a>
                  </h2>
                  <div class="flex flex-col mb-4 ml-2">
                    <ema:notes>
                      <ema:each-note>
                        <div>
                          <!-- TODO: DRY -->
                          <a class="font-bold text-${theme}-600 hover:underline truncate"
                            href="${ema:note:url}">
                            <ema:note:title />
                          </a>
                        </div>
                      </ema:each-note>
                    </ema:notes>
                  </div>
                </div>
              </ema:notes>
            </div>
          </div>
        </div>
      </main>
      <apply template="components/footer" />
    </div>
  </bind>
</apply>