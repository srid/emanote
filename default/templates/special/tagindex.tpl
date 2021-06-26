<apply template="base">
  <bind tag="body-main">
    <div class="container mx-auto ">
      <main class="flex-col items-center justify-center mx-2">
        <h1 class="pb-2 mt-2 mb-2 text-6xl text-center">
          <ema:title />
        </h1>
        <apply template="experimental">
          <bind tag="dnum">50</bind>
        </apply>
        <div class="pb-2 mx-auto my-4 bg-gray-50 lg:max-w-screen-md ">
          <nav id="tagcrumbs" class="w-full pl-2 text-gray-100 bg-${theme}-500">
            <div class="flex items-center justify-left">
              <div class="w-full px-2 py-2 ">
                <ul class="flex flex-wrap text-lg">
                  <ema:tagcrumbs>
                    <ema:each-crumb>
                      <li class="inline-flex items-center">
                        <a class="px-1 font-mono font-semibold" href="${ema:tagcrumb:url}">
                          <ema:tagcrumb:title />
                        </a>
                        <svg fill="currentColor" viewBox="0 0 20 20" class="w-auto h-5 ">
                          <path fill-rule="evenodd"
                            d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                            clip-rule="evenodd"></path>
                        </svg>
                      </li>
                    </ema:each-crumb>
                  </ema:tagcrumbs>
                  <li class="inline-flex items-center text-black">
                    <a class="px-1 font-mono font-semibold">
                      <ema:tag:title />
                    </a>
                  </li>
                </ul>
              </div>
            </div>
          </nav>
          <div class="flex flex-col pl-2 my-4 ml-2 space-y-2">
            <ema:childTags>
              <ema:each-childTag>
                <div>
                  <svg class="${iconSize} inline" fill="none" stroke="currentColor"
                    viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                      d="M7 7h.01M7 3h5c.512 0 1.024.195 1.414.586l7 7a2 2 0 010 2.828l-7 7a2 2 0 01-2.828 0l-7-7A1.994 1.994 0 013 12V7a4 4 0 014-4z">
                    </path>
                  </svg>
                  <span>
                    <a href="${ema:childTag:url}" clas="font-mono font-semibold">
                      <ema:childTag:title />
                    </a>
                    <span class="font-mono text-sm text-gray-500">
                      <!-- DoNotFormat -->
                      (<ema:childTag:count />)
                      <!-- DoNotFormat -->
                    </span>
                  </span>
                </div>
              </ema:each-childTag>
            </ema:childTags>
            <ema:notes>
              <ema:each-note>
                <div>
                  <svg class="${iconSize} inline" fill="none" stroke="currentColor"
                    viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                      d="M7 21h10a2 2 0 002-2V9.414a1 1 0 00-.293-.707l-5.414-5.414A1 1 0 0012.586 3H7a2 2 0 00-2 2v14a2 2 0 002 2z">
                    </path>
                  </svg>

                  <!-- TODO: DRY -->
                  <a class="text-${theme}-600 hover:underline truncate" href="${ema:note:url}">
                    <ema:note:title />
                  </a>
                </div>
              </ema:each-note>
            </ema:notes>
          </div>
        </div>
      </main>
      <apply template="components/footer" />
    </div>
  </bind>
</apply>