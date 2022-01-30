<apply template="/templates/special/base">
  <bind tag="special-main">
    <div class="w-full bg-gray-300">
      <div class="w-full px-3 py-1">
        <div class="border-2 bg-white py-1 px-2">
          Searching for: <tt>
            <query:string />
          </tt>
          <!-- TODO: copied from query-default.tpl; reuse or?-->
          <div class="mb-8">
            <header class="pb-2 mb-2 font-semibold text-gray-600">
              <query />
            </header>
            <result>
              <div class="flex flex-wrap my-2">
                <ema:note:metadata>
                  <span class="mr-2 text-right text-gray-600">
                  </span>
                </ema:note:metadata>
                <a class="flex-1 text-${theme}-600 mavenLinkBold border-l-2 pl-2 hover:underline"
                  href="${ema:note:url}">
                  <ema:note:title />
                </a>
              </div>
            </result>
          </div>
        </div>
      </div>
    </div>
  </bind>
</apply>