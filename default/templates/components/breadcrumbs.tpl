<ema:breadcrumbs>
  <div class="w-full text-gray-700">
    <div class="flex justify-left">
      <div class="bg-${theme}-100 md:rounded-b-lg w-full py-2 px-2 md:w-auto md:py-0 md:px-0">
        <ul class="flex flex-wrap text-lg md:text-sm">
          <each-crumb>
            <li class="inline-flex items-center">
              <a class="px-1 font-bold" href="${crumb:url}">
                <crumb:title />
              </a>
              <svg fill="currentColor" viewBox="0 0 20 20" class="w-auto h-5 text-gray-400">
                <path fill-rule="evenodd"
                  d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                  clip-rule="evenodd"></path>
              </svg>
            </li>
          </each-crumb>
        </ul>
      </div>
    </div>
  </div>

</ema:breadcrumbs>
<h1 class="flex items-center justify-center mt-2 text-4xl font-extrabold text-black bg-white">
  <a>
    <ema:note:title />
  </a>
</h1>