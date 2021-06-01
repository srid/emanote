<ema:breadcrumbs>
  <div class="w-full p-4 mt-4 text-gray-700 bg-${theme}-500 rounded">
    <div class="flex justify-center">
      <div class="w-full py-2 rounded">
        <ul class="flex text-sm lg:text-base">
          <each-crumb>
            <li class="inline-flex items-center">
              <a class="px-1 font-bold bg-gray-100 rounded" href="${crumb:url}">
                <crumb:title />
              </a>
              <svg fill="currentColor" viewBox="0 0 20 20" class="w-auto h-5 text-gray-400">
                <path fill-rule="evenodd"
                  d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                  clip-rule="evenodd"></path>
              </svg>
            </li>
          </each-crumb>
          <li class="inline-flex items-center text-gray-200 font-bold">
            <a>
              <ema:note:title />
            </a>
          </li>
        </ul>
      </div>
    </div>
  </div>
</ema:breadcrumbs>