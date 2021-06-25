<ema:breadcrumbs>
  <nav id="breadcrumbs" class="w-full text-gray-700 md:hidden">
    <div class="flex justify-left">
      <div class="w-full px-2 py-2 bg-gray-50">
        <ul class="flex flex-wrap text-lg">
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
  </nav>
</ema:breadcrumbs>