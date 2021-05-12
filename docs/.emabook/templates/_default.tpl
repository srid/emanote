<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>
    <note-title /> – emabook
  </title>
  <!-- TODO: Re-use Tailwind/windicss from emabook/ema? -->
  <link href="https://unpkg.com/tailwindcss@2.1.1/dist/tailwind.min.css" rel="stylesheet" type="text/css">
  <link href="/favicon.svg" rel="icon" />
</head>

<body>
  <div class="container mx-auto xl:max-w-screen-lg">
    <!-- Header row-->
    <div class="grid grid-cols-12 px-2">
      <!-- Logo column -->
      <div class="hidden md:mr-4 md:block md:col-span-3">
        <div class="flex items-end h-full pl-2 mt-2 space-x-2">
          <a href="/">
            <img class="z-50 h-20 transition transform hover:scale-125 hover:opacity-80" src="/favicon.svg" />
          </a>
        </div>
      </div>
      <!-- Title column -->
      <div class="col-span-12 md:col-span-9">
        <div class="flex items-center justify-center">
          <h1 class="pb-2 mt-2 mb-2 text-6xl text-center">
            <note-title />
          </h1>
        </div>
      </div>
    </div>

    <!-- Main row-->
    <div class="grid grid-cols-12 px-2">
      <!-- Sidebar column -->
      <div class="hidden overflow-x-auto md:mr-4 md:block md:col-span-3 md:sticky md:top-0 md:h-screen">
        <div class="pt-1 pb-2 rounded bg-${theme}-50">
          <route-tree>
            <sub-tree class="pl-2" />
            <item-parent class="my-2" />
            <item-terminal class="my-2 text-gray-600" />
            <link-active class="font-bold text-${theme}-600 hover:text-black" />
            <link-inactive class="hover:text-black" />
          </route-tree>
        </div>
      </div>
      <!-- Main body column -->
      <div class="col-span-12 md:col-span-9">
        <breadcrumbs>
          <div class="block w-full mt-4 text-gray-600 md:hidden">
            <div class="flex justify-center">
              <div class="w-full py-2 bg-white rounded">
                <ul class="flex text-sm text-gray-500 lg:text-base">
                  <crumb>
                    <li class="inline-flex items-center">
                      <a class="px-1 font-bold bg-${theme}-500 rounded text-gray-50" href="${crumb-url}">
                        <crumb-title />
                      </a>
                      <svg fill="currentColor" viewBox="0 0 20 20" class="w-auto h-5 text-gray-400">
                        <path fill-rule="evenodd"
                          d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                          clip-rule="evenodd"></path>
                      </svg>
                    </li>
                  </crumb>
                  <li class="inline-flex items-center text-gray-600">
                    <a>
                      <note-title />
                    </a>
                  </li>
                </ul>
              </div>
            </div>
          </div>
        </breadcrumbs>

        <note-pandoc>
          <PandocLink class="text-${theme}-600">
            <Internal class="font-bold hover:bg-${theme}-50" />
            <External class="hover:underline" target="_blank" rel="noopener" />
          </PandocLink>
          <Para class="my-2" />
          <CodeBlock class="py-0.5 text-sm" />
          <OrderedList class="ml-4 space-y-1 list-decimal list-inside" />
          <BulletList class="ml-4 space-y-1 list-decimal list-inside" />
          <Header>
            <h1 class="pb-2 mt-2 mb-2 text-6xl text-center" />
            <h2 class="mt-4 mb-2 text-5xl text-gray-700" />
            <h3 class="mt-4 mb-2 text-4xl text-gray-700" />
            <h4 class="mt-4 mb-2 text-3xl text-gray-700" />
            <h5 class="mt-4 mb-2 text-2xl text-gray-700" />
            <h6 class="mt-4 mb-2 text-xl text-gray-700" />
          </Header>
        </note-pandoc>

        <footer class="flex items-center justify-center my-8 space-x-4 text-center text-gray-500">
          <div>
            Powered by <a class="font-bold" href="https://github.com/srid/emabook">Emabook</a>
          </div>
        </footer>
      </div>
    </div>
  </div>
</body>

</html>