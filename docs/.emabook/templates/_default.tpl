<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>
    <ema:note:title /> –
    <value var="settings.siteTitle" />
  </title>
  <!-- TODO: Re-use Tailwind/windicss from emabook/ema? -->
  <link href="https://unpkg.com/tailwindcss@2.1.2/dist/tailwind.min.css" rel="stylesheet" type="text/css">
  <link href="/favicon.svg" rel="icon" />
  <!-- Syntax highlighting -->
  <link href="https://cdn.jsdelivr.net/npm/prismjs@1.23.0/themes/prism-tomorrow.css" rel="stylesheet" />
  <script
    src="https://cdn.jsdelivr.net/combine/npm/prismjs@1.23.0/prism.min.js,npm/prismjs@1.23.0/plugins/autoloader/prism-autoloader.min.js"></script>
</head>

<body>
  <!-- 
    Just defining a convenient alias, to avoid <with>'ing
    NOTE: There must not be any whitespace inside! 
  -->
  <bind tag="theme"><value var="settings.theme" /></bind>

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
            <ema:note:title />
          </h1>
        </div>
      </div>
    </div>

    <!-- Main row-->
    <div class="grid grid-cols-12 px-2">
      <!-- Sidebar column -->
      <div class="hidden overflow-x-auto md:mr-4 md:block md:col-span-3 md:sticky md:top-0 md:h-screen">
        <div class="pt-1 pb-2 rounded bg-${theme}-50">
          <apply template="components/sidebar-tree" />
        </div>
      </div>
      <!-- Main body column -->
      <div class="col-span-12 md:col-span-9">
        <apply template="components/markdown" />
        <apply template="components/backlinks" />

        <ema:note:tags>
          <div class="flex items-center justify-center mt-8 space-x-2 font-mono text-sm">
            <tag>
              <a title="Tag" class="px-1 bg-gray-100 rounded">
                <tag:name />
              </a>
            </tag>
          </div>
        </ema:note:tags>

        <apply template="components/breadcrumbs" />

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