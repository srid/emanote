<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>
    <ema:note:title /> –
    <value var="page.siteTitle" />
  </title>
  <with var="page">
    <meta property="og:description" content="${value:description}" />
    <meta property="og:site_name" content="${value:siteName}" />
  </with>
  <!-- TODO: Re-use Tailwind/windicss from emanote/ema? -->
  <link href="https://unpkg.com/tailwindcss@2.1.2/dist/tailwind.min.css" rel="stylesheet"
    type="text/css">
  <with var="template">
    <link href="${value:iconUrl}" rel="icon" />
  </with>
  <snippet var="page.headHtml" />
</head>

<body>
  <!-- 
    Just defining a convenient alias, to avoid <with>'ing
    NOTE: There must not be any whitespace inside this tag! 
  -->
  <bind tag="theme"><value var="template.theme" /></bind>

  <div class="container mx-auto xl:max-w-screen-lg">
    <!-- Header row-->
    <div class="grid grid-cols-12 px-2">
      <!-- Logo column -->
      <div class="hidden md:mr-4 md:block md:col-span-3">
        <div class="flex items-end h-full pl-2 mt-2 space-x-2">
          <a href="/">
            <with var="template">
              <img class="z-50 h-20 transition transform hover:scale-125 hover:opacity-80"
                src="${value:iconUrl}" />
            </with>
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
      <div
        class="hidden overflow-x-auto md:mr-4 md:block md:col-span-3 md:sticky md:top-0 md:h-screen">
        <div class="pt-1 pb-2 rounded bg-${theme}-50">
          <ema:route-tree>
            <apply template="components/sidebar-tree" />
          </ema:route-tree>
        </div>
      </div>
      <!-- Main body column -->
      <div class="col-span-12 md:col-span-9">
        <apply template="components/markdown" />
        <apply template="components/backlinks" />

        <apply template="components/breadcrumbs" />

        <note-meta>
          <div class="flex items-center justify-center mt-8 space-x-2 font-mono text-sm">
            <with var="tags">
              <a title="Tag" class="px-1 bg-gray-100 rounded">
                <value />
              </a>
            </with>
          </div>
        </note-meta>

        <footer class="flex items-center justify-center my-8 space-x-4 text-center text-gray-500">
          <div>
            Powered by <a class="font-bold" href="https://github.com/srid/emanote">Emanote</a>
          </div>
        </footer>
      </div>
    </div>
  </div>
</body>

</html>