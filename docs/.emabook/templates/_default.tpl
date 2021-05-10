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
        <div class="px-2 grid grid-cols-12">
            <!-- Logo column -->
            <div class="hidden md:mr-4 md:block md:col-span-3">
                <div class="mt-2 h-full flex pl-2 space-x-2 items-end">
                    <a href="/">
                        <img class="z-50 transition transform hover:scale-125 hover:opacity-80 h-20"
                            src="/favicon.svg" />
                    </a>
                </div>
            </div>
            <!-- Title column -->
            <div class="col-span-12 md:col-span-9">
                <div class="flex justify-center items-center">
                    <h1 class="text-6xl mt-2 mb-2 text-center pb-2">
                        <note-title />
                    </h1>
                </div>
            </div>
        </div>

        <!-- Main row-->
        <div class="px-2 grid grid-cols-12">
            <!-- Sidebar column -->
            <div class="hidden md:mr-4 md:block md:col-span-3 md:sticky md:top-0 md:h-screen overflow-x-auto">
                <div class="bg-pink-50 rounded pt-1 pb-2">
                    <note-sidebarHtml />
                </div>
            </div>
            <!-- Main body column -->
            <div class="col-span-12 md:col-span-9">
                <note-breadcrumbsHtml />
                <note-html />

                <footer class="flex justify-center items-center space-x-4 my-8 text-center text-gray-500">
                    <div>
                        Powered by
                        <a class="font-bold" href="https://github.com/srid/ema">Ema</a>
                    </div>
                </footer>
            </div>
        </div>
    </div>
</body>

</html>