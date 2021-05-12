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
                    <route-tree>
                        <sub-tree class="pl-2" />
                        <item-parent class="my-2" />
                        <item-terminal class="my-2 text-gray-600" />
                        <link-active class="hover:text-black text-pink-600 font-bold" />
                        <link-inactive class="hover:text-black" />
                    </route-tree>
                </div>
            </div>
            <!-- Main body column -->
            <div class="col-span-12 md:col-span-9">
                <breadcrumbs>
                    <div class="w-full text-gray-600 mt-4 block md:hidden">
                        <div class="flex justify-center">
                            <div class="w-full bg-white py-2 rounded">
                                <ul class="flex text-gray-500 text-sm lg:text-base">
                                    <crumb>
                                        <li class="inline-flex items-center">
                                            <a class="px-1 font-bold bg-pink-500 text-gray-50 rounded"
                                                href="${crumb-url}">
                                                <crumb-title />
                                            </a>
                                            <svg fill="currentColor" viewBox="0 0 20 20"
                                                class="h-5 w-auto text-gray-400">
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
                    <PandocLink class="text-pink-600">
                        <Internal class="font-bold hover:bg-pink-50" />
                        <External class="hover:underline" target="_blank" rel="noopener" />
                    </PandocLink>
                    <Para class="my-2" />
                    <CodeBlock class="py-0.5 text-sm" />
                    <OrderedList class="list-inside ml-4 space-y-1 list-decimal" />
                    <BulletList class="list-inside ml-4 space-y-1 list-decimal" />
                    <Header>
                        <h1 class="text-6xl mt-2 mb-2 text-center pb-2" />
                        <h2 class="text-5xl mt-4 mb-2 text-gray-700" />
                        <h3 class="text-4xl mt-4 mb-2 text-gray-700" />
                        <h4 class="text-3xl mt-4 mb-2 text-gray-700" />
                        <h5 class="text-2xl mt-4 mb-2 text-gray-700" />
                        <h6 class="text-xl  mt-4 mb-2 text-gray-700" />
                    </Header>
                </note-pandoc>

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