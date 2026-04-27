<!-- DoNotFormat -->
<bind tag="containerClass"><ema:metadata><value var="template.error.containerClass" /></ema:metadata></bind>
<!-- DoNotFormat -->

<apply template="base">
  <bind tag="body-main">
    <div class="${containerClass}">
      <div class="mt-2 md:mt-4">
        <div class="md:shadow-2xl md:mb-8">
          <div class="flex-1 w-full overflow-x-auto bg-white dark:bg-gray-800">
            <main class="px-4 py-4 bg-red-100 dark:bg-red-900">
              <h1
                class="flex items-end justify-center mb-4 p-3 bg-red-500 dark:bg-red-600 text-5xl font-extrabold text-gray-100 dark:text-gray-200 rounded">
                <span class="z-40 tracking-tighter ">
                  <ema:note:title />
                </span>
              </h1>
              <div class="flex items-center justify-center font-bold text-lg text-gray-900 dark:text-gray-100">
                Your notebook has an issue.
              </div>
              <div class="bg-gray-50 dark:bg-gray-700 p-2 my-4">
                <apply template="/templates/components/pandoc" />
              </div>
              <div class="flex items-center justify-center text-xl text-gray-900 dark:text-gray-100">
                <ema:metadata>
                  <with var="template">
                    <div>Fix it (this page will reload), or <a class="font-bold underline text-red-700 dark:text-red-300 hover:text-red-800 dark:hover:text-red-200"
                        href="${value:baseUrl}">go
                        to /</a>.
                    </div>
                  </with>
                </ema:metadata>
              </div>
              <div class="transform scale-95 opacity-50 hover:opacity-100">
                <apply template="components/timeline" />
                <apply template="components/backlinks" />
              </div>
            </main>
          </div>
        </div>
      </div>
    </div>
  </bind>
</apply>