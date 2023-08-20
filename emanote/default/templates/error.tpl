<!-- DoNotFormat -->
<bind tag="containerClass"><ema:metadata><value var="template.error.containerClass" /></ema:metadata></bind>
<!-- DoNotFormat -->

<apply template="base">
  <bind tag="body-main">
    <div class="${containerClass}">
      <div class="mt-2 md:mt-4">
        <div class="md:shadow-2xl md:mb-8">
          <div class="flex-1 w-full overflow-x-auto bg-white">
            <main class="px-4 py-4 bg-red-100">
              <h1
                class="flex items-end justify-center mb-4 p-3 bg-red-500 text-5xl font-extrabold text-gray-100 rounded">
                <a class="z-40 tracking-tighter ">
                  <ema:note:title />
                </a>
              </h1>
              <div class="flex items-center justify-center font-bold text-lg">
                Your notebook has an issue.
              </div>
              <div class="bg-gray-50 p-2 my-4">
                <apply template="/templates/components/pandoc" />
              </div>
              <div class="flex items-center justify-center text-xl">
                <div>Fix it (this page will reload), or <a class="font-bold underline" href="/">go
                    to /</a>.
                </div>
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