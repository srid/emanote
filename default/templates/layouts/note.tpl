<apply template="base">
  <bind tag="body-main">
    <div class="container mx-auto xl:max-w-screen-lg">
      <apply template="components/breadcrumbs" />
      <div class="flex items-center justify-center">
        <h1 class="pb-2 mt-2 mb-2 text-6xl text-center">
          <ema:note:title />
        </h1>
      </div>
      <apply template="components/pandoc" />
      <apply template="components/backlinks" />

      <note-meta>
        <div class="flex items-center justify-center mt-8 space-x-2 font-mono text-sm">
          <with var="tags">
            <a title="Tag" class="px-1 bg-gray-100 rounded">
              <value />
            </a>
          </with>
        </div>
      </note-meta>

      <apply template="components/footer" />
    </div>
  </bind>
</apply>