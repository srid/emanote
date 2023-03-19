<section title="Embedded note" class="p-4 mx-2 mb-2 bg-white border-2 rounded-lg shadow-inner">
  <header
    class="flex items-center justify-center text-2xl italic bg-${theme}-50 rounded py-1 px-2 mb-3">
    <a href="${ema:note:url}">
      <ema:note:title />
    </a>
  </header>
  <div>
    <apply template="/templates/components/pandoc" />
  </div>
</section>