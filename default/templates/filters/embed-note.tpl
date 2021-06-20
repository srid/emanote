<section title="Embedded note"
  class="border-2 rounded-lg shadow-inner p-4 mb-4 transform scale-95 opacity-95">
  <header class="flex items-center justify-center text-2xl italic bg-${theme}-50 rounded py-1">
    <a href="${ema:note:url}">
      <ema:note:title />
    </a>
  </header>
  <div>
    <apply template="/templates/components/pandoc" />
  </div>
</section>