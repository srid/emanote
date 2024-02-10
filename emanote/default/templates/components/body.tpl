<div class="flex-1 w-full bg-white">
  <main class="px-4 py-4">
    <apply template="note-title" />
    <ema:has:toc>
      <div class="flex container">
        <div class="w-3/4">
          <apply template="note-body" />
        </div>
        <div class="w-1/4 h-screen overflow-x-auto sticky top-0 p-4">
          <apply template="toc" />
        </div>
      </div>
    </ema:has:toc>
    <div class="flex flex-col lg:flex-row lg:space-x-2">
      <apply template="timeline" />
      <apply template="backlinks" />
    </div>
    <apply template="metadata" />
    <apply template="/templates/hooks/note-end" />
  </main>
</div>
