<div class="flex-1 w-full bg-white">
  <main class="px-4 py-4">
    <apply template="note-title" />
    <ema:has:toc>
      <div class="grid gap-4 md:grid-cols-8">
        <div class="md:col-span-6">
          <apply template="note-body" />
        </div>
        <div class="md:col-span-2 border-l-2 bg-gray-50">
          <apply template="toc" />
        </div>
      </div>
      <else />
      <apply template="note-body" />
    </ema:has:toc>
    <div class="flex flex-col lg:flex-row lg:space-x-2">
      <apply template="timeline" />
      <apply template="backlinks" />
    </div>
    <apply template="metadata" />
    <apply template="/templates/hooks/note-end" />
  </main>
</div>