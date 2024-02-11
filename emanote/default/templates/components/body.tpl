<div class="flex-1 w-full bg-white">
  <main class="px-4 py-4">
    <apply template="note-title" />
    <ema:has:toc>
      <div class="grid gap-4 md:grid-cols-8">
        <div class="md:col-span-7">
          <apply template="note-body" />
        </div>
        <apply template="toc" />
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
