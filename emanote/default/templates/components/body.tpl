<div class="flex-1 w-full overflow-x-auto bg-white">
  <main class="px-4 py-4">
    <apply template="note-title" />
    <apply template="note-body" />
    <div class="flex flex-col lg:flex-row lg:space-x-2">
      <apply template="timeline" />
      <apply template="backlinks" />
    </div>
    <apply template="metadata" />
    <apply template="/templates/hooks/note-end" />
  </main>
</div>