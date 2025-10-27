<div class="flex-1 w-full bg-white dark:bg-gray-800">
  <main class="px-6 py-8 max-w-7xl mx-auto">
    <apply template="note-title" />
    <ema:has:toc>
      <div class="md:grid md:gap-8 md:grid-cols-8">
        <div class="md:col-span-6">
          <apply template="note-body" />
        </div>
        <div class="hidden md:block md:col-span-2 md:border-l md:border-gray-200 dark:md:border-gray-800 md:pl-6 md:bg-gray-50/50 dark:md:bg-gray-950/50 md:-mr-6 md:pr-6 md:-my-8 md:py-8">
          <apply template="toc" />
        </div>
      </div>
      <else />
      <apply template="note-body" />
    </ema:has:toc>
    <div class="flex flex-col lg:flex-row lg:space-x-4 mt-8">
      <apply template="timeline" />
      <apply template="backlinks" />
    </div>
    <apply template="metadata" />
    <apply template="/templates/hooks/note-end" />
  </main>
</div>