<!-- TODO: Main navigation links (Home, index, tags, ...) -->
<apply template="base">
  <bind tag="body-main">
    <div class="container mx-auto xl:max-w-prose">
      <div class="md:mt-8 md:shadow-2xl md:mb-8">
        <apply template="components/breadcrumbs" />
        <div class="flex-1 w-full overflow-x-auto bg-white">
          <main class="px-4 py-4">
            <apply template="components/note-title" />
            <apply template="components/note-body" />
            <apply template="components/backlinks" />
            <apply template="components/metadata" />
          </main>
        </div>
      </div>
      <apply template="components/footer" />
    </div>
  </bind>
</apply>