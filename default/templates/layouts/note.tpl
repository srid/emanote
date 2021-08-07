<!-- TODO: Main navigation links (Home, index, tags, ...) -->
<apply template="base">
  <bind tag="head-main">
    <link rel="stylesheet" href="_emanote-static/inverted-tree.css" />
  </bind>
  <bind tag="body-main">
    <div class="container mx-auto max-w-prose">
      <div class="mt-2 md:mt-4">
        <apply template="components/note-uptree" />
        <div class="md:shadow-2xl md:mb-8">
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
    </div>
  </bind>
</apply>