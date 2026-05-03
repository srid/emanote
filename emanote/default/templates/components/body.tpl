<!-- min-w-0 lets the flex item shrink below its min-content (default
     min-width:auto on flex items would otherwise let a long URL in a
     code block push the column past the container, dragging the
     right-panel outside the card chrome). -->
<div class="flex-1 min-w-0 bg-white dark:bg-gray-800">
  <main class="px-6 py-8 max-w-7xl mx-auto">
    <apply template="note-body" />
    <apply template="timeline" />
    <apply template="metadata" />
    <apply template="/templates/hooks/note-end" />
  </main>
</div>