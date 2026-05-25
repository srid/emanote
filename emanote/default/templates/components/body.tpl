<!-- min-w-0 lets the flex item shrink below its min-content (default
     min-width:auto on flex items would otherwise let a long URL in a
     code block push the column past the container, dragging the
     right-panel outside the card chrome). -->
<!-- DoNotFormat -->
<bind tag="noteFocusToggle">
  <div data-emanote-note-focus-toolbar class="hidden md:block absolute top-4 right-4 z-10">
    <button type="button"
      data-emanote-note-focus-toggle
      class="p-2 rounded-md text-gray-500 dark:text-gray-400 hover:bg-gray-100 dark:hover:bg-gray-700 hover:text-primary-600 dark:hover:text-primary-400 transition-colors cursor-pointer"
      title="${ema:i18n:maximizeNoteArea}"
      aria-label="${ema:i18n:maximizeNoteArea}"
      aria-pressed="false">
      <span data-emanote-note-focus-icon="maximize" aria-hidden="true">
        <svg class="w-[1.125rem] h-[1.125rem]" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 3H5a2 2 0 00-2 2v3m0 8v3a2 2 0 002 2h3m8-18h3a2 2 0 012 2v3m0 8v3a2 2 0 01-2 2h-3" />
        </svg>
      </span>
      <span data-emanote-note-focus-icon="restore" aria-hidden="true" hidden>
        <svg class="w-[1.125rem] h-[1.125rem]" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 9H5m0 0V5m0 4l5-5m5 5h4m0 0V5m0 4l-5-5M9 15H5m0 0v4m0-4l5 5m5-5h4m0 0v4m0-4l-5 5" />
        </svg>
      </span>
    </button>
  </div>
</bind>
<!-- DoNotFormat -->

<div id="note-column" data-emanote-note-focus-main class="relative flex-1 min-w-0 bg-white dark:bg-gray-800">
  <main class="relative px-6 py-8 max-w-7xl mx-auto">
    <ema:has:sidebar>
      <noteFocusToggle />
    </ema:has:sidebar>
    <apply template="note-body" />
    <apply template="metadata" />
    <apply template="/templates/hooks/note-end" />
  </main>
</div>
