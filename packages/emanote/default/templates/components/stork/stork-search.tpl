<div id="stork-search-container"
  class="hidden fixed w-screen h-screen inset-0 backdrop-filter backdrop-blur-sm">
  <!-- Backdrop close: data-emanote-stork-toggle is wired in stork.js
       via event delegation. Toggling closes here because the backdrop
       is only visible while the modal is open. -->
  <div class="fixed w-screen h-screen inset-0" data-emanote-stork-toggle></div>

  <div class="container mx-auto p-10 mt-10">
    <div id="stork-wrapper" class="container mx-auto">
      <input id="stork-search-input" data-stork="emanote-search" class="stork-input"
        placeholder="Search (Ctrl+K) ..." />
      <div data-stork="emanote-search-output" class="stork-output"></div>
    </div>
  </div>
</div>

<!-- Theme mirror (.dark on <html> → .stork-wrapper-edible-dark on
     #stork-wrapper) moved into _emanote-static/js/stork.js. -->
