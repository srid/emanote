<div id="stork-search-container"
  class="hidden fixed w-screen h-screen inset-0 backdrop-filter backdrop-blur-sm">
  <div class="fixed w-screen h-screen inset-0" onclick="toggleSearch()"></div>

  <div class="container mx-auto p-10 mt-10">
    <div class="stork-wrapper-flat container mx-auto">
      <input id="stork-search-input" data-stork="emanote-search" class="stork-input"
        placeholder="Search (Ctrl+K) ..." />

      <button class="stork-close-button" onclick="clearSearch()">
        <svg height="0.8em" viewBox="0 0 23 24" xmlns="http://www.w3.org/2000/svg">
          <g fill="none" fill-rule="evenodd" stroke-linecap="round">
            <g transform="translate(-700 -149)" stroke="currentcolor" stroke-width="4">
              <line id="a" x1="702.5" x2="720" y1="152.5" y2="170"></line>
              <line transform="translate(711 161) rotate(-90) translate(-711 -161)" x1="702.5"
                x2="720" y1="152.5" y2="170"></line>
            </g>
          </g>
        </svg>
      </button>

      <div data-stork="emanote-search-output" class="stork-output"></div>
    </div>
  </div>
</div>