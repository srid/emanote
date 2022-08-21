<link rel="stylesheet" href="https://files.stork-search.net/releases/v1.5.0/flat.css" />
<!-- Custom Stork-search styling for Emanote -->
<style>
  #stork-search-container {
    z-index: 1000;
    background-color: rgb(15 23 42/.8);
  }

  .stork-overflow-hidden-important {
    overflow: hidden !important;
  }

  .stork-wrapper-flat .stork-close-button svg {
    /* small bugfix in Stork's stock styling */
    margin: auto;
  }
</style>


<script src="https://files.stork-search.net/releases/v1.5.0/stork.js"></script>
<ema:metadata>
  <with var="template">
    <script data-emanote-base-url="${value:baseUrl}">
      window.searchShown = false;
      function toggleSearch() {
        document.getElementById('stork-search-container').classList.toggle('hidden');
        window.searchShown = document.body.classList.toggle('stork-overflow-hidden-important');
        if (window.searchShown) {
          document.getElementById('stork-search-input').focus();
        }
      }

      function clearSearch() {
        document.getElementById('stork-search-container').classList.add('hidden');
        document.body.classList.remove('stork-overflow-hidden-important');
        window.searchShown = false;
      }

      (function () {
        const indexName = 'emanote-search'; // used to match input[data-stork] attribute value
        const baseUrl = document.currentScript.getAttribute('data-emanote-base-url') || '/';
        const indexUrl = baseUrl + '-/stork.st';
        if (document.readyState !== 'complete') {
          window.addEventListener('load', function () {
            stork.register(indexName, indexUrl);
          });

          document.addEventListener('keydown', event => {
            if (window.searchShown && event.key === 'Escape') {
              clearSearch();
              event.preventDefault();
            } else if ((event.key == 'k' || event.key == 'K') && (event.ctrlKey || event.metaKey)) {
              toggleSearch();
              event.preventDefault();
            }
          });
        } else {
          // Override existing on Ema's hot-reload
          stork.register(indexName, indexUrl, { forceOverwrite: true });
        }
      })();
    </script>
  </with>
</ema:metadata>