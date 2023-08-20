<link rel="stylesheet" href="${ema:emanoteStaticLayerUrl}/stork/flat.css" />
<!-- Custom Stork-search styling for Emanote -->
<style>
  #stork-search-container {
    z-index: 1000;
    background-color: rgb(15 23 42/.8);
  }

  .stork-overflow-hidden-important {
    overflow: hidden !important;
  }
</style>


<script src="${ema:emanoteStaticLayerUrl}/stork/stork.js"></script>
<ema:metadata>
  <with var="template">
    <script id="emanote-stork" data-emanote-base-url="${value:baseUrl}">
      window.emanote = {};
      window.emanote.stork = {
        searchShown: false,
        indexIsStale: false,
        toggleSearch: function () {
          window.emanote.stork.refreshIndex();
          document.getElementById('stork-search-container').classList.toggle('hidden');
          window.emanote.stork.searchShown = document.body.classList.toggle('stork-overflow-hidden-important');
          if (window.emanote.stork.searchShown) {
            document.getElementById('stork-search-input').focus();
          }
        },
        clearSearch: function () {
          document.getElementById('stork-search-container').classList.add('hidden');
          document.body.classList.remove('stork-overflow-hidden-important');
          window.emanote.stork.searchShown = false;
        },

        getBaseUrl: function () {
          const baseUrl = document.getElementById("emanote-stork").getAttribute('data-emanote-base-url') || '/';
          return baseUrl;
        },

        registerIndex: function (options) {
          const indexName = 'emanote-search'; // used to match input[data-stork] attribute value
          const indexUrl = window.emanote.stork.getBaseUrl() + '-/stork.st';
          stork.register(
            indexName,
            indexUrl,
            options);
        },

        init: function () {
          if (document.readyState !== 'complete') {
            window.addEventListener('load', function () {
              stork.initialize(window.emanote.stork.getBaseUrl() + '_emanote-static/stork/stork.wasm');
              window.emanote.stork.registerIndex();
            });

            document.addEventListener('keydown', event => {
              if (window.emanote.stork.searchShown && event.key === 'Escape') {
                window.emanote.stork.clearSearch();
                event.preventDefault();
              } else if ((event.key == 'k' || event.key == 'K') && (event.ctrlKey || event.metaKey)) {
                window.emanote.stork.toggleSearch();
                event.preventDefault();
              }
            });
          } else {
            // This section is called during Ema's hot reload.
            // 
            // Mark the current index as stale, and refresh it *only when* the
            // user actually invokes search.
            // 
            // We do not refresh the index *right away*, as that will cause
            // memory leaks in the browser. See
            // https://github.com/srid/emanote/issues/411#issuecomment-1402056235
            console.log("stork: Marking index as stale");
            window.emanote.stork.markIndexAsStale();
          }
        },

        markIndexAsStale: function () {
          window.emanote.stork.indexIsStale = true;
        },

        refreshIndex: function () {
          if (window.emanote.stork.indexIsStale) {
            console.log("stork: Reloading index");
            window.emanote.stork.indexIsStale = false;
            // NOTE: This will leak memory. See the comment above.
            window.emanote.stork.registerIndex({ forceOverwrite: true });
          }
        }

      };

      window.emanote.stork.init();
    </script>
  </with>
</ema:metadata>