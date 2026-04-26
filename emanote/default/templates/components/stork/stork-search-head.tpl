<!-- Custom Stork-search styling for Emanote. Both stylesheets are loaded
     unconditionally; the active one is picked by toggling the wrapper's
     CSS class, driven by the same .dark class the theme toggle manages. -->
<link rel="stylesheet" href="${ema:emanoteStaticLayerUrl}/stork/edible.css" />
<link rel="stylesheet" href="${ema:emanoteStaticLayerUrl}/stork/edible-dark.css" />

<style data-category="stork">
  #stork-search-container {
    z-index: 1000;
    background-color: rgb(15 23 42/.8);
  }

  .stork-overflow-hidden-important {
    overflow: hidden !important;
  }
</style>

<!-- Vendor WASM loader. Defines window.stork synchronously so the
     stork.js module (loaded via the importmap entry from base.tpl)
     can call stork.initialize / stork.register at evaluate time. -->
<script src="${ema:emanoteStaticLayerUrl}/stork/stork.js"></script>
