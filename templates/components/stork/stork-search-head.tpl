<!-- Custom Stork-search styling for Emanote. Both stylesheets are loaded
     unconditionally; the active one is picked by toggling the wrapper's
     CSS class, driven by the same .dark class the theme toggle manages. -->
<emanoteStaticUrl path="stork/edible.css">
  <link rel="stylesheet" href="${url}" />
</emanoteStaticUrl>
<emanoteStaticUrl path="stork/edible-dark.css">
  <link rel="stylesheet" href="${url}" />
</emanoteStaticUrl>

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
<emanoteStaticUrl path="stork/stork.js">
  <script src="${url}"></script>
</emanoteStaticUrl>
