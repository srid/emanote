<div id="stork-search-container"
  class="hidden fixed w-screen h-screen inset-0 backdrop-filter backdrop-blur-sm">
  <div class="fixed w-screen h-screen inset-0" onclick="window.emanote.stork.toggleSearch()"></div>

  <div class="container mx-auto p-10 mt-10">
    <div id="stork-wrapper" class="container mx-auto">
      <input id="stork-search-input" data-stork="emanote-search" class="stork-input"
        placeholder="Search (Ctrl+K) ..." />
      <div data-stork="emanote-search-output" class="stork-output"></div>
    </div>
  </div>
</div>

<script>
  // Block-scoped so Ema's reloadScripts can re-execute on route switches
  // without hitting "Identifier 'wrapper' has already been declared".
  {
    const wrapper = document.getElementById('stork-wrapper');
    const applyTheme = () => {
      const isDark = document.documentElement.classList.contains('dark');
      wrapper.classList.remove('stork-wrapper-edible', 'stork-wrapper-edible-dark');
      wrapper.classList.add(isDark ? 'stork-wrapper-edible-dark' : 'stork-wrapper-edible');
    };
    applyTheme();
    // Theme toggle mutates the .dark class on <html>; mirror it onto the
    // stork wrapper so the search dialog re-skins live without a reload.
    new MutationObserver(applyTheme).observe(document.documentElement, {
      attributes: true, attributeFilter: ['class']
    });
  }
</script>