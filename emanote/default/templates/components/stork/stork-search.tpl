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
  const wrapper = document.getElementById('stork-wrapper');
  const isDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
  wrapper.classList.add(isDark ? 'stork-wrapper-edible-dark' : 'stork-wrapper-edible');

  window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', e => {
    wrapper.classList.remove('stork-wrapper-edible', 'stork-wrapper-edible-dark');
    wrapper.classList.add(e.matches ? 'stork-wrapper-edible-dark' : 'stork-wrapper-edible');
  });
</script>