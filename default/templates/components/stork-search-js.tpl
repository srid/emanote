<script src="https://files.stork-search.net/releases/v1.5.0/stork.js"></script>
<ema:metadata>
	<with var="template">
		<script data-emanote-base-url="${value:baseUrl}">
			function toggleSearch() {
				document.getElementById('stork-search-container').classList.toggle('hidden');
				const shown = document.body.classList.toggle('stork-overflow-hidden-important');
				if (shown) {
					document.getElementById('stork-search-input').focus();
				}
			}

			function clearSearch() {
				document.getElementById('stork-search-container').classList.add('hidden');
				document.body.classList.remove('stork-overflow-hidden-important');
			}

			(function() {
				const indexName = 'emanote-search'; // used to match input[data-stork] attribute value
				const baseUrl = document.currentScript.getAttribute('data-emanote-base-url') || '/';
				const indexUrl = baseUrl + '-/stork.st';
				if (document.readyState !== 'complete') {
					window.addEventListener('load', function() {
						stork.register(indexName, indexUrl);
					});

					document.addEventListener('keydown', event => {
						if ((event.key == 'k' || event.key == 'K') && event.ctrlKey) {
							toggleSearch();
							event.preventDefault();
						}
					});
				} else {
					// Override existing on Ema's hot-reload
					stork.register(indexName, indexUrl, {forceOverwrite: true});
				}
			})();
		</script>
	</with>
</ema:metadata>
