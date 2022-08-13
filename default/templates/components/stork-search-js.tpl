<link rel="stylesheet" href="https://files.stork-search.net/releases/v1.5.0/basic.css" />
<script src="https://files.stork-search.net/releases/v1.5.0/stork.js"></script>
<ema:metadata>
	<with var="template">
		<script data-emanote-base-url="${value:baseUrl}">
			(function() {
				const indexName = 'emanote-search'; // used to match input[data-stork] attribute value
				const baseUrl = document.currentScript.getAttribute('data-emanote-base-url') || '/';
				const indexUrl = baseUrl + '-/stork.st';
				if (document.readyState !== 'complete') {
					window.addEventListener('load', function() {
						stork.register(indexName, indexUrl);
					});
				} else {
					stork.register(indexName, indexUrl, {forceOverwrite: true});
				}
			})();
		</script>
	</with>
</ema:metadata>
