<!-- Stork search scripts, styling added in more-head.tpl -->
<script src="https://files.stork-search.net/releases/v1.5.0/stork.js"></script>
<script>
(function () {
	const indexName = 'emanote-search'; // used to match input[data-stork] attribute value
	const baseUrl = document.querySelector('head > base').getAttribute('href') || '/';
	const indexUrl = baseUrl + 'stork.st';

	if (document.readyState !== "complete") {
		stork.register(indexName, indexUrl);
	} else {
		// In case of Ema hot-reload: only update index
		stork.downloadIndex(indexName, indexUrl, {
			forceOverwrite: true
		});
	}
})();
</script>
