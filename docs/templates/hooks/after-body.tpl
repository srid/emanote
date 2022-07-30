<!-- Stork search scripts, styling are at end of <head> -->
<script src="https://files.stork-search.net/releases/v1.5.0/stork.js"></script>
<script>
	if (document.readyState !== "complete") {
		stork.register(
			'emanote-search', // has to match input[data-stork] attribute value
			(document.querySelector('head > base').getAttribute('href') || '')+'stork.st'
		);
	} else {
		stork.downloadIndex(
			'emanote-search', // has to match input[data-stork] attribute value
			(document.querySelector('head > base').getAttribute('href') || '')+'stork.st',
			{ forceOverwrite: true } // needed for Ema's hot-reload
		);
	}
</script>
