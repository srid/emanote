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
<!-- Stork-search styling base stylings -->
<style>
	.stork-wrapper .stork-output {
		margin-top: 0;
		border-radius: 0;
		position: sticky;
		border-color: rgba(99,102,241,var(--tw-border-opacity));
		--tw-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1),0 4px 6px -2px rgba(0, 0, 0, 0.05);
		box-shadow: var(--tw-ring-offset-shadow,0 0 #0000),var(--tw-ring-shadow,0 0 #0000),var(--tw-shadow);
	}

	.stork-wrapper .stork-message {
		padding: 0.5rem 1rem;
		border-radius: 0;
		border-color: rgba(99,102,241,var(--tw-border-opacity));
	}

	.stork-wrapper .stork-close-button {
		top: 0;
		margin: 0.8em 0.6em;
	}

	.stork-wrapper .stork-close-button svg {
		top: unset;
		margin-left: auto;
		margin-right: auto;
	}
</style>
