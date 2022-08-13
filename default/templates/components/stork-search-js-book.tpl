<apply template="components/stork-search-js" />
<!-- Stork-search styling, specific to Emanote's book layout -->
<style>
	.stork-wrapper > input {
		border-top-color: rgba(209,213,219,var(--tw-border-opacity));
		border-left-color: transparent;
		border-right-color: transparent;
	}

	@media (min-width: 768px) {
		.stork-wrapper > input {
			border-top-color: transparent;
			border-right-color: rgba(209,213,219,var(--tw-border-opacity));
		}

		.stork-wrapper .stork-output {
			top: 0;
			left: calc(100% - 1px);
			position: absolute;
			width: calc(768px - 12rem);
		}

		.stork-wrapper {
			margin-right: -1px;
		}
	}

	@media (min-width: 1024px) {
		.stork-wrapper .stork-output {
			width: calc(1024px - 12rem);
		}
	}

	@media (min-width: 1280px) {
		.stork-wrapper .stork-output {
			width: calc(1280px - 16rem);
		}
	}

	@media (min-width: 1536px) {
		.stork-wrapper .stork-output {
			width: calc(1536px - 16rem);
		}
	}

	nav#sidebar {
		z-index: 100;
	}
</style>
