<!-- Stork search styling, scripts added in after-body.tpl -->
<link rel="stylesheet" href="https://files.stork-search.net/releases/v1.5.0/basic.css" />
<style>
/* Applies changes to Stork search (https://stork-search.net/)
   to make it fit in with Emanote's styling.
*/

.stork-wrapper .stork-output {
  margin-top: 0;
  border-radius: 0;
  position: sticky;
  border-color: rgba(99,102,241,var(--tw-border-opacity));
  --tw-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1),0 4px 6px -2px rgba(0, 0, 0, 0.05);
  box-shadow: var(--tw-ring-offset-shadow,0 0 #0000),var(--tw-ring-shadow,0 0 #0000),var(--tw-shadow);
}

@media (min-width: 768px) {
  .stork-wrapper .stork-output {
    top: 0;
    left: 100%;
    position: absolute;
    width: calc(768px - 12rem);
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

nav#sidebar {
  z-index: 100;
}
</style>
