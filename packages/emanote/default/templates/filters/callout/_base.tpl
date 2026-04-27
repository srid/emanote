<bind tag="callout-icon-svg">
  <div class="callout-icon flex-shrink-0">
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor"
      stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="w-5 h-5">
      <icon />
    </svg>
  </div>
</bind>
<bind tag="callout-fold-marker">
  <div class="callout-fold-marker ml-auto flex-shrink-0" aria-hidden="true">
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor"
      stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="w-4 h-4 transition-transform">
      <polyline points="6 9 12 15 18 9"></polyline>
    </svg>
  </div>
</bind>
<bind tag="callout-body-block">
  <div class="callout-content text-gray-700 dark:text-gray-300">
    <callout:body />
  </div>
</bind>

<callout:if-not-foldable>
  <div data-callout-metadata="" data-callout-fold="" data-callout="${callout:type}" data-footnote-scope
    class="callout p-5 pt-4 pb-3 rounded-xl mb-6 border-l-4 shadow-sm" role="note"
    style="background-color: color-mix(in srgb, ${color} 8%, transparent); border-left-color: ${color}">
    <div class="callout-title flex items-center mb-3 font-semibold text-lg" style="color: ${color}">
      <callout-icon-svg />
      <div class="ml-3">
        <callout:title />
      </div>
    </div>
    <callout-body-block />
  </div>
</callout:if-not-foldable>

<callout:if-foldable-expanded>
  <details open data-callout-metadata="" data-callout-fold="${callout:fold-state}" data-callout="${callout:type}" data-footnote-scope
    class="callout callout-foldable p-5 pt-4 pb-3 rounded-xl mb-6 border-l-4 shadow-sm" role="note"
    style="background-color: color-mix(in srgb, ${color} 8%, transparent); border-left-color: ${color}">
    <summary class="callout-title flex items-center font-semibold text-lg cursor-pointer select-none list-none" style="color: ${color}">
      <callout-icon-svg />
      <div class="ml-3 flex-1">
        <callout:title />
      </div>
      <callout-fold-marker />
    </summary>
    <div class="mt-3">
      <callout-body-block />
    </div>
  </details>
</callout:if-foldable-expanded>

<callout:if-foldable-collapsed>
  <details data-callout-metadata="" data-callout-fold="${callout:fold-state}" data-callout="${callout:type}" data-footnote-scope
    class="callout callout-foldable p-5 pt-4 pb-3 rounded-xl mb-6 border-l-4 shadow-sm" role="note"
    style="background-color: color-mix(in srgb, ${color} 8%, transparent); border-left-color: ${color}">
    <summary class="callout-title flex items-center font-semibold text-lg cursor-pointer select-none list-none" style="color: ${color}">
      <callout-icon-svg />
      <div class="ml-3 flex-1">
        <callout:title />
      </div>
      <callout-fold-marker />
    </summary>
    <div class="mt-3">
      <callout-body-block />
    </div>
  </details>
</callout:if-foldable-collapsed>
