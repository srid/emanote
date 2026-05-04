<!-- Daily-note backlinks rendered as a year-stacked heatmap.
     Two visible homes:
       - Right-panel (lg+): timeline as part of the side-material column.
       - Bottom strip (<lg): timeline attached to the bottom of the
         card, alongside the regular backlinks fallback.
     Classes (not IDs) so both instances are valid HTML and the JS in
     _emanote-static/js/timeline-heatmap.js can paint each independently
     via querySelectorAll. The hidden <ul.timeline-data> is the
     screen-reader / no-JS fallback. -->
<ema:note:backlinks:daily>
  <section class="emanote-timeline text-sm text-gray-700 dark:text-gray-300">
    <h3 class="text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400 mb-3">
      Timeline
    </h3>
    <ul class="timeline-data" hidden>
      <backlink>
        <li data-url="${backlink:note:url}" data-title="${backlink:note:title}">
          <backlink:note:contexts>
            <apply template="context" />
          </backlink:note:contexts>
        </li>
      </backlink>
    </ul>
    <div class="timeline-heatmap"></div>
  </section>
</ema:note:backlinks:daily>
