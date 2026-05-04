<!-- Daily-note backlinks rendered as a year-stacked heatmap.
     The hidden <ul id="timeline-data"> carries one <li> per daily
     backlink with data-url, data-title, and the optional rendered
     context. JS in _emanote-static/js/timeline-heatmap.js parses the
     dates out of the titles (YYYY-MM-DD), groups by year/month, and
     paints the cell grid into #timeline-heatmap. The hidden list is
     also the screen-reader / no-JS fallback. -->
<ema:note:backlinks:daily>
  <section id="timeline" class="text-sm text-gray-700 dark:text-gray-300">
    <h3 class="text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400 mb-3">
      Timeline
    </h3>
    <ul id="timeline-data" hidden>
      <backlink>
        <li data-url="${backlink:note:url}" data-title="${backlink:note:title}">
          <backlink:note:contexts>
            <apply template="context" />
          </backlink:note:contexts>
        </li>
      </backlink>
    </ul>
    <div id="timeline-heatmap" data-emanote-timeline></div>
  </section>
</ema:note:backlinks:daily>
