<!-- Side material attached to the bottom of the main card at <lg.
     Hosts both the timeline heatmap (daily-note backlinks) and the
     "Linked from" regular-backlink list — same content as the
     right-panel at lg+, just rendered as a stacked footer strip when
     the right-panel is hidden.

     The aside renders unconditionally (Heist can't easily express an
     OR across two splices), and the [&:not(:has(*))]:hidden Tailwind
     guard hides it on pages where neither splice emitted anything —
     so the chrome doesn't show as an empty bar on backlink-less pages. -->
<aside id="backlinks-bottom" class="lg:hidden [&:not(:has(*))]:hidden bg-gray-50 dark:bg-gray-950 border-t border-gray-200 dark:border-gray-800 px-4 py-6 space-y-6">
  <ema:note:backlinks:daily>
    <apply template="timeline" />
  </ema:note:backlinks:daily>
  <ema:note:backlinks:nodaily>
    <section>
      <h3 class="text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400 mb-3">
        Linked from
      </h3>
      <ul class="space-y-3 text-sm">
        <backlink>
          <li>
            <a class="inline-block bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 font-semibold tracking-tight px-2 py-0.5 rounded-sm hover:bg-primary-100/80 dark:hover:bg-primary-900/70 transition-colors"
               href="${backlink:note:url}">
              <backlink:note:title />
            </a>
            <div class="text-gray-500 dark:text-gray-300 mt-1.5">
              <backlink:note:contexts>
                <apply template="context" />
              </backlink:note:contexts>
            </div>
          </li>
        </backlink>
      </ul>
    </section>
  </ema:note:backlinks:nodaily>
</aside>
