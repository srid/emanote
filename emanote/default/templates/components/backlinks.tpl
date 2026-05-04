<!-- Card-style "Linked from" used by the no-sidebar layout (e.g.
     /neuron-layout) and error.tpl, where there's no right-panel or
     bottom-strip. Same row shape as backlinks-bottom.tpl: chip on
     top, context indented under it with a left rule, divide-y rule
     between adjacent entries. The outer chrome is light because
     the parent container already supplies the rounded card. -->
<ema:note:backlinks:nodaily>
  <section id="backlinks" class="mt-8 border-t border-gray-200 dark:border-gray-800 pt-4">
    <header class="mb-3 text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400">
      Linked from
    </header>
    <ul class="divide-y divide-gray-200 dark:divide-gray-800 text-sm">
      <backlink>
        <li class="group py-3 first:pt-0 last:pb-0">
          <a class="inline-block bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 font-semibold tracking-tight px-2 py-0.5 rounded-sm hover:bg-primary-100/80 dark:hover:bg-primary-900/70 transition-colors after:content-none group-has-[[data-wikilink-type=WikiLinkEmbed]]:after:content-['embeds'] after:ml-2 after:px-[0.45em] after:py-[0.05em] after:text-[0.65em] after:font-medium after:tracking-wider after:uppercase after:text-primary-700 after:bg-primary-100 dark:after:text-primary-200 dark:after:bg-primary-800 after:rounded after:align-[0.15em]"
            href="${backlink:note:url}">
            <backlink:note:title />
          </a>
          <div class="mt-2 ml-2 pl-3 border-l-2 border-gray-200 dark:border-gray-800 text-gray-600 dark:text-gray-400">
            <backlink:note:contexts>
              <apply template="context" />
            </backlink:note:contexts>
          </div>
        </li>
      </backlink>
    </ul>
  </section>
</ema:note:backlinks:nodaily>
