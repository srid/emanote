<!-- Slim backlinks variant rendered inside the right-panel at md+ (see
     components/right-panel.tpl). Drops the card chrome of backlinks.tpl:
     just a labelled list of titles. The full context paragraph(s) ride
     along in the DOM but stay hidden until hover/focus, when a flyout
     opens to the left over the prose column.

     Tailwind's `hover:` variant is automatically gated on (hover: hover),
     so on touch devices the flyout never triggers — touch users see the
     labelled titles only and follow the link to read more. -->
<ema:note:backlinks:nodaily>
  <section id="backlinks-margin" class="text-sm text-gray-700 dark:text-gray-300">
    <h3 class="text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400 mb-3">
      Linked from
    </h3>
    <ul class="space-y-1">
      <backlink>
        <li class="group relative">
          <a class="block py-1 rounded-md hover:text-primary-700 dark:hover:text-primary-300 transition-colors no-underline"
             href="${backlink:note:url}">
            <span class="text-gray-400 dark:text-gray-600 mr-1.5 select-none" aria-hidden="true">→</span><backlink:note:title />
          </a>
          <!-- Outer wrapper extends right up against the li (right:full)
               with pr-3 of padding so the inner card visually sits 0.75rem
               away. That padding is the "hover bridge": cursor crossing
               from li to the card stays inside the outer wrapper, so
               group-hover doesn't drop and the flyout doesn't vanish
               mid-traverse. -->
          <div class="hidden group-hover:block group-focus-within:block absolute right-full top-0 pr-3 z-30">
            <div class="w-[22rem] max-w-[min(22rem,40vw)] px-4 py-3.5 border border-gray-200 dark:border-gray-700 rounded-lg bg-white dark:bg-gray-900 shadow-xl text-[0.85rem] leading-[1.55]">
              <backlink:note:contexts>
                <apply template="context" />
              </backlink:note:contexts>
            </div>
          </div>
        </li>
      </backlink>
    </ul>
  </section>
</ema:note:backlinks:nodaily>
