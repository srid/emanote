<!-- Backlinks attached to the bottom of the main panel at <lg. Container
     is flex-col below lg (md still has the sidebar but no right-panel —
     right-panel hides one breakpoint earlier than the sidebar so the
     prose keeps room as the viewport narrows). The surrounding
     #container's rounded/shadow chrome wraps this strip into the same
     panel as sidebar and prose, no card chrome of its own. Hidden at
     lg+, where right-panel.tpl carries the same backlinks. -->
<ema:note:backlinks:nodaily>
  <aside id="backlinks-bottom" class="lg:hidden bg-gray-50 dark:bg-gray-950 border-t border-gray-200 dark:border-gray-800 px-4 py-6">
    <h3 class="text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400 mb-3">
      Linked from
    </h3>
    <ul class="space-y-3 text-sm">
      <backlink>
        <li>
          <a class="inline-block bg-primary-50 dark:bg-primary-950 text-primary-700 dark:text-primary-300 font-semibold tracking-tight px-2 py-0.5 rounded-sm hover:bg-primary-100 dark:hover:bg-primary-900 transition-colors"
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
  </aside>
</ema:note:backlinks:nodaily>
