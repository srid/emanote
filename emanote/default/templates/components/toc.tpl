<nav id="toc"
  class="hidden leading-relaxed md:block md:sticky md:top-0 md:max-h-screen md:overflow-y-auto">
  <div class="text-gray-700 dark:text-gray-300 text-sm pt-2">
    <h3 class="text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400 mb-3 px-2">On this page</h3>
    <ema:note:toc>
      <Toc>
        <ul class="space-y-1 ml-2">
          <toc:entry>
            <li class="truncate" title="${toc:title}">
              <a href="${ema:note:url}#${toc:anchor}"
                class="--ema-toc block rounded-md px-2 py-1.5 hover:bg-primary-50 dark:hover:bg-primary-950 hover:text-primary-700 dark:hover:text-primary-300 transition-colors">
                <toc:title />
              </a>
              <toc:childs />
            </li>
          </toc:entry>
        </ul>
      </Toc>
    </ema:note:toc>
  </div>
  <!-- Scroll-spy behavior moved to _emanote-static/js/toc-spy.js — see issue #643. -->
</nav>
