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
  <script>
    // Highlight the TOC link for the section currently in view.
    // Uses Intersection Observer (passive, no scroll-loop layout thrashing);
    // replaces the old window.onscroll approach from issue #520.
    (function () {
      const tocLinks = document.querySelectorAll("a.--ema-toc");
      if (tocLinks.length === 0) return;

      const anchors = {};
      document.querySelectorAll("a.--ema-anchor").forEach((a) => {
        anchors[a.href] = a;
      });

      const observed = [];
      const visibility = new Map();
      tocLinks.forEach((link) => {
        const anchor = anchors[link.href];
        if (!anchor) return;
        const section = anchor.closest("h1, h2, h3, h4, h5, h6") || anchor;
        observed.push({ section, link });
        visibility.set(section, false);
      });

      const mark = "toc-item-active";
      const setActive = (activeLink) => {
        tocLinks.forEach((l) => l.classList.remove(mark));
        if (activeLink) activeLink.classList.add(mark);
      };

      const pickActive = () => {
        let firstVisible = null;
        let lastAbove = null;
        for (const { section, link } of observed) {
          if (visibility.get(section)) {
            if (!firstVisible) firstVisible = link;
          } else if (section.getBoundingClientRect().top < 0) {
            lastAbove = link;
          }
        }
        setActive(firstVisible || lastAbove || observed[0]?.link);
      };

      const observer = new IntersectionObserver((entries) => {
        for (const entry of entries) {
          visibility.set(entry.target, entry.isIntersecting);
        }
        pickActive();
      }, { rootMargin: "-80px 0px -60% 0px", threshold: 0 });

      for (const { section } of observed) observer.observe(section);
      pickActive();
    })();
  </script>
</nav>
