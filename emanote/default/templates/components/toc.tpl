<nav id="toc"
  class="hidden leading-relaxed md:block md:sticky md:top-0 md:max-h-screen md:overflow-y-auto">
  <div class="text-gray-600 text-sm -mt-2">
    <ema:note:toc>
      <Toc>
        <ul class="ml-2">
          <toc:entry>
            <li class="whitespace-nowrap truncate mt-2" title="${toc:title}">
              <a href="${ema:note:url}#${toc:anchor}"
                class="--ema-toc bg-${theme}-200 rounded-md p-0.5">
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
    // Highlight the TOC, based from https://stackoverflow.com/a/75346369
    // TODO: We should get rid of JavaScript! See https://github.com/srid/emanote/issues/520
    function highlightTOC() {
      // Grab the toc links
      const links = document.querySelectorAll("ul > li > a.--ema-toc");

      // Find the matching anchors in the document body
      const sections = [];
      links.forEach((link) => {
        let found = {};
        for (section of document.querySelectorAll("a.--ema-anchor")) {
          if (link.href == section.href) {
            found = section;
            break;
          }
        }
        sections.push(found);
      });

      // Current toc link is marked with the following class
      const mark = "toc-item-active";

      // Set window scroll handler to update the toc mark.
      window.onscroll = () => {
        // Remove previous mark
        links.forEach((link) => {
          link.classList.remove(mark);
        });

        // Mark the link of the section that is in view, starting from the end
        let marked = false;
        for (var i = sections.length - 1; i >= 0; i--) {
          if (window.scrollY > sections[i].offsetTop - 80) {
            links[i].classList.add(mark);
            marked = true;
            break;
          }
        }

        // Special case for the first and last section which might not reach the top
        if (!marked && window.scrollY > 0) {
          let i = 0;
          if ((window.innerHeight + Math.round(window.scrollY)) >= document.body.offsetHeight) {
            // We are at the bottom
            i = links.length - 1
          }
          links[i].classList.add(mark);
        }
      };
    }
    highlightTOC();
  </script>
</nav>