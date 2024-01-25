<ema:note:toc>
  <Toc:List>
    <div class="md:w-1/3 lg:w-1/4 h-screen relative sticky top-[400px]">
      <toc:entries />
    </div>
    <script>
      // Highlight the TOC, based from https://stackoverflow.com/a/75346369
      function highlightTOC() {
        // Grab the toc links
        const links = document.querySelectorAll("ul > li > a.--ema-toc");

        // Find the section anchors
        const sections = [];
        links.forEach((link) => {
          for (section of document.querySelectorAll("a.--ema-anchor")) {
            if (link.href == section.href) {
              sections.push(section);
              break;
            }
          }
        });

        const mark = "bg-gray-200";
        window.onscroll = () => {
          // Remove previous mark
          links.forEach((link) => {
            link.classList.remove(mark);
          });

          // Mark the link of the section that is in view, starting from the end
          for (var i = sections.length - 1; i >= 0; i--) {
            if (window.scrollY > sections[i].offsetTop - 80) {
              links[i].classList.add(mark);
              break;
            }
          }
        };
      }
      highlightTOC();
    </script>
  </Toc:List>
  <Toc:Node>
    <ul class="ml-2">
      <toc:entry>
        <li class="whitespace-nowrap">
          <a href="${ema:note:url}#${toc:anchor}" class="--ema-toc"
            ><toc:title
          /></a>
          <toc:childs />
        </li>
      </toc:entry>
    </ul>
  </Toc:Node>
</ema:note:toc>
