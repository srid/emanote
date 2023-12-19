<!-- DoNotFormat -->
<bind tag="containerClass"><ema:metadata><value var="template.layout.note.containerClass" /></ema:metadata></bind>
<!-- DoNotFormat -->

<bind tag="storkSearchButtonTopRight">
  <div class="absolute -top-6 right-1 md:right-0 flex flex-row items-center justify-center">
    <a title="Search (Ctrl+K)" class="cursor-pointer" onclick="window.emanote.stork.toggleSearch()">
      <apply template="components/stork/stork-icon" />
    </a>
  </div>
</bind>

<apply template="base">
  <bind tag="head-main">
    <link rel="stylesheet" href="${ema:emanoteStaticLayerUrl}/inverted-tree.css" />
  </bind>
  <bind tag="body-main">
    <div class="mt-2 md:mt-4">
      <apply template="components/note-uptree" />

      <div class="flex flex-row items-start justify-center">
        <nav class="w-64 flex flex-col items-end space-y-2">
          <ema:note:parents>
            <parent>
              <div class="flex flex-col items-end">
                <a class="font-bold underline mavenLinkBold hover:bg-${theme}-50"
                  href="${parent:url}">
                  <parent:title />
                </a>
                <parent:children>
                  <div class="flex flex-col items-end">
                    <child>
                      <a class="hover:bg-${theme}-50" href="${child:url}">
                        <child:title />
                      </a>
                    </child>
                  </div>
                </parent:children>
              </div>
            </parent>
          </ema:note:parents>
        </nav>
        <div id="container" class="${containerClass} relative md:shadow-2xl md:mb-8">
          <storkSearchButtonTopRight />
          <apply template="components/body" />
        </div>
        <nav class="w-64 flex flex-col items-start space-y-2">
          <ema:note:children>
            <child>
              <div class="flex flex-col items-end">
                <a class="font-bold mavenLinkBold hover:bg-${theme}-50" href="${child:url}">
                  <child:title />
                </a>
              </div>
            </child>
          </ema:note:children>
        </nav>
      </div>

      <apply template="components/footer" />
    </div>
  </bind>
</apply>