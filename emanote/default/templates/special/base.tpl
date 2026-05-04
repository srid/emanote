<!-- Special-page layout (tag index, task index, all-notes index).
     These are list pages — no sidebar / right-panel / TOC / backlinks /
     timeline — but they share the note-page card chrome so the site
     reads as one design: title strip on top, content in the middle,
     footer attached at the bottom, all inside the rounded `#container`.

     Mirrors the no-sidebar branch of layouts/default.tpl. The
     containerClass binding pulls the same "container mx-auto + 2xl
     max-w bump" from index.yaml so ultra-wide viewports don't waste
     gutter space. -->
<bind tag="containerClass"><ema:metadata><value var="template.base.containerClass" /></ema:metadata> mt-2 md:mt-8</bind>
<apply template="/templates/base">
  <bind tag="body-main">
    <div class="${containerClass}">
      <div id="container" class="relative flex flex-col md:shadow-md md:rounded-lg md:mb-8 bg-white dark:bg-gray-900 md:border md:border-gray-200 dark:md:border-gray-800">
        <!-- Back-to-Home affordance left-anchored in the title strip — special
             pages have no sidebar so this is the only nav back to /. Logo +
             "Home" matches the sidebar header pattern. -->
        <header class="relative px-8 py-5 md:rounded-t-lg bg-primary-50 dark:bg-primary-950/40 text-primary-700 dark:text-primary-300 text-center border-b border-primary-200/60 dark:border-primary-800/40">
          <ema:metadata>
            <with var="template">
              <a href="${value:baseUrl}" title="Go to Home" class="hidden md:inline-flex absolute left-4 top-1/2 -translate-y-1/2 items-center gap-1.5 text-sm font-medium hover:opacity-70 transition-opacity">
                <img class="w-4 h-4" src="${value:iconUrl}" alt="" />
                <span>Home</span>
              </a>
            </with>
          </ema:metadata>
          <h1 class="text-3xl md:text-4xl font-semibold tracking-tight">
            <ema:title />
          </h1>
        </header>
        <main class="w-full px-6 py-8 max-w-7xl mx-auto">
          <special-main />
        </main>
        <apply template="components/footer" />
      </div>
    </div>
  </bind>
</apply>
