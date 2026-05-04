<ema:tagsList>
  <!-- Metadata tag chips at the page footer. Same primary palette as
       the in-prose `emanote:inline-tag` rewriteClass and the wikilink
       chips, kept in font-mono since the tag string IS an identifier
       (slashed paths read better in mono). Drop the older gray-100
       pill chrome that pre-dated the chip language. -->
  <section
    class="flex flex-wrap items-end justify-center mt-8 mb-4 gap-x-2 gap-y-1.5 font-mono text-sm">
    <ema:each-tag>
      <a title="Tag" class="bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 px-1.5 py-0.5 rounded-sm hover:bg-primary-100/80 dark:hover:bg-primary-900/70 transition-colors"
        href="${ema:tag:url}">
        <!-- DoNotFormat -->
        #<ema:tag:name />
        <!-- DoNotFormat -->
      </a>
    </ema:each-tag>
  </section>
</ema:tagsList>
