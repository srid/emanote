<apply template="/templates/special/base">
  <bind tag="special-main">
    <!-- Tag breadcrumbs as tag-chips with `/` separator. Mirrors the
         tasks.tpl breadcrumb pattern but with the tag-chip (font-mono,
         primary palette) since each crumb segment IS a tag. -->
    <nav id="tagcrumbs" class="mb-6 pb-3 border-b border-gray-100 dark:border-gray-800">
      <ol class="flex flex-wrap items-baseline gap-x-1.5 gap-y-1 font-mono text-sm">
        <ema:tagcrumbs>
          <ema:each-crumb>
            <li class="inline-flex items-baseline gap-x-1.5 [&:not(:last-child)]:after:content-['/'] [&:not(:last-child)]:after:text-gray-300 dark:[&:not(:last-child)]:after:text-gray-600">
              <a class="bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 px-1.5 py-0.5 rounded-sm hover:bg-primary-100/80 dark:hover:bg-primary-900/70 transition-colors"
                href="${ema:tagcrumb:url}">
                #<ema:tagcrumb:title />
              </a>
            </li>
          </ema:each-crumb>
        </ema:tagcrumbs>
        <li class="inline-flex items-baseline">
          <span class="bg-primary-100/80 dark:bg-primary-900/70 text-primary-700 dark:text-primary-200 font-semibold px-1.5 py-0.5 rounded-sm">
            #<ema:tag:title />
          </span>
        </li>
      </ol>
    </nav>

    <!-- Child tags: tag-chip + count. The (N+M) notation reads as
         (subtags + notes-with-this-tag). -->
    <ema:childTags>
      <section class="mb-6">
        <header data-nosnippet class="text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400 mb-2">
          Subtags
        </header>
        <ul class="space-y-0.5">
          <ema:each-childTag>
            <li class="flex items-baseline gap-2 px-2 py-1 rounded-sm hover:bg-gray-50 dark:hover:bg-gray-900 transition-colors">
              <a href="${ema:childTag:url}"
                class="bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 font-mono px-1.5 py-0.5 rounded-sm hover:bg-primary-100/80 dark:hover:bg-primary-900/70 transition-colors">
                #<ema:childTag:title />
              </a>
              <span class="font-mono text-xs text-gray-400 dark:text-gray-500">
                <!-- DoNotFormat -->
                <ema:childTag:count-tag />+<ema:childTag:count-note />
                <!-- DoNotFormat -->
              </span>
            </li>
          </ema:each-childTag>
        </ul>
      </section>
    </ema:childTags>

    <!-- Notes tagged here: same chip language as queries / wikilinks. -->
    <ema:notes>
      <section>
        <header data-nosnippet class="text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400 mb-2">
          Notes
        </header>
        <ul class="space-y-0.5">
          <ema:each-note>
            <li class="flex items-baseline gap-2 px-2 py-1 rounded-sm hover:bg-gray-50 dark:hover:bg-gray-900 transition-colors">
              <span class="text-gray-300 dark:text-gray-700 select-none shrink-0" aria-hidden="true">→</span>
              <a class="bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 font-semibold tracking-tight px-2 py-0.5 rounded-sm hover:bg-primary-100/80 dark:hover:bg-primary-900/70 transition-colors"
                href="${ema:note:url}">
                <ema:note:title />
              </a>
            </li>
          </ema:each-note>
        </ul>
      </section>
    </ema:notes>
  </bind>
</apply>