<!-- Timeline variant of query-default.tpl: same row shape, with a
     fixed-width date column (tabular-nums for clean alignment) in
     place of the leading arrow. -->
<nav class="mb-8">
  <header data-nosnippet class="text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400 mb-2">
    <query />
  </header>
  <ul class="space-y-0.5">
    <result>
      <li class="flex items-baseline gap-3 px-2 py-1 rounded-sm hover:bg-gray-50 dark:hover:bg-gray-900 transition-colors">
        <ema:note:metadata>
          <span data-nosnippet class="font-mono text-xs text-gray-500 dark:text-gray-400 [font-variant-numeric:tabular-nums] shrink-0 w-24">
            <value var="date" />
          </span>
        </ema:note:metadata>
        <a class="bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 font-semibold tracking-tight px-2 py-0.5 rounded-sm hover:bg-primary-100/80 dark:hover:bg-primary-900/70 transition-colors"
          href="${ema:note:url}">
          <ema:note:title />
        </a>
      </li>
    </result>
  </ul>
</nav>