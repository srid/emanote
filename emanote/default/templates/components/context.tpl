<!-- Each <context> iteration emits its own <div>; a thin top-rule on
     every-but-first sibling makes the boundary between contexts obvious
     (otherwise adjacent contexts read as one continuous block of prose).
     Within a single context, [&_p+p]:mt-2 spaces adjacent paragraphs. -->
<div class="text-sm text-gray-500 dark:text-gray-300 [&>div+div]:border-t [&>div+div]:border-gray-200 dark:[&>div+div]:border-gray-700 [&>div+div]:pt-3 [&>div+div]:mt-3 [&_p]:m-0 [&_p+p]:mt-2 [&_pre]:overflow-x-auto [&_table]:overflow-x-auto">
  <context>
    <div>
      <context:body>
        <PandocLink class="transition-colors">
          <Internal class="bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 font-semibold tracking-tight px-1 py-0.5 rounded-sm box-decoration-clone hover:bg-primary-100/80 dark:hover:bg-primary-900/70" />
          <External class="text-primary-600 dark:text-primary-300 underline decoration-1 hover:decoration-2" target="_blank" rel="noopener" />
        </PandocLink>
        <OrderedList class="ml-4 space-y-1 list-decimal list-inside" />
        <BulletList class="ml-4 space-y-1 list-decimal list-inside" />
        <Task:Checked>
          <apply template="/templates/components/checkbox-checked">
            <inlines />
          </apply>
        </Task:Checked>
        <Task:Unchecked>
          <apply template="/templates/components/checkbox-unchecked">
            <inlines />
          </apply>
        </Task:Unchecked>
      </context:body>
    </div>
  </context>
</div>