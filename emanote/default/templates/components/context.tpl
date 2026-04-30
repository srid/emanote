<div class="text-sm text-gray-500 dark:text-gray-300 [&_p]:m-0 [&_p+p]:mt-[0.35em] [&_pre]:overflow-x-auto [&_table]:overflow-x-auto">
  <context>
    <div>
      <context:body>
        <PandocLink class="text-primary-600 dark:text-primary-400">
          <Internal class="bg-primary-50 dark:bg-primary-950 px-1.5 py-0.5 rounded-md font-semibold no-underline hover:bg-primary-100 dark:hover:bg-primary-900" />
          <External class="underline decoration-1 hover:decoration-2" target="_blank" rel="noopener" />
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