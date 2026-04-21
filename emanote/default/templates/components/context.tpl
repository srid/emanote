<div class="mb-4 overflow-auto text-sm text-gray-500 dark:text-gray-300">
  <context>
    <div class="pl-2 mt-2 border-l-2 border-primary-200 dark:border-primary-600 hover:border-primary-500 dark:hover:border-primary-400">
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