<div class="mb-4 overflow-auto text-sm text-gray-500 dark:text-gray-300">
  <context>
    <div class="pl-2 mt-2 border-l-2 border-${theme}-200 dark:border-${theme}-600 hover:border-${theme}-500 dark:hover:border-${theme}-400">
      <context:body>
        <PandocLink class="text-${theme}-600 dark:text-${theme}-400">
          <Internal class="bg-${theme}-50 dark:bg-${theme}-950 px-1.5 py-0.5 rounded-md font-semibold no-underline hover:bg-${theme}-100 dark:hover:bg-${theme}-900" />
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