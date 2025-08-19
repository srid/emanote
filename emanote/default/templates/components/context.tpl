<div class="mb-4 overflow-auto text-sm text-gray-500 dark:text-gray-300">
  <context>
    <div class="pl-2 mt-2 border-l-2 border-${theme}-200 dark:border-${theme}-600 hover:border-${theme}-500 dark:hover:border-${theme}-400">
      <context:body>
        <PandocLink class="text-gray-600 dark:text-gray-200">
          <Internal class="font-bold hover:bg-gray-50 dark:hover:bg-gray-600" />
          <External class="hover:underline" target="_blank" rel="noopener" />
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