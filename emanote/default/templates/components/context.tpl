<div class="mb-4 overflow-x-auto text-sm text-gray-500 dark:text-gray-300">
  <context>
    <div class="mt-2">
      <context:body>
        <PandocLink class="text-primary-700 dark:text-primary-300">
          <Internal class="font-semibold no-underline hover:underline decoration-primary-500" />
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