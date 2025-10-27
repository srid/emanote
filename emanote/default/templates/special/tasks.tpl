<apply template="/templates/special/base">
  <bind tag="special-main">
    <div class="w-full bg-gray-300 dark:bg-gray-700">
      <div class="w-full px-3 py-1">
        <ema:taskGroups>
          <taskGroup>
            <section class="mt-4 mb-6">
              <div class="mt-1 mb-2 py-0.5 px-1 rounded flex flex-row-reverse">
                <t:note:breadcrumbs>
                  <each-crumb>
                    <li class="inline-flex items-center">
                      <a class="text-gray-900 dark:text-gray-100 opacity-50 hover:opacity-100" href="${crumb:url}">
                        <crumb:title />
                      </a>
                      <span class="text-gray-700 dark:text-gray-300 px-2">\</span>
                    </li>
                  </each-crumb>
                </t:note:breadcrumbs>

                <div class="flex-1">
                  <a class="font-bold text-${theme}-700 dark:text-${theme}-300" href="${t:note:url}">
                    <t:note:title />
                  </a>
                </div>
              </div>
              <div class="">
                <t:tasks>
                  <task>
                    <div
                      class="bg-white dark:bg-gray-800 py-1.5 px-2 my-1 rounded shadow border-2 border-gray-200 dark:border-gray-600 hover:border-${theme}-600 dark:hover:border-${theme}-400">
                      <apply template="/templates/components/checkbox-unchecked">
                        <task:description>
                          <PandocLink class="text-${theme}-600 dark:text-${theme}-400">
                            <Internal class="bg-${theme}-50 dark:bg-${theme}-950 px-1.5 py-0.5 rounded-md font-semibold no-underline hover:bg-${theme}-100 dark:hover:bg-${theme}-900" />
                            <External class="underline decoration-1 hover:decoration-2" target="_blank" rel="noopener" />
                          </PandocLink>
                          <OrderedList class="ml-4 space-y-1 list-decimal list-inside" />
                          <BulletList class="ml-4 space-y-1 list-decimal list-inside" />
                        </task:description>
                      </apply>
                    </div>
                  </task>
                </t:tasks>
              </div>
            </section>
          </taskGroup>
        </ema:taskGroups>
      </div>
    </div>
  </bind>
</apply>