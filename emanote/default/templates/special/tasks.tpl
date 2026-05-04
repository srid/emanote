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
                  <a class="font-bold text-primary-700 dark:text-primary-300" href="${t:note:url}">
                    <t:note:title />
                  </a>
                </div>
              </div>
              <div class="">
                <t:tasks>
                  <task>
                    <div
                      class="bg-white dark:bg-gray-800 py-1.5 px-2 my-1 rounded shadow border-2 border-gray-200 dark:border-gray-600 hover:border-primary-600 dark:hover:border-primary-400">
                      <apply template="/templates/components/checkbox-unchecked">
                        <task:description>
                          <PandocLink class="transition-colors">
                            <Internal class="bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 font-semibold tracking-tight px-1 py-0.5 rounded-sm box-decoration-clone hover:bg-primary-100/80 dark:hover:bg-primary-900/70" />
                            <External class="text-primary-600 dark:text-primary-300 underline decoration-1 hover:decoration-2" target="_blank" rel="noopener" />
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