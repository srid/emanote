<apply template="/templates/special/base">
  <bind tag="special-main">
    <div class="w-full">
      <div class="w-full px-1">
        <ema:taskGroups>
          <taskGroup>
            <section class="mt-6 mb-8">
              <header class="flex flex-wrap items-baseline justify-between gap-x-4 gap-y-1 mb-3 pb-2 border-b border-gray-100 dark:border-gray-800">
                <a class="bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 font-semibold tracking-tight px-1.5 py-0.5 rounded-sm hover:bg-primary-100/80 dark:hover:bg-primary-900/70 transition-colors" href="${t:note:url}">
                  <t:note:title />
                </a>
                <ol class="text-xs text-gray-500 dark:text-gray-400 inline-flex flex-wrap items-center gap-x-1.5">
                  <t:note:breadcrumbs>
                    <each-crumb>
                      <li class="inline-flex items-center gap-x-1.5 [&:not(:last-child)]:after:content-['/'] [&:not(:last-child)]:after:text-gray-300 dark:[&:not(:last-child)]:after:text-gray-600">
                        <a class="hover:text-primary-700 dark:hover:text-primary-300 transition-colors" href="${crumb:url}">
                          <crumb:title />
                        </a>
                      </li>
                    </each-crumb>
                  </t:note:breadcrumbs>
                </ol>
              </header>
              <ul class="space-y-1.5">
                <t:tasks>
                  <task>
                    <li class="rounded-sm hover:bg-gray-50 dark:hover:bg-gray-900 transition-colors px-2 py-1">
                      <apply template="/templates/components/checkbox-unchecked">
                        <task:description>
                          <PandocLink class="transition-colors">
                            <Internal class="bg-primary-50/70 dark:bg-primary-950/50 text-primary-600 dark:text-primary-300 font-semibold tracking-tight px-1 py-0.5 rounded-sm box-decoration-clone hover:bg-primary-100/80 dark:hover:bg-primary-900/70" />
                            <External class="text-primary-600 dark:text-primary-300 underline decoration-1 hover:decoration-2" target="_blank" rel="noopener" />
                          </PandocLink>
                          <OrderedList class="ml-4 space-y-1 list-decimal list-inside marker:text-gray-400 dark:marker:text-gray-500" />
                          <BulletList class="ml-4 space-y-1 list-disc list-inside marker:text-gray-400 dark:marker:text-gray-500" />
                        </task:description>
                      </apply>
                    </li>
                  </task>
                </t:tasks>
              </ul>
            </section>
          </taskGroup>
        </ema:taskGroups>
      </div>
    </div>
  </bind>
</apply>