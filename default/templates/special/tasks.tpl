<apply template="/templates/special/base">
  <bind tag="special-main">
    <div class="w-full bg-gray-300">
      <div class="w-full px-3 py-1">
        <ema:taskGroups>
          <taskGroup>
            <section class="mt-4 mb-6">
              <div class="mt-1 mb-2 py-0.5 px-1 rounded flex flex-row-reverse">
                <t:note:breadcrumbs>
                  <each-crumb>
                    <li class="inline-flex items-center">
                      <a class="text-gray-900 opacity-50 hover:opacity-100" href="${crumb:url}">
                        <crumb:title />
                      </a>
                      <span class="text-gray-700 px-2">\</span>
                    </li>
                  </each-crumb>
                </t:note:breadcrumbs>

                <div class="flex-1">
                  <a class="font-bold text-${theme}-700  hover:underline" href="${t:note:url}">
                    <t:note:title />
                  </a>
                </div>
              </div>
              <div class="">
                <t:tasks>
                  <task>
                    <div
                      class="bg-white py-1.5 px-2 my-1 rounded shadow border-2 hover:border-${theme}-600">
                      <apply template="/templates/components/checkbox-unchecked">
                        <task:description>
                          <PandocLink class="text-gray-600">
                            <Internal class="font-bold hover:bg-gray-50" />
                            <External class="hover:underline" target="_blank" rel="noopener" />
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