<apply template="/templates/special/base">
  <bind tag="special-main">
    <div class="w-full">
      <div class="flex flex-col items-center justify-left">
        <div class="w-full px-3 py-3 bg-gray-200">
          <p>All tasks indexed by their containing note</p>
        </div>
        <div class="w-full bg-gray-200 px-3">
          <ema:taskGroups>
            <taskGroup>
              <section class="mt-4 mb-6">
                <div class="mt-1 mb-2 py-0.5 px-1 rounded flex flex-row-reverse">
                  <t:note:breadcrumbs>
                    <each-crumb>
                      <li class="inline-flex items-center">
                        <a class="text-gray-600 opacity-50 hover:opacity-100" href="${crumb:url}">
                          <crumb:title />
                        </a>
                        <span class="text-gray-500 px-2">\</span>
                      </li>
                    </each-crumb>
                  </t:note:breadcrumbs>

                  <a class="flex-1 font-bold hover:bg-gray-100" href="${t:note:url}">
                    <t:note:title />
                  </a>
                </div>
                <div class="">
                  <t:tasks>
                    <task>
                      <div class="bg-white py-1.5 px-2 my-1 rounded-lg">
                        <label class="inline-flex items-start space-x-2">
                          <input type="checkbox" class="flex-shrink-0 w-5 h-5">
                          <span>
                            <task:description>
                              <PandocLink class="text-gray-600">
                                <Internal class="font-bold hover:bg-gray-50" />
                                <External class="hover:underline" target="_blank" rel="noopener" />
                              </PandocLink>
                              <OrderedList class="ml-4 space-y-1 list-decimal list-inside" />
                              <BulletList class="ml-4 space-y-1 list-decimal list-inside" />
                            </task:description>
                          </span>
                        </label>
                      </div>
                    </task>
                  </t:tasks>
                </div>
              </section>
            </taskGroup>
          </ema:taskGroups>
        </div>
      </div>
    </div>
  </bind>
</apply>