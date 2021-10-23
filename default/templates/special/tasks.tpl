<!-- TODO: Parametrize experimental thingy -->
<apply template="/templates/special/base">
  <bind tag="special-main">
    <div class="w-full">
      <div class="flex flex-col items-center justify-left">
        <div class="w-full px-3 py-3 bg-gray-200">
          <p>All tasks indexed by their containing note</p>
        </div>
        <div class="w-full bg-white px-3">
          <ema:taskGroups>
            <taskGroup>
              <section class="my-4">
                <div>
                  <a class="text-${theme}-600 font-bold hover:bg-${theme}-50" href="${t:note:url}">
                    <t:note:title />
                  </a>
                </div>
                <t:tasks>
                  <task>
                    <div>
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
              </section>
            </taskGroup>
          </ema:taskGroups>
        </div>
      </div>
    </div>
  </bind>
</apply>