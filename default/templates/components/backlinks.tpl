<ema:note:backlinks>
  <div class="p-4 mt-8 bg-gray-100 rounded">
    <header class="mb-2 text-xl font-semibold text-gray-500">Links to this page</header>
    <ul class="space-y-1">
      <backlink>
        <li>
          <a class="text-${theme}-600 font-bold hover:bg-${theme}-50" href="${backlink:note:url}">
            <backlink:note:title />
          </a>
          <div class="mb-4 overflow-auto text-sm text-gray-500">
            <!-- TODO: How to style the backlinks list item element? Or render each separately?
              Also, group contexts from same source.
            -->
            <backlink:note:context>
              <PandocLink class="text-gray-600">
                <Internal class="font-bold hover:bg-gray-50" />
                <External class="hover:underline" target="_blank" rel="noopener" />
              </PandocLink>
              <OrderedList class="ml-4 space-y-1 list-decimal list-inside" />
              <BulletList class="ml-4 space-y-1 list-decimal list-inside" />
            </backlink:note:context>
          </div>
        </li>
      </backlink>
    </ul>
  </div>
</ema:note:backlinks>