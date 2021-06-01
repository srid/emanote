<ema:note:pandoc>
  <PandocLink class="text-${theme}-600">
    <Internal class="font-bold hover:bg-${theme}-50" />
    <External class="hover:underline" target="_blank" rel="noopener" />
  </PandocLink>
  <Para class="my-3" />
  <!-- FIXME: See the TODO in Pandoc.hs -->
  <Note class="inline-block float-right clear-right w-2/5 text-left bg-red-100" />
  <CodeBlock class="py-0.5 text-sm" />
  <Code class="py-0.5 bg-gray-100 text-sm" />
  <OrderedList class="ml-4 space-y-1 list-decimal list-inside" />
  <BulletList class="ml-4 space-y-1 list-disc list-inside" />
  <BlockQuote
    class="py-0.5 px-4 italic border-l-4 bg-gray-50 text-gray-600 border-gray-400 quote" />
  <!-- TODO: Table styling -->
  <Header>
    <h1 class="pb-2 mt-4 mb-2 text-6xl text-center" />
    <h2 class="mt-4 mb-4 text-5xl text-gray-700" />
    <h3 class="mt-4 mb-2 text-4xl text-gray-700" />
    <h4 class="mt-4 mb-2 text-3xl text-gray-700" />
    <h5 class="mt-4 mb-2 text-2xl text-gray-700" />
    <h6 class="mt-4 mb-2 text-xl text-gray-700" />
  </Header>
  <CodeBlock:Query>
    <div class="p-2 border-2 rounded shadow hover:shadow-lg hover:border-gray-400">
      <header class="text-gray-800 font-bold mb-2">
        <query />
      </header>
      <result>
        <!-- This should be a grid -->
        <div class="flex flex-row items-end space-y-2">
          <div class="flex items-center pr-2">
            <note:metadata>
              <value var="date" />
            </note:metadata>
          </div>
          <div class="flex items-center">
            <a class="text-${theme}-600 font-bold hover:bg-${theme}-50" href="${note:url}">
              <note:title />
            </a>
          </div>
        </div>
      </result>
    </div>
  </CodeBlock:Query>

  <CodeBlock:Query class="timeline">
    <div class="p-2 border-2 rounded shadow hover:shadow-lg hover:border-gray-400">
      <header class="text-gray-800 font-bold mb-2">
        <query />
      </header>
      <result>
        <!-- This should be a grid -->
        <div class="flex flex-row items-end space-y-2">
          <div class="flex items-center pr-2">
            <note:metadata>
              <value var="date" />
            </note:metadata>
          </div>
          <div class="flex items-center">
            <a class="text-${theme}-600 font-bold hover:bg-${theme}-50" href="${note:url}">
              <note:title />
            </a>
          </div>
        </div>
      </result>
    </div>
  </CodeBlock:Query>


</ema:note:pandoc>