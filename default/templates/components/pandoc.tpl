<ema:note:pandoc>
  <PandocLink class="text-${theme}-600">
    <Internal class="font-bold hover:underline" />
    <External class="hover:underline" target="_blank" rel="noopener" />
  </PandocLink>
  <Para class="my-3" />
  <Note:Ref>
    <!-- DoNotFormat -->
    <sup class="px-0.5"><a class="text-${theme}-600 hover:underline" href="#fn${footnote:idx}"><footnote:idx /></a></sup>
    <!-- DoNotFormat -->
  </Note:Ref>
  <Note:List>
    <ol title="Footnotes"
      class="list-decimal list-inside space-y-1 mt-4 pt-2 pl-2 border-t-2 text-gray-700">
      <footnote>
        <li id="fn${footnote:idx}">
          <div class="inline-block">
            <footnote:content />
          </div>
        </li>
      </footnote>
    </ol>
  </Note:List>
  <CodeBlock class="py-0.5 text-sm" />
  <Code class="py-0.5 bg-gray-100 text-sm" />
  <OrderedList class="ml-4 space-y-1 list-decimal list-inside" />
  <BulletList class="ml-4 space-y-1 list-disc list-inside" />
  <BlockQuote
    class="py-0.5 px-4 italic border-l-4 bg-gray-50 text-gray-600 border-gray-400 quote" />
  <!-- TODO: Table styling -->
  <Header>
    <h1 class="pb-2 mt-4 mb-2 text-6xl font-bold text-center" />
    <h2 class="mt-4 mb-4 text-4xl font-bold text-gray-700" />
    <h3 class="mb-2 text-3xl font-bold text-gray-700 jmt-4" />
    <h4 class="mt-4 mb-2 text-2xl font-bold text-gray-700" />
    <h5 class="mt-4 mb-2 text-xl font-bold text-gray-700" />
    <h6 class="mt-4 mb-2 text-xl font-bold text-gray-700" />
  </Header>
  <CodeBlock:Query>
    <div class="p-2 border-2 rounded shadow hover:shadow-lg hover:border-gray-400">
      <header class="mb-2 font-bold text-gray-800">
        <query />
      </header>
      <result>
        <!-- This should be a grid -->
        <div class="flex flex-row items-end space-y-2">
          <div class="flex items-center">
            <a class="text-${theme}-600 font-bold hover:underline" href="${note:url}">
              <note:title />
            </a>
          </div>
        </div>
      </result>
    </div>
  </CodeBlock:Query>

  <CodeBlock:Query class="timeline">
    <div class="p-2 border-2 rounded shadow hover:shadow-lg hover:border-gray-400">
      <header class="mb-2 font-bold text-gray-800">
        <query />
      </header>
      <result>
        <!-- This should be a grid -->
        <div class="flex flex-row items-end space-y-2">
          <div class="flex items-center pr-2 font-light">
            <note:metadata>
              <value var="date" />
            </note:metadata>
          </div>
          <div class="flex items-center">
            <a class="text-${theme}-600 font-bold hover:underline" href="${note:url}">
              <note:title />
            </a>
          </div>
        </div>
      </result>
    </div>
  </CodeBlock:Query>


</ema:note:pandoc>