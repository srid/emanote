<ema:note:pandoc>
  <Para>
    <p class="my-3">
      <inlines />
    </p>
  </Para>
  <Cite>
    <cite>
      <inlines />
    </cite>
  </Cite>
  <BlockQuote>
    <blockquote
      class="py-0.5 px-4 italic border-l-4 bg-gray-50 text-gray-600 border-gray-400 quote">
      <blocks />
    </blockquote>
  </BlockQuote>
  <Note:Ref>
    <!-- DoNotFormat -->
    <sup class="px-0.5"><a class="text-${theme}-600 hover:underline" href="#fn${footnote:idx}"><footnote:idx /></a></sup>
    <!-- DoNotFormat -->
  </Note:Ref>
  <Note:List>
    <ol title="Footnotes"
      class="pt-2 pl-2 mt-4 space-y-1 text-gray-700 list-decimal list-inside border-t-2">
      <footnote>
        <li id="fn${footnote:idx}">
          <div class="inline-block">
            <footnote:content />
          </div>
        </li>
      </footnote>
    </ol>
  </Note:List>

  <!-- TODO: Expand the above kind of overriding (full DOM control) to other AST nodes (below) -->
  <PandocLink class="text-${theme}-600">
    <Internal class="font-bold hover:underline" />
    <External class="hover:underline" target="_blank" rel="noopener" />
  </PandocLink>
  <CodeBlock class="py-0.5 text-sm" />
  <Code class="py-0.5 bg-gray-100 text-sm" />
  <OrderedList class="ml-4 space-y-1 list-decimal list-inside" />
  <BulletList class="ml-4 space-y-1 list-disc list-inside" />
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
    <div class="px-4 py-2 border-2 rounded">
      <header class="mb-2 font-bold text-gray-800">
        <query />
      </header>
      <!-- This should be a grid -->
      <div class="flex flex-col space-y-1">
        <result>
          <div class="flex items-center">
            <a class="text-${theme}-600 font-bold hover:underline" href="${note:url}">
              <note:title />
            </a>
          </div>
        </result>
      </div>
    </div>
  </CodeBlock:Query>
  <!-- TODO: DRY (same as above, except for date column)-->
  <CodeBlock:Query class="timeline">
    <div class="px-4 py-2 border-2 rounded">
      <header class="mb-2 font-bold text-gray-800">
        <query />
      </header>
      <!-- This should be a grid -->
      <div class="flex flex-col space-y-1">
        <result>
          <div class="flex items-center">
            <note:metadata>
              <span class="mr-4">
                <value var="date" />
              </span>
            </note:metadata>
            <a class="text-${theme}-600 font-bold hover:underline" href="${note:url}">
              <note:title />
            </a>
          </div>
        </result>
      </div>
    </div>
  </CodeBlock:Query>


</ema:note:pandoc>