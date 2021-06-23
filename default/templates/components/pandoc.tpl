<ema:note:pandoc>
  <Para>
    <p class="my-3">
      <inlines />
    </p>
  </Para>
  <Task:Checked>
    <!-- FIXME: Fix list styling to use flexbox, so task lists don't botch them up -->
    <label class="inline-flex items-start space-x-2">
      <input checked type="checkbox"
        class="flex-shrink-0 w-5 h-5 checked:bg-blue-600 checked:border-transparent">
      <span>
        <inlines />
      </span>
    </label>
  </Task:Checked>
  <Task:Unchecked>
    <label class="inline-flex items-start space-x-2">
      <input type="checkbox" class="flex-shrink-0 w-5 h-5">
      <span>
        <inlines />
      </span>
    </label>
  </Task:Unchecked>
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
  <DefinitionList>
    <dl class="flex flex-col">
      <DefinitionList:Items>
        <div class="my-1">
          <dt class="font-bold text-l">
            <DefinitionList:Item:Term />
          </dt>
          <DefinitionList:Item:DescList>
            <div class="flex flex-col pl-2">
              <dd class="text-gray-700">
                <DefinitionList:Item:Desc />
              </dd>
            </div>
          </DefinitionList:Item:DescList>
        </div>
      </DefinitionList:Items>
    </dl>
  </DefinitionList>
  <Note:Ref>
    <!-- DoNotFormat -->
    <sup class="px-0.5"><a class="text-${theme}-600 hover:underline" href="#fn${footnote:idx}"><footnote:idx /></a></sup>
    <!-- DoNotFormat -->
  </Note:Ref>
  <Note:List>
    <div title="Footnotes" class="pt-2 mt-4 space-y-1 text-gray-700 transform scale-90 border-t-2">
      <header class="font-semibold">Footnotes</header>
      <footnote>
        <div id="fn${footnote:idx}">
          <header class="italic">
            <footnote:idx />.
          </header>
          <div class="inline-block mb-2 ml-4">
            <footnote:content />
          </div>
        </div>
      </footnote>
    </div>
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
  <Header>
    <h1 class="pb-2 mt-4 mb-2 text-5xl font-bold text-center" />
    <h2 class="mt-4 mb-4 text-4xl font-bold text-gray-700" />
    <h3 class="mt-4 mb-2 text-3xl font-bold text-gray-700" />
    <h4 class="mt-4 mb-2 text-2xl font-bold text-gray-700" />
    <h5 class="mt-4 mb-2 text-xl font-bold text-gray-700" />
    <h6 class="mt-4 mb-2 text-xl font-bold text-gray-700" />
  </Header>

</ema:note:pandoc>