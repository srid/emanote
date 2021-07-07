<ema:note:pandoc>
  <Para>
    <p class="mb-3">
      <inlines />
    </p>
  </Para>
  <Task:Checked>
    <!-- FIXME: Fix list styling to use flexbox, so task lists don't botch them up -->
    <label class="inline-flex items-start space-x-2">
      <input checked type="checkbox"
        class="flex-shrink-0 w-5 h-5 checked:bg-${theme}-600 checked:border-transparent">
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
      class="py-0.5 px-4 mb-3 italic border-l-4 bg-gray-50 text-gray-600 border-gray-400 quote">
      <blocks />
    </blockquote>
  </BlockQuote>
  <DefinitionList>
    <dl class="flex flex-col mb-3">
      <DefinitionList:Items>
        <div class="my-1">
          <dt class="font-bold text-l">
            <DefinitionList:Item:Term />
          </dt>
          <DefinitionList:Item:DescList>
            <div class="flex flex-col pl-1">
              <dd class="pl-2 my-1 text-gray-700 border-l-2">
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
    <!-- We use JavaScript because anchor links won't work if there is a <base> tag;
         see https://stackoverflow.com/a/34765348 
    -->
    <sup class="px-0.5"><a class="text-${theme}-600 hover:underline" href="javascript:;" onclick="document.location.hash='#fn${footnote:idx}';"><footnote:idx /></a></sup>
    <!-- DoNotFormat -->
  </Note:Ref>
  <Note:List>
    <div title="Footnotes"
      class="pt-2 mt-8 space-y-1 text-gray-500 transform scale-x-90 border-t-2">
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

  <BulletList>
    <ul class="my-3 ml-6 space-y-1 list-disc">
      <BulletList:Items>
        <li>
          <BulletList:Item />
        </li>
      </BulletList:Items>
    </ul>
  </BulletList>
  <OrderedList>
    <ul class="my-3 ml-6 space-y-1 list-decimal list-inside">
      <OrderedList:Items>
        <li>
          <OrderedList:Item />
        </li>
      </OrderedList:Items>
    </ul>
  </OrderedList>

  <!-- TODO: Expand the above kind of overriding (full DOM control) to other AST nodes (below) -->
  <PandocLink class="text-${theme}-600">
    <Internal class="font-bold hover:underline" />
    <External class="hover:underline" target="_blank" rel="noopener" />
  </PandocLink>
  <CodeBlock class="py-0.5 mb-3 text-sm" />
  <Code class="py-0.5 px-1 rounded bg-gray-700 text-sm text-white" />
  <Header>
    <h1 class="pb-2 mb-2 text-5xl font-bold text-center" />
    <h2 class="inline-block mt-6 mb-4 text-4xl font-bold text-gray-700 border-b-2" />
    <h3 class="mt-6 mb-2 text-3xl font-bold text-gray-700" />
    <h4 class="mt-6 mb-2 text-2xl font-bold text-gray-700" />
    <h5 class="mt-6 mb-2 text-xl font-bold text-gray-700" />
    <h6 class="mt-6 mb-2 text-xl font-bold text-gray-700" />
  </Header>

</ema:note:pandoc>