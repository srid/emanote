<ema:note:pandoc>
  <Para>
    <p class="mb-3">
      <inlines />
    </p>
  </Para>
  <Task:Checked>
    <!-- FIXME: Fix list styling to use flexbox, so task lists don't botch them up -->
    <apply template="/templates/components/checkbox-checked">
      <inlines />
    </apply>
  </Task:Checked>
  <Task:Unchecked>
    <apply template="/templates/components/checkbox-unchecked">
      <inlines />
    </apply>
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
    <sup class="px-0.5">
      <a class="text-${theme}-600 hover:underline" href="${ema:note:url}#fn${footnote:idx}">
        <footnote:idx />
      </a>
    </sup>
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
  <HorizontalRule>
    <hr class="mb-3" />
  </HorizontalRule>
  <!-- TODO: Expand the above kind of overriding (full DOM control) to other AST nodes (below) -->
  <PandocLink class="text-${theme}-600">
    <Internal class="mavenLinkBold hover:underline" />
    <External class="hover:underline" target="_blank" rel="noopener" />
  </PandocLink>
  <CodeBlock class="py-0.5 mb-3 text-sm" />
  <Code class="py-0.5 px-0.5 bg-gray-100" />

  <Header:1>
    <h1 id="${header:id}" class="group pb-2 mb-2 text-5xl font-bold text-center">
      <inlines />
    </h1>
  </Header:1>
  <Header:2>
    <h2 id="${header:id}" class="group mt-6 mb-4 text-4xl font-bold text-gray-700 border-b-2">
      <a href="#${header:id}" class="group-hover:visible invisible text-sm align-middle">🔗</a>
      <inlines />
    </h2>
  </Header:2>
  <Header:3>
    <h3 id="${header:id}" class="group mt-6 mb-2 text-3xl font-bold text-gray-700">
      <a href="#${header:id}" class="group-hover:visible invisible text-sm align-middle">🔗</a>
      <inlines />
    </h3>
  </Header:3>
  <Header:4>
    <h4 id="${header:id}" class="mt-6 mb-2 text-3xl font-bold text-gray-700">
      <inlines />
    </h4>
  </Header:4>
  <Header:5>
    <h5 id="${header:id}" class="mt-6 mb-2 text-2xl font-bold text-gray-700">
      <a href="#${header:id}">⛓</a>
    </h5>
  </Header:5>
  <Header:6>
    <h6 id="${header:id}" class="mt-6 mb-2 text-2xl font-bold text-gray-700">
      <a href="#${header:id}">⛓</a>
    </h6>
  </Header:6>
</ema:note:pandoc>
