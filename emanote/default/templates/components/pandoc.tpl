<bind tag="clickable-icon">
  <a href="${ema:note:url}#${header:id}" class="--ema-anchor">
    <span
      class="hover:text-${theme}-400 dark:text-slate-200 group-hover:visible invisible cursor-pointer text-sm align-middle"
      aria-label="Copy link"><svg class="inline w-4" xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"
        stroke-linecap="round" stroke-linejoin="round">
        <path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"></path>
        <path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"></path>
      </svg></span>
  </a>
</bind>

<bind tag="heading-inlines-with-anchor">
  <inlines />
  <clickable-icon />
</bind>

<bind tag="header-class">group mt-6 mb-4 font-bold text-gray-700 dark:text-slate-200</bind>

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
      <a class="text-${theme}-600 dark:text-${theme}-200 hover:underline"
        href="${ema:note:url}#fn${footnote:idx}">
        <footnote:idx />
      </a>
    </sup>
  </Note:Ref>
  <Note:List>
    <div title="Footnotes"
      class="pt-2 mt-8 space-y-1 text-gray-500 dark:text-slate-300 transform scale-x-90 border-t-2">
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
  <PandocLink class="text-${theme}-600 dark:text-${theme}-300">
    <Internal class="mavenLinkBold hover:underline" />
    <External class="hover:underline" target="_blank" rel="noopener" />
  </PandocLink>
  <CodeBlock class="py-0.5 mb-3 text-sm" />
  <Code class="py-0.5 px-0.5 bg-gray-100 dark:bg-slate-200 dark:text-black" />

  <Header:1>
    <h1 id="${header:id}" class="${header-class} text-5xl">
      <heading-inlines-with-anchor />
    </h1>
  </Header:1>
  <Header:2>
    <h2 id="${header:id}" class="${header-class} text-4xl">
      <heading-inlines-with-anchor />
    </h2>
  </Header:2>
  <Header:3>
    <h3 id="${header:id}" class="${header-class} text-3xl">
      <heading-inlines-with-anchor />
    </h3>
  </Header:3>
  <Header:4>
    <h4 id="${header:id}" class="${header-class} text-2xl">
      <heading-inlines-with-anchor />
    </h4>
  </Header:4>
  <Header:5>
    <h5 id="${header:id}" class="${header-class} text-xl">
      <heading-inlines-with-anchor />
    </h5>
  </Header:5>
  <Header:6>
    <h6 id="${header:id}" class="${header-class} text-lg">
      <heading-inlines-with-anchor />
    </h6>
  </Header:6>
</ema:note:pandoc>