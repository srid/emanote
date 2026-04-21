<bind tag="clickable-icon">
  <a href="${ema:note:url}#${header:id}" class="--ema-anchor ml-2">
    <span
      class="text-gray-400 hover:text-primary-500 dark:hover:text-primary-400 group-hover:opacity-100 opacity-0 cursor-pointer text-sm align-middle transition-opacity"
      aria-label="Copy link" title="Copy link to heading"><svg class="inline w-5 h-5" xmlns="http://www.w3.org/2000/svg"
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

<bind tag="h2-class">group mt-12 mb-6 font-bold text-gray-900 dark:text-gray-50 pb-2 border-b-2 border-gray-200 dark:border-gray-800</bind>
<bind tag="h3-class">group mt-10 mb-5 font-semibold text-gray-900 dark:text-gray-50</bind>
<bind tag="h4-class">group mt-8 mb-4 font-semibold text-gray-900 dark:text-gray-50</bind>
<bind tag="h5-class">group mt-6 mb-3 font-semibold text-gray-800 dark:text-gray-100</bind>
<bind tag="h6-class">group mt-6 mb-3 font-semibold text-gray-700 dark:text-gray-200</bind>

<ema:note:pandoc>
  <Para>
    <p class="mb-4 leading-relaxed">
      <inlines />
    </p>
  </Para>
  <Task:Checked>
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
      class="py-3 px-6 mb-6 italic border-l-4 bg-gray-50 dark:bg-gray-900 text-gray-700 dark:text-gray-300 border-gray-300 dark:border-gray-700 quote rounded-r-lg">
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
              <dd class="pl-2 my-1 text-gray-700 dark:text-gray-300 border-l-2 border-gray-300 dark:border-gray-600">
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
      <a class="text-primary-600 dark:text-primary-400 hover:underline" href="${ema:note:url}#fn${footnote:idx}">
        <footnote:idx />
      </a>
    </sup>
  </Note:Ref>
  <Note:List>
    <aside title="Footnotes"
      class="mt-12 pt-4 text-sm leading-relaxed text-gray-600 dark:text-gray-400 border-t-2 border-gray-200 dark:border-gray-800">
      <header class="mb-3 text-xs font-semibold uppercase tracking-wider text-gray-500 dark:text-gray-400">Footnotes</header>
      <ol class="list-decimal pl-6 space-y-3 marker:font-mono marker:text-xs marker:text-gray-400 dark:marker:text-gray-500">
        <footnote>
          <li id="fn${footnote:idx}" class="pl-2">
            <footnote:content />
          </li>
        </footnote>
      </ol>
    </aside>
  </Note:List>

  <BulletList>
    <ul class="my-4 ml-6 list-disc space-y-1.5 leading-relaxed marker:text-gray-400 dark:marker:text-gray-500">
      <BulletList:Items>
        <li>
          <BulletList:Item />
        </li>
      </BulletList:Items>
    </ul>
  </BulletList>
  <OrderedList>
    <ol class="my-4 ml-6 list-decimal space-y-1.5 leading-relaxed marker:font-semibold marker:text-primary-600 dark:marker:text-primary-400">
      <OrderedList:Items>
        <li class="pl-1">
          <OrderedList:Item />
        </li>
      </OrderedList:Items>
    </ol>
  </OrderedList>
  <HorizontalRule>
    <hr class="my-8 border-gray-200 dark:border-gray-800" />
  </HorizontalRule>
  <!-- TODO: Expand the above kind of overriding (full DOM control) to other AST nodes (below) -->
  <PandocLink class="text-primary-700 dark:text-primary-300">
    <Internal class="font-semibold no-underline hover:bg-primary-50 dark:hover:bg-primary-950 px-1 -mx-1 rounded transition-colors" />
    <External class="no-underline hover:underline decoration-primary-500 decoration-1 hover:decoration-2 underline-offset-4" target="_blank" rel="noopener" />
  </PandocLink>
  <CodeBlock class="py-4 pr-4 mb-6 text-sm font-mono rounded-lg overflow-x-auto" />
  <Code class="py-0 px-1.5 bg-gray-100 dark:bg-gray-800 text-gray-900 dark:text-gray-100 font-mono rounded text-[0.9em] leading-normal" />

  <Header:1>
    <h1 id="${header:id}" class="group mt-12 mb-8 font-bold text-gray-900 dark:text-gray-50 text-5xl">
      <heading-inlines-with-anchor />
    </h1>
  </Header:1>
  <Header:2>
    <h2 id="${header:id}" class="${h2-class} text-4xl">
      <heading-inlines-with-anchor />
    </h2>
  </Header:2>
  <Header:3>
    <h3 id="${header:id}" class="${h3-class} text-3xl">
      <heading-inlines-with-anchor />
    </h3>
  </Header:3>
  <Header:4>
    <h4 id="${header:id}" class="${h4-class} text-2xl">
      <heading-inlines-with-anchor />
    </h4>
  </Header:4>
  <Header:5>
    <h5 id="${header:id}" class="${h5-class} text-xl">
      <heading-inlines-with-anchor />
    </h5>
  </Header:5>
  <Header:6>
    <h6 id="${header:id}" class="${h6-class} text-lg">
      <heading-inlines-with-anchor />
    </h6>
  </Header:6>
</ema:note:pandoc>