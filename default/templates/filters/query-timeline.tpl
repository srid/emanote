<!-- TODO: DRY (same as query-default.tpl, except for date column)-->
<nav>
  <div class="mb-8">
    <header data-nosnippet class="pb-2 mb-2 font-semibold text-gray-600">
      <query />
    </header>
    <ul>
      <result>
        <li>
          <div class="flex flex-wrap flex-row my-2">
            <ema:note:metadata>
              <span data-nosnippet class="mr-2 text-right font-mono text-gray-600">
                <value var="date" />
              </span>
            </ema:note:metadata>
            <a class="flex-1 text-${theme}-600 mavenLinkBold border-l-2 pl-2 hover:underline"
              href="${ema:note:url}">
              <ema:note:title />
            </a>
          </div>
        </li>
      </result>
    </ul>
  </div>
</nav>