<ema:note:backlinks:daily>
  <div class="p-4 mt-8 bg-${theme}-50 dark:bg-${theme}-900 rounded flex-1">
    <header class="mb-2 text-xl font-semibold text-gray-500 dark:text-gray-400"
      title="Links to this page from daily notes">🗓️ Timeline</header>
    <ul class="space-y-1">
      <backlink>
        <li>
          <a class="header-font font-light text-gray-700 dark:text-gray-300 hover:text-${theme}-500 dark:hover:text-${theme}-400"
            href="${backlink:note:url}">
            <backlink:note:title />
          </a>
          <backlink:note:contexts>
            <apply template="context" />
          </backlink:note:contexts>
        </li>
      </backlink>
    </ul>
  </div>
</ema:note:backlinks:daily>