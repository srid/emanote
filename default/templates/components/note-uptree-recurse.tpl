<li>

  <div class="forest-link text-gray-900">
    <a href="${node:url}">
      <node:text />
    </a>
  </div>

  <tree:open>
    <ul>
      <children>
        <apply template="note-uptree-recurse" />
      </children>
    </ul>
  </tree:open>
</li>