<li>

  <div class="${nodeClass} forest-link">
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