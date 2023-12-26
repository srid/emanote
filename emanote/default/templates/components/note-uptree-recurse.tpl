<li>

  <a href="${node:url}">
    <div class="${nodeClass} forest-link">
      <node:text />
    </div>
  </a>

  <tree:open>
    <ul>
      <children>
        <apply template="note-uptree-recurse" />
      </children>
    </ul>
  </tree:open>
</li>