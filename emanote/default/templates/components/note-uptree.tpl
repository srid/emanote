<!-- DoNotFormat -->
<bind tag="nodeClass"><ema:metadata><value var="template.uptree.nodeClass" /></ema:metadata></bind>
<!-- DoNotFormat -->

<nav id="uptree" class="flipped tree" style="transform-origin: 50%;">
  <ul class="root">
    <li>
      <ema:note:uptree:nonempty>
        <ul>
          <ema:note:uptree>
            <apply template="note-uptree-recurse" />
          </ema:note:uptree>
        </ul>
      </ema:note:uptree:nonempty>
    </li>
  </ul>
</nav>