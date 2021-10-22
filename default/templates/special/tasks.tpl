<!-- TODO: Parametrize experimental thingy -->
<apply template="/templates/special/base">
  <bind tag="special-main">
    Tasks as follows:
    <ul>
      <ema:tasks>
        <task>
          <li>
            <task:description />
            (
            <a class="text-${theme}-600 font-bold hover:bg-${theme}-50" href="${note:url}">
              <note:title />
            </a>
            )
          </li>
        </task>
      </ema:tasks>
    </ul>
  </bind>
</apply>