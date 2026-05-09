<!-- Note title attached as a top strip on the container card (see
     layouts/default.tpl). The md:rounded-t-lg matches the container's
     md:rounded-lg so the strip's top corners flow into the card's
     curve; the bottom is square + thin border so it sits flush against
     the inner row below.

     Calm tint instead of a saturated banner — neuron-style "tinted
     label" rather than a brand bar. The primary palette still anchors
     the page identity but at low intensity, so the prose below isn't
     out-shouted. -->
<header class="px-8 py-5 md:rounded-t-lg bg-primary-50 dark:bg-primary-950/40 text-primary-700 dark:text-primary-300 text-center border-b border-primary-200/60 dark:border-primary-800/40">
  <h1 class="text-3xl md:text-4xl font-semibold tracking-tight">
    <ema:note:title />
  </h1>
</header>
