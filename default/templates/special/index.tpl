<apply template="base">
  <bind tag="body-main">
    <div class="container mx-auto xl:max-w-screen-lg">
      <main>
        <h1 class="pb-2 mt-2 mb-2 text-6xl text-center">
          <ema:title />
        </h1>
        <a href="/">Back to Home</a>
        <div class="pt-1 pb-2 rounded bg-${theme}-50 pl-4">
          <ema:route-tree>
            <apply template="components/sidebar-tree" />
          </ema:route-tree>
        </div>
      </main>
      <apply template="components/footer" />
    </div>
  </bind>
</apply>