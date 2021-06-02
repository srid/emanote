<apply template="base">
  <bind tag="body-main">
    <div class="container mx-auto xl:max-w-screen-lg">
      <main>
        <h2>index</h2>
        <ema:route-tree>
          <apply template="components/sidebar-tree" />
        </ema:route-tree>
      </main>
      <apply template="components/footer" />
    </div>
  </bind>
</apply>