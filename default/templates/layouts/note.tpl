<apply template="base">
  <bind tag="body-main">
    <div class="container mx-auto xl:max-w-screen-lg">
      <apply template="components/breadcrumbs" />
      <main>
        <apply template="components/pandoc" />
        <apply template="components/backlinks" />
        <apply template="components/metadata" />
      </main>
      <apply template="components/footer" />
    </div>
  </bind>
</apply>