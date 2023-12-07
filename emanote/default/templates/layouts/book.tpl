<!-- DoNotFormat -->
<bind tag="containerClass"><ema:metadata><value var="template.layout.book.containerClass" /></ema:metadata></bind>
<!-- DoNotFormat -->

<apply template="base">
  <bind tag="head-main"></bind>
  <bind tag="body-main">
    <div class="${containerClass}">

      <apply template="components/breadcrumbs" />

      <div id="container"
        class="flex flex-nowrap flex-col md:flex-row bg-gray-50 md:mt-8 md:shadow-2xl md:mb-8">
        <!-- Sidebar column -->
        <apply template="components/sidebar" />
        <!-- Main body column -->
        <apply template="components/body" />
      </div>
      <apply template="components/footer" />
    </div>
  </bind>
</apply>