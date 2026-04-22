<!DOCTYPE HTML>

<!-- DoNotFormat -->
<bind tag="ema-lang"><ema:metadata><value var="page.lang" /></ema:metadata></bind>
<!-- DoNotFormat -->

<html lang="${ema-lang}">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <script>
    // Apply theme before first paint to avoid FOUC.
    // Priority: explicit user choice (localStorage) > OS preference.
    (function () {
      var stored = null;
      try { stored = localStorage.getItem('emanote-theme'); } catch (e) {}
      var prefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
      var isDark = stored === 'dark' || (stored !== 'light' && prefersDark);
      if (isDark) document.documentElement.classList.add('dark');
      document.documentElement.style.colorScheme = isDark ? 'dark' : 'light';
    })();
  </script>
  <title>
    <ema:titleFull />
  </title>
  <ema:metadata>
    <with var="page">
      <meta property="og:description" content="${value:description}" />
      <meta property="og:site_name" content="${value:siteTitle}" />
      <meta property="og:image" content="${value:image}" />
      <meta property="og:type" content="website" />
      <meta property="og:title" content="${ema:title}" />
      <with var="twitter">
        <meta name="twitter:card" content="${value:card}" />
      </with>
    </with>
    <with var="template">
      <base href="${value:baseUrl}" />
      <link href="${value:iconUrl}" rel="icon" />
    </with>
    <snippet var="page.headHtml" />
  </ema:metadata>
  <emaNoteFeedUrl />
  <tailwindCssShim />

  <apply template="/templates/styles" />
  <apply template="/templates/components/footnote-popup" />
  <apply template="/templates/hooks/more-head" />

  <head-main />
  <apply template="components/stork/stork-search-head" />
</head>

<!-- DoNotFormat -->
<bind tag="theme"><ema:metadata><value var="template.theme" /></ema:metadata></bind>
<bind tag="iconSize">w-5 h-5 flex-shrink-0</bind>
<bind tag="bodyClass"><ema:metadata><value var="template.base.bodyClass" /></ema:metadata></bind>
<!-- DoNotFormat -->

<body class="${bodyClass}">
  <body-main />
  <apply template="components/stork/stork-search" />
  <ema:metadata>
    <snippet var="page.bodyHtml" />
  </ema:metadata>
</body>

</html>