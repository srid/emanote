<!DOCTYPE HTML>

<!-- DoNotFormat -->
<bind tag="ema-lang"><ema:metadata><value var="page.lang" /></ema:metadata></bind>
<!-- DoNotFormat -->

<html lang="${ema-lang}">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
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
  <apply template="/templates/hooks/more-head" />

  <head-main />
  <apply template="components/stork/stork-search-head" />
</head>

<!-- DoNotFormat -->
<bind tag="theme"><ema:metadata><value var="template.theme" /></ema:metadata></bind>
<bind tag="iconSize">w-4 h-4 flex-shrink-0</bind>
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