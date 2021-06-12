<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>
    <ema:titleFull />
  </title>
  <ema:metadata>
    <with var="page">
      <meta property="og:description" content="${value:description}" />
      <meta property="og:site_name" content="${value:siteName}" />
    </with>
    <with var="template">
      <base href="${value:baseUrl}" />
      <link href="${value:iconUrl}" rel="icon" />
    </with>
    <snippet var="page.headHtml" />
  </ema:metadata>
  <!-- CSSSTART -->
  <tailwindCssShim />
  <!-- CSSEND -->
  <head-extra />
</head>

<body class="overflow-y-scroll">
  <!-- DoNotFormat -->
  <bind tag="theme"><ema:metadata><value var="template.theme" /></ema:metadata></bind>
  <!-- DoNotFormat -->

  <body-main />
</body>

</html>