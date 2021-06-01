<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>
    <ema:note:titleFull />
  </title>
  <with var="page">
    <meta property="og:description" content="${value:description}" />
    <meta property="og:site_name" content="${value:siteName}" />
  </with>
  <tailwindCssShim />
  <ema:metadata>
    <with var="template">
      <base href="${value:baseUrl}" />
      <link href="${value:iconUrl}" rel="icon" />
    </with>
  </ema:metadata>
  <snippet var="page.headHtml" />
  <head-extra />
</head>

<body class="overflow-y-scroll">
  <!-- DoNotFormat -->
  <bind tag="theme"><ema:metadata><value var="template.theme" /></ema:metadata></bind>
  <!-- DoNotFormat -->

  <body-main />
</body>

</html>