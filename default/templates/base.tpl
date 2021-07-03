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

  <!-- Heist error element -->
  <style type="text/css">
    strong.error {
      color: lightcoral;
      font-size: 90%;
      font-family: monospace;
    }
  </style>
  <apply template="/templates/hooks/more-head" />
</head>

<!-- DoNotFormat -->
<bind tag="theme"><ema:metadata><value var="template.theme" /></ema:metadata></bind>
<bind tag="iconSize">w-4 h-4 flex-shrink-0</bind>
<!-- DoNotFormat -->

<body class="overflow-y-scroll bg-gray-400">
  <body-main />
</body>

</html>