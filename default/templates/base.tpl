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
      <meta property="og:site_name" content="${value:siteTitle}" />
      <meta property="og:image" content="${value:image}" />
      <meta property="og:type" content="website" />
      <meta property="og:title" content="${ema:title}" />
    </with>
    <with var="template">
      <base href="${value:baseUrl}" />
      <link href="${value:iconUrl}" rel="icon" />
    </with>
    <snippet var="page.headHtml" />
  </ema:metadata>
  <tailwindCssShim />

  <style type="text/css">
    /* Heist error element */
    strong.error {
      color: lightcoral;
      font-size: 90%;
      font-family: monospace;
    }

    /* External link icon (see https://stackoverflow.com/a/66093928/5603549)*/
    a.emanote-external:not(.emanote-external-no-icon)::after {
      background: transparent url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 22 22' fill='rgb(156, 163, 175)' %3E%3Cpath d='M11 3a1 1 0 100 2h2.586l-6.293 6.293a1 1 0 101.414 1.414L15 6.414V9a1 1 0 102 0V4a1 1 0 00-1-1h-5z' /%3E%3Cpath d='M5 5a2 2 0 00-2 2v8a2 2 0 002 2h8a2 2 0 002-2v-3a1 1 0 10-2 0v3H5V7h3a1 1 0 000-2H5z' /%3E%3C/svg%3E") 0 0 no-repeat;
      background-size: 100% 100%;
      display: inline-block;
      height: 1em;
      width: 1em;
      content: '';
    }

    a.emanote-external:not(.emanote-external-no-icon):hover::after {
      background: transparent url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 22 22' fill='rgb(75, 85, 99)' %3E%3Cpath d='M11 3a1 1 0 100 2h2.586l-6.293 6.293a1 1 0 101.414 1.414L15 6.414V9a1 1 0 102 0V4a1 1 0 00-1-1h-5z' /%3E%3Cpath d='M5 5a2 2 0 00-2 2v8a2 2 0 002 2h8a2 2 0 002-2v-3a1 1 0 10-2 0v3H5V7h3a1 1 0 000-2H5z' /%3E%3C/svg%3E") 0 0 no-repeat;
    }
  </style>
  <apply template="/templates/hooks/more-head" />

  <head-main />
</head>

<!-- DoNotFormat -->
<bind tag="theme"><ema:metadata><value var="template.theme" /></ema:metadata></bind>
<bind tag="iconSize">w-4 h-4 flex-shrink-0</bind>
<bind tag="bodyClass"><ema:metadata><value var="template.layout.base.bodyClass" /></ema:metadata></bind>
<!-- DoNotFormat -->

<body class="${bodyClass}">
  <body-main />
</body>

</html>