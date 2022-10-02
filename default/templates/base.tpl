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

  <style>
    /* Heist error element */
    strong.error {
      color: lightcoral;
      font-size: 90%;
      font-family: monospace;
    }

    /* External link icon */
    a[data-linkicon="show"]::after {
      /* filter converts black to rgb(156,163,175) */
      filter: invert(71%) sepia(3%) saturate(904%) hue-rotate(179deg) brightness(92%) contrast(87%);
      margin-left: 1px;
      content: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' height='1em' fill='none' viewBox='0 0 24 24' stroke='black' stroke-width='2'%3E%3Cpath stroke-linecap='round' stroke-linejoin='round' d='M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14' /%3E%3C/svg%3E");
    }

    a[data-linkicon="show"]:hover::after {
      /* filter converts black to  rgb(175,85,99) */
      filter: invert(32%) sepia(10%) saturate(834%) hue-rotate(176deg) brightness(92%) contrast(88%);
    }

    a[data-linkicon="show"][href^="mailto:"]::after {
      content: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' height='1em' fill='none' viewBox='0 0 24 24' stroke='black' stroke-width='2'%3E%3Cpath stroke-linecap='round' stroke-linejoin='round' d='M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z' /%3E%3C/svg%3E");
    }
  </style>
  <apply template="/templates/hooks/more-head" />

  <head-main />
  <apply template="components/stork/stork-search-head" />
</head>

<!-- DoNotFormat -->
<bind tag="theme"><ema:metadata><value var="template.theme" /></ema:metadata></bind>
<bind tag="iconSize">w-4 h-4 flex-shrink-0</bind>
<bind tag="bodyClass"><ema:metadata><value var="template.layout.base.bodyClass" /></ema:metadata></bind>
<!-- DoNotFormat -->

<body class="${bodyClass}">
  <body-main />
  <apply template="components/stork/stork-search" />
</body>

</html>