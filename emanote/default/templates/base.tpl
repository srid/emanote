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

  <style>
    /* Heist error element */
    strong.error {
      color: lightcoral;
      font-size: 90%;
      font-family: monospace;
    }

    /* <kbd> styling */
    kbd {
      background-color: #eee;
      border-radius: 3px;
      border: 1px solid #b4b4b4;
      box-shadow:
        0 1px 1px rgba(0, 0, 0, 0.2),
        0 2px 0 0 rgba(255, 255, 255, 0.7) inset;
      display: inline-block;
      line-height: 1;
      padding: 2px 4px;
      white-space: nowrap;
    }

    /* Callouts */
    div.callout {
      background-color: #f5f5f5;
      padding: 1em 1em 0.5em;
      border-radius: 0.5em;
      margin-bottom: 1em;
    }

    .callout[data-callout="note"] {
      --callout-color: 8, 109, 221;
    }

    .callout[data-callout="info"] {
      --callout-color: 8, 109, 221;
    }

    .callout[data-callout="tip"] {
      --callout-color: 8, 191, 188;
    }

    .callout[data-callout="warning"] {
      --callout-color: 236, 117, 0;
    }

    .callout[data-callout="failure"] {
      --callout-color: 233, 49, 71;
    }

    .callout[data-callout="quote"] {
      --callout-color: 158, 158, 158;
    }

    /* To prevent overemphasis of link styles in callout titles */
    .callout .callout-title a {
      color: inherit;
      text-decoration: underline;
    }

    div.callout {
      background-color: rgba(var(--callout-color), 0.1);
    }

    .callout .callout-title {
      color: rgb(var(--callout-color));
    }

    div.callout-title {
      display: flex;
      align-items: center;
      margin-bottom: 0.5em;
      font-variation-settings: 'wght' 600;
    }

    div.callout-title div.callout-title-inner {
      margin-left: 0.5em;
    }

    a.--ema-toc:not(.toc-item-active) {
      background-color: #ffffff !important;
    }

    /* External link icon */
    a[data-linkicon=""]::after {
      content: ""
    }

    a[data-linkicon=none]::after {
      content: ""
    }

    a[data-linkicon="external"] {
      padding-right: 24px;
    }

    a[data-linkicon="external"]::after {
      margin-right: -24px;
      content: url('data:image/svg+xml,\
      <svg xmlns="http://www.w3.org/2000/svg" height="0.7em" viewBox="0 0 20 20"> \
        <g style="stroke:gray;stroke-width:1"> \
          <line x1="5" y1="5" x2="5" y2="14" /> \
          <line x1="14" y1="9" x2="14" y2="14" /> \
          <line x1="5" y1="14" x2="14" y2="14" /> \
          <line x1="5" y1="5" x2="9" y2="5"  /> \
          <line x1="10" y1="2" x2="17" y2="2"  /> \
          <line x1="17" y1="2" x2="17" y2="9" /> \
          <line x1="10" y1="9" x2="17" y2="2" style="stroke-width:1.0" /> \
        </g> \
      </svg>');
    }

    a[data-linkicon="external"][href^="mailto:"]::after {
      content: url('data:image/svg+xml,\
        <svg \
          xmlns="http://www.w3.org/2000/svg" \
          height="0.7em" \
          fill="none" \
          viewBox="0 0 24 24" \
          stroke="gray" \
          stroke-width="2"> \
          <path \
            stroke-linecap="round" \
            stroke-linejoin="round" \
            d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z" /> \
        </svg>');
    }
  </style>
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