<!-- What goes in this file will appear on near the end of <head>-->
<link rel="preload"
  href="${ema:emanoteStaticLayerUrl}/fonts/Work_Sans/WorkSans-VariableFont_wght.ttf" as="font"
  type="font/ttf" crossorigin>

<style data-category="fonts">
  @font-face {
    font-family: 'WorkSans';
    /* FIXME: This ought to be: ${ema:emanoteStaticLayerUrl}/fonts/Work_Sans/WorkSans-VariableFont_wght.ttf */
    src: url(_emanote-static/fonts/Work_Sans/WorkSans-VariableFont_wght.ttf) format("truetype");
    font-display: swap;
  }

  body {
    font-family: 'WorkSans', sans-serif;

    /* Set the default font weight */
    font-variation-settings: 'wght' 350;

    /* Define css variables so they may be used outside of this file */
    --font-variation-bold: 'wght' 500;
    --font-variation-link: 'wght' 400;
  }

  a.mavenLinkBold {
    font-variation-settings: var(--font-variation-link);
  }

  strong {
    font-variation-settings: var(--font-variation-bold);
  }

  h1,
  h2,
  h3,
  h4,
  h5,
  h6,
  header,
  .header-font {
    font-family: 'WorkSans', sans-serif;
  }

  h1 {
    font-variation-settings: 'wght' 500;
  }

  h2 {
    font-variation-settings: 'wght' 400;
  }

  h3 {
    font-variation-settings: 'wght' 300;
  }
</style>