<!-- What goes in this file will appear on near the end of <head>-->
<link rel="preload"
  href="${ema:emanoteStaticLayerUrl}/fonts/Maven_Pro/WorkSans-VariableFont_wght.ttf" as="font"
  type="font/ttf" crossorigin>

<style>
  @font-face {
    font-family: 'WorkSans';
    /* FIXME: This ought to be: ${ema:emanoteStaticLayerUrl}/fonts/Maven_Pro/WorkSans-VariableFont_wght.ttf */
    src: url(_emanote-static/fonts/Work_Sans/WorkSans-VariableFont_wght.ttf) format("truetype");
    font-display: swap;
  }

  body {
    font-family: 'WorkSans', sans-serif;
    font-variation-settings: 'wght' 350;
  }

  a.mavenLinkBold {
    font-variation-settings: 'wght' 400;
  }

  strong {
    font-variation-settings: 'wght' 500;
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