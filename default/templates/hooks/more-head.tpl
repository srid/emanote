<!-- What goes in this file will appear on near the end of <head>-->
<link rel="preload"
  href="${ema:emanoteStaticLayerUrl}/fonts/Maven_Pro/MavenPro-VariableFont_wght.ttf" as="font"
  type="font/ttf" crossorigin>

<style>
  @font-face {
    font-family: 'MavenPro';
    /* FIXME: This ought to be: ${ema:emanoteStaticLayerUrl}/fonts/Maven_Pro/MavenPro-VariableFont_wght.ttf */
    src: url(_emanote-static/fonts/Maven_Pro/MavenPro-VariableFont_wght.ttf) format("truetype");
    font-display: swap;
  }

  body {
    font-family: 'MavenPro', sans-serif;
    /* font-variation-settings: 'wght'300; */
  }

  a.mavenLinkBold {
    font-variation-settings: 'wght'500;
  }

  strong {
    font-variation-settings: 'wght'500;
  }

  h1,
  h2,
  h3,
  h4,
  h5,
  h6,
  header,
  .header-font {
    font-family: 'MavenPro', sans-serif;
  }
</style>