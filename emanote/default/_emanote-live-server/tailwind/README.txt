Files in this directory are excluded from the static site, as they are used only the live server.

To upgrade the tailwind CDN css, download it from a URL like
https://unpkg.com/tailwindcss@2.2.2/dist/tailwind.min.css and position it in the
appropriate directory, before updating the path in `LiveServerFiles.hs`.

Unfortunately there is no CDN for 3.x: https://github.com/tailwindlabs/tailwindcss/issues/6355
