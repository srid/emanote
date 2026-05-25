--- slides.lua — turn `:::slides` divs into a navigable Markdown presentation.
---
--- Author writes a fenced div with class `slides`; each level-2 header
--- inside it begins a new slide. The filter wraps each slide in its own
--- `<section class="emanote-slide">`, prepends a nav strip with anchor
--- links, and injects scoped CSS + arrow-key JS so the result reads as a
--- real horizontal-snap deck.
---
--- Layout uses CSS scroll-snap (no framework, no reveal.js); the JS is a
--- ~20-line module that adds keyboard nav. Both blocks are emitted as raw
--- HTML so the demo page is self-contained — no template surgery, no
--- bundled assets in the default layer.
---
--- Example:
---
---   ::: slides
---
---   ## Hello
---
---   Welcome to a Markdown presentation.
---
---   ## Why
---
---   - It's just Markdown
---   - It hot-reloads
---   :::
---
--- HTML render-time filter; the emitted RawBlocks target HTML.

if FORMAT ~= "html" then
  return {}
end

local function slugify(s, idx)
  local base = s:lower():gsub("[^%w]+", "-"):gsub("^%-+", ""):gsub("%-+$", "")
  if base == "" then base = "slide" end
  return string.format("%s-%d", base, idx)
end

local function header_text(header)
  return pandoc.utils.stringify(header.content)
end

local css = [[
<style>
.emanote-slides {
  border: 1px solid var(--color-gray-200);
  border-radius: 0.5rem;
  margin: 2rem 0;
  background: var(--color-gray-50);
}
.emanote-slides-nav {
  display: flex;
  flex-wrap: wrap;
  gap: 0.25rem;
  padding: 0.5rem 0.75rem;
  border-bottom: 1px solid var(--color-gray-200);
  font-size: 0.875rem;
}
.emanote-slides-nav [data-slide-id] {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-width: 1.75rem;
  height: 1.75rem;
  padding: 0 0.5rem;
  border-radius: 0.25rem;
  border: 1px solid transparent;
  color: var(--color-gray-700);
  text-decoration: none;
  font-variant-numeric: tabular-nums;
}
.emanote-slides-nav [data-slide-id]:hover {
  border-color: var(--color-gray-300);
  background: var(--color-gray-100);
}
.emanote-slides-nav [data-slide-id].active {
  background: var(--color-primary-600);
  border-color: var(--color-primary-600);
  color: #fff;
}
.emanote-slides-nav [data-slide-id].active:hover {
  background: var(--color-primary-700);
  border-color: var(--color-primary-700);
}
.emanote-slides-nav .emanote-slides-fs {
  margin-left: auto;
  border: 1px solid transparent;
  background: transparent;
  color: var(--color-gray-700);
  border-radius: 0.25rem;
  width: 1.75rem;
  height: 1.75rem;
  padding: 0;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
}
.emanote-slides-nav .emanote-slides-fs:hover {
  border-color: var(--color-gray-300);
  background: var(--color-gray-100);
}
.emanote-slides-nav .emanote-slides-fs svg { width: 1rem; height: 1rem; display: block; }
.emanote-slides-nav .emanote-slides-fs .icon-exit { display: none; }
.emanote-slides:fullscreen .emanote-slides-nav .emanote-slides-fs .icon-enter { display: none; }
.emanote-slides:fullscreen .emanote-slides-nav .emanote-slides-fs .icon-exit { display: block; }
.emanote-slides-track {
  display: flex;
  overflow-x: auto;
  scroll-snap-type: x mandatory;
  scroll-behavior: smooth;
  aspect-ratio: 16 / 9;
  background: var(--color-gray-50);
}
/* Fullscreen mode: fill the viewport, kill margins/border, scale slide
   padding so the deck doesn't read as a tiny embedded frame. */
.emanote-slides:fullscreen {
  margin: 0;
  border: none;
  border-radius: 0;
  display: flex;
  flex-direction: column;
  background: var(--color-gray-50);
}
.emanote-slides:fullscreen .emanote-slides-track {
  flex: 1 1 auto;
  aspect-ratio: auto;
  height: auto;
}
.emanote-slides:fullscreen .emanote-slide { padding: 4rem 6rem; }
.emanote-slides:fullscreen .emanote-slide > h2:first-child { font-size: 3rem; }
.emanote-slides:fullscreen .emanote-slide :is(p, ul, ol, pre) { font-size: 1.5rem; }
.emanote-slide {
  flex: 0 0 100%;
  scroll-snap-align: start;
  scroll-snap-stop: always;
  padding: 2.5rem 3rem;
  display: flex;
  flex-direction: column;
  justify-content: center;
  overflow-y: auto;
  color: var(--color-gray-900);
}
.emanote-slide > h2:first-child {
  margin-top: 0;
  font-size: 2rem;
  color: var(--color-gray-900);
}
.emanote-slide :is(p, ul, ol, pre) { font-size: 1.125rem; }

.dark .emanote-slides {
  background: var(--color-gray-900);
  border-color: var(--color-gray-800);
}
.dark .emanote-slides-nav { border-bottom-color: var(--color-gray-800); }
.dark .emanote-slides-nav [data-slide-id] { color: var(--color-gray-300); }
.dark .emanote-slides-nav [data-slide-id]:hover {
  border-color: var(--color-gray-700);
  background: var(--color-gray-800);
}
.dark .emanote-slides-nav [data-slide-id].active {
  background: var(--color-primary-500);
  border-color: var(--color-primary-500);
  color: var(--color-gray-950);
}
.dark .emanote-slides-nav [data-slide-id].active:hover {
  background: var(--color-primary-400);
  border-color: var(--color-primary-400);
}
.dark .emanote-slides-track { background: var(--color-gray-900); }
.dark .emanote-slide,
.dark .emanote-slide > h2:first-child { color: var(--color-gray-100); }
</style>
]]

local js = [[
<script>
(() => {
  // Nav buttons drive horizontal scroll on the track. Deep-linking
  // via `#slide-id` works one-shot at load time via `location.hash`,
  // but we never push/replace the hash on click — the back stack
  // would fill with intra-deck transitions and interact badly with
  // the live-server's morph-DOM navigation.
  for (const deck of document.querySelectorAll('.emanote-slides')) {
    // Ema's live-server uses morph-DOM for in-app navigation; if a
    // slides page is re-entered the script tag may run again over a
    // deck node that already has listeners. Idempotent guard: the
    // first run flips a dataset flag; subsequent runs skip.
    if (deck.dataset.emanoteSlidesInited) continue;
    const track = deck.querySelector('.emanote-slides-track');
    const slides = [...deck.querySelectorAll('.emanote-slide')];
    if (!track || slides.length === 0) continue;
    deck.dataset.emanoteSlidesInited = '1';
    deck.tabIndex = 0;
    const indexOf = (id) => slides.findIndex(s => s.id === id);
    // Scroll the track horizontally to the chosen slide. Using
    // track.scrollTo (vs slides[j].scrollIntoView) keeps scrolling
    // contained to the track — scrollIntoView with block:'nearest'
    // has been observed to drop horizontal scroll updates when the
    // surrounding page is the actual scroll-container winner.
    //
    // Use getBoundingClientRect rather than offsetLeft: offsetLeft
    // is relative to the slide's offsetParent, which is whatever
    // ancestor happens to be positioned. The track itself has no
    // explicit position, so offsetLeft mixes track-internal scroll
    // coords with outer-wrapper coords and current() mis-identifies
    // the visible slide whenever the track has a non-zero left in
    // the page (any sidebar layout, in practice).
    const go = (i, smooth = true) => {
      const j = Math.max(0, Math.min(slides.length - 1, i));
      const slideLeft = slides[j].getBoundingClientRect().left;
      const trackLeft = track.getBoundingClientRect().left;
      track.scrollTo({
        left: track.scrollLeft + (slideLeft - trackLeft),
        behavior: smooth ? 'smooth' : 'instant',
      });
    };
    const current = () => {
      const trackRect = track.getBoundingClientRect();
      const trackCenter = trackRect.left + trackRect.width / 2;
      let best = 0, bestDist = Infinity;
      slides.forEach((s, i) => {
        const r = s.getBoundingClientRect();
        const d = Math.abs(r.left + r.width / 2 - trackCenter);
        if (d < bestDist) { bestDist = d; best = i; }
      });
      return best;
    };
    const navLinks = [...deck.querySelectorAll('.emanote-slides-nav [data-slide-id]')];
    const setActive = (id) => {
      for (const a of navLinks) {
        a.classList.toggle('active', a.dataset.slideId === id);
      }
    };
    navLinks.forEach(a => {
      a.addEventListener('click', (e) => {
        const i = indexOf(a.dataset.slideId);
        if (i < 0) return;
        e.preventDefault();
        e.stopPropagation();
        go(i);
        // Move focus to the deck so subsequent arrow keys reach the
        // deck-level keydown handler.
        requestAnimationFrame(() => deck.focus({ preventScroll: true }));
      });
    });
    // Fullscreen toggle: requestFullscreen on click, exitFullscreen
    // when already fullscreen. The CSS swaps the icon based on
    // :fullscreen pseudo-class — no manual class juggling.
    const fsBtn = deck.querySelector('.emanote-slides-fs');
    if (fsBtn) {
      fsBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        if (document.fullscreenElement === deck) {
          document.exitFullscreen?.();
        } else {
          // requestFullscreen rejects when the user denies the
          // permission gesture or the browser refuses (e.g. the
          // page isn't allowed to take fullscreen at all). Log so
          // a click that does nothing isn't a silent mystery.
          deck.requestFullscreen?.()
            .then(() => deck.focus({ preventScroll: true }))
            .catch((err) =>
              console.warn('emanote-slides: fullscreen request denied or unavailable:', err)
            );
        }
      });
    }
    if (slides[0]) setActive(slides[0].id);
    const observer = new IntersectionObserver((entries) => {
      let best = null, bestRatio = 0;
      for (const entry of entries) {
        if (entry.isIntersecting && entry.intersectionRatio > bestRatio) {
          best = entry.target;
          bestRatio = entry.intersectionRatio;
        }
      }
      if (best) setActive(best.id);
    }, { root: track, threshold: [0.5, 0.75, 1] });
    slides.forEach(s => observer.observe(s));
    deck.addEventListener('keydown', (e) => {
      if (e.key === 'ArrowRight' || e.key === 'PageDown') { e.preventDefault(); go(current() + 1); }
      else if (e.key === 'ArrowLeft' || e.key === 'PageUp') { e.preventDefault(); go(current() - 1); }
      else if (e.key === 'Home') { e.preventDefault(); go(0); }
      else if (e.key === 'End') { e.preventDefault(); go(slides.length - 1); }
    });
    if (location.hash) {
      const i = indexOf(location.hash.slice(1));
      if (i >= 0) requestAnimationFrame(() => go(i, false));
    }
  }
})();
</script>
]]

--- Split a list of blocks into slide groups: every level-2 Header begins
--- a new slide; blocks before the first header are dropped (an authoring
--- convention that lets users put a leading paragraph as a deck intro
--- if they want it outside the slides themselves — but currently we
--- treat such pre-content as discarded; could change later).
local function split_into_slides(blocks)
  local slides = {}
  local current = nil
  for _, b in ipairs(blocks) do
    if b.t == "Header" and b.level == 2 then
      if current then table.insert(slides, current) end
      current = { header = b, body = {} }
    elseif current then
      table.insert(current.body, b)
    end
  end
  if current then table.insert(slides, current) end
  return slides
end

-- Inline SVG icons for the fullscreen toggle (24x24 viewBox; CSS sizes
-- them down to 1rem). Two icons, one shown at a time via the
-- :fullscreen pseudo-class — no JS class toggling needed.
local fs_icons = [[<svg class="icon-enter" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M4 9V4h5"/><path d="M20 9V4h-5"/><path d="M4 15v5h5"/><path d="M20 15v5h-5"/></svg><svg class="icon-exit" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M9 4v5H4"/><path d="M15 4v5h5"/><path d="M9 20v-5H4"/><path d="M15 20v-5h5"/></svg>]]

local function build_deck(slides)
  local nav_links = {}
  local sections = {}
  for i, s in ipairs(slides) do
    local title = header_text(s.header)
    local id = slugify(title, i)
    s.header.identifier = id
    -- Use <button>, not <a>. A bare-hash <a href="#id"> gets resolved
    -- to "/#id" via <base href="/">, and the static link-checker
    -- follows it back to root and reports a missing anchor. <a> with
    -- no href fails htmlproofer's "missing reference" check. <button>
    -- carries no link semantics for the checker; JS handles
    -- navigation via the data-slide-id attribute.
    local link = pandoc.RawInline(
      "html",
      string.format(
        '<button type="button" data-slide-id="%s" title="%s">%d</button>',
        id, title, i
      )
    )
    table.insert(nav_links, link)
    if i < #slides then
      table.insert(nav_links, pandoc.Space())
    end
    local body = { s.header }
    for _, b in ipairs(s.body) do table.insert(body, b) end
    local section = pandoc.Div(body, pandoc.Attr(id, { "emanote-slide" }, {}))
    table.insert(sections, section)
  end
  -- Fullscreen toggle: sits on the right side of the nav (margin-left:
  -- auto in CSS pushes it past the slide-number links). Inline raw HTML
  -- because pandoc's Builder doesn't model <button>.
  local fs_button =
    pandoc.RawInline(
      "html",
      string.format(
        '<button type="button" class="emanote-slides-fs" aria-label="Toggle fullscreen" title="Fullscreen">%s</button>',
        fs_icons
      )
    )
  table.insert(nav_links, fs_button)
  local nav = pandoc.Div(
    { pandoc.Plain(nav_links) },
    pandoc.Attr("", { "emanote-slides-nav" }, {})
  )
  local track = pandoc.Div(sections, pandoc.Attr("", { "emanote-slides-track" }, {}))
  return pandoc.Div({ nav, track }, pandoc.Attr("", { "emanote-slides" }, {}))
end

local emitted_assets = false

function Div(el)
  if not el.classes:includes("slides") then return nil end
  local slides = split_into_slides(el.content)
  if #slides == 0 then return nil end
  local deck = build_deck(slides)
  if not emitted_assets then
    emitted_assets = true
    return { pandoc.RawBlock("html", css), deck, pandoc.RawBlock("html", js) }
  end
  return deck
end
