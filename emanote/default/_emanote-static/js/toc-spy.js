// Highlight the TOC link for the section currently in view.
// Uses IntersectionObserver (passive, no scroll-loop layout thrashing);
// replaces the old window.onscroll approach from issue #520.

import { ready } from '@emanote/morph';

ready(() => {
  const tocLinks = document.querySelectorAll('a.--ema-toc');
  if (tocLinks.length === 0) return;

  const anchors = {};
  document.querySelectorAll('a.--ema-anchor').forEach((a) => {
    anchors[a.href] = a;
  });

  const observed = [];
  const visibility = new Map();
  tocLinks.forEach((link) => {
    const anchor = anchors[link.href];
    if (!anchor) return;
    const section = anchor.closest('h1, h2, h3, h4, h5, h6') || anchor;
    observed.push({ section, link });
    visibility.set(section, false);
  });

  const mark = 'toc-item-active';
  const setActive = (activeLink) => {
    tocLinks.forEach((l) => l.classList.remove(mark));
    if (activeLink) activeLink.classList.add(mark);
  };

  const pickActive = () => {
    let firstVisible = null;
    let lastAbove = null;
    for (const { section, link } of observed) {
      if (visibility.get(section)) {
        if (!firstVisible) firstVisible = link;
      } else if (section.getBoundingClientRect().top < 0) {
        lastAbove = link;
      }
    }
    setActive(firstVisible || lastAbove || observed[0]?.link);
  };

  const observer = new IntersectionObserver(
    (entries) => {
      for (const entry of entries) {
        visibility.set(entry.target, entry.isIntersecting);
      }
      pickActive();
    },
    { rootMargin: '-80px 0px -60% 0px', threshold: 0 },
  );

  for (const { section } of observed) observer.observe(section);
  pickActive();

  // The IO only fires on intersection-state changes. Tall sections
  // separated by skinny headings can all stay outside the active band
  // through a long scroll — no state change, no callback, pickActive
  // never re-runs, so the active link freezes on observed[0] (the
  // initial fallback). pickActive's lastAbove branch reads from
  // getBoundingClientRect() though, so it does need to re-run on
  // scroll. rAF-coalesce so we do at most one pass per frame even
  // under fast wheel/trackpad input.
  let scrollTick = false;
  window.addEventListener(
    'scroll',
    () => {
      if (scrollTick) return;
      scrollTick = true;
      requestAnimationFrame(() => {
        scrollTick = false;
        pickActive();
      });
    },
    { passive: true },
  );
});
