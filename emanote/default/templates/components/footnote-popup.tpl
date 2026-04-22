<!-- Progressive-enhancement popup for footnote references.

     Binds to the stable contract `data-footnote-ref` / `data-footnote-id`
     / `data-footnote-list` / `data-footnote-embed` emitted by pandoc.tpl
     and embed-note.tpl, not to Pandoc's `.footnote-ref` / `.footnote-list`
     class names (those are stable-by-stasis, not by encapsulation).

     The bottom `<aside>` stays visible as a no-JS / print fallback; the
     popup is a convenience layer, not a replacement. -->

<style data-category="footnote-popup">
  #emanote-footnote-popover {
    margin: 0;
    padding: 0;
    border: 0;
    background: transparent;
    position: fixed;
    max-width: min(32rem, calc(100vw - 2rem));
    width: max-content;
    font-size: 0.95rem;
    line-height: 1.6;
    color: inherit;
    z-index: 9999;
  }
  #emanote-footnote-popover::backdrop { background: transparent; }
  .emanote-footnote-popup-body {
    background: white;
    color: var(--color-gray-800, #1f2937);
    padding: 0.875rem 1.125rem;
    border-radius: 0.5rem;
    border: 1px solid var(--color-gray-200, #e5e7eb);
    box-shadow: 0 10px 25px -5px rgb(0 0 0 / 0.15), 0 8px 10px -6px rgb(0 0 0 / 0.1);
    position: relative;
  }
  .dark .emanote-footnote-popup-body {
    background: var(--color-gray-900, #111827);
    color: var(--color-gray-200, #e5e7eb);
    border-color: var(--color-gray-700, #374151);
    box-shadow: 0 10px 25px -5px rgb(0 0 0 / 0.6), 0 8px 10px -6px rgb(0 0 0 / 0.5);
  }
  .emanote-footnote-popup-body > :last-child { margin-bottom: 0; }
  .emanote-footnote-popup-body p { margin-bottom: 0.5rem; }

  sup.footnote-ref.emanote-footnote-active a {
    background-color: var(--color-primary-100);
    border-radius: 3px;
    padding: 0 0.2em;
  }
  .dark sup.footnote-ref.emanote-footnote-active a {
    background-color: var(--color-primary-900);
  }

  /* Mobile bottom-sheet: slide up from the bottom, full width.
     The 640px breakpoint must stay in sync with MOBILE_MAX in the
     <script> below — the JS side toggles the --mobile class, the CSS
     side owns what that class looks like. */
  @media (max-width: 640px) {
    .emanote-footnote-popup--mobile {
      top: auto !important;
      left: 0 !important;
      right: 0 !important;
      bottom: 0 !important;
      width: 100% !important;
      max-width: 100vw !important;
    }
    .emanote-footnote-popup--mobile .emanote-footnote-popup-body {
      border-radius: 1rem 1rem 0 0;
      padding: 1rem 1.25rem 1.5rem;
      max-height: 60vh;
      overflow-y: auto;
      box-shadow: 0 -12px 30px -5px rgb(0 0 0 / 0.25);
    }
  }
</style>

<script>
  (function () {
    'use strict';

    if (typeof HTMLElement === 'undefined' ||
        !HTMLElement.prototype.hasOwnProperty('popover')) {
      // No Popover API — fall through to native anchor navigation.
      return;
    }

    var POPOVER_ID = 'emanote-footnote-popover';
    // MOBILE_MAX must stay in sync with the @media (max-width: …) breakpoint
    // in the <style> above — neither side enforces it.
    var MOBILE_MAX = 640;

    var popoverEl = null;
    var currentRef = null;

    function ensurePopover() {
      if (popoverEl) return popoverEl;
      popoverEl = document.createElement('div');
      popoverEl.id = POPOVER_ID;
      popoverEl.setAttribute('popover', 'auto');
      var body = document.createElement('div');
      body.className = 'emanote-footnote-popup-body';
      popoverEl.appendChild(body);
      document.body.appendChild(popoverEl);
      popoverEl.addEventListener('toggle', function (e) {
        if (e.newState === 'closed' && currentRef) {
          currentRef.classList.remove('emanote-footnote-active');
          currentRef = null;
        }
      });
      return popoverEl;
    }

    function findTarget(ref) {
      var idx = ref.getAttribute('data-footnote-ref');
      if (!idx) return null;
      var liSel = 'li[data-footnote-id="' + CSS.escape(idx) + '"]';
      var embed = ref.closest('[data-footnote-embed]');
      if (embed) {
        return embed.querySelector('aside[data-footnote-list] ' + liSel);
      }
      // Non-embedded ref: find the first top-level aside (one whose closest
      // data-footnote-embed ancestor is itself null).
      var asides = document.querySelectorAll('aside[data-footnote-list]');
      for (var i = 0; i < asides.length; i++) {
        if (!asides[i].closest('[data-footnote-embed]')) {
          var li = asides[i].querySelector(liSel);
          if (li) return li;
        }
      }
      return null;
    }

    function cloneContent(li) {
      var clone = li.cloneNode(true);
      // Backref would navigate away from the popup — drop it.
      var backrefs = clone.querySelectorAll('a[data-footnote-backref]');
      for (var i = 0; i < backrefs.length; i++) backrefs[i].remove();
      return clone;
    }

    function isMobile() {
      return window.matchMedia('(max-width: ' + MOBILE_MAX + 'px)').matches;
    }

    function positionPopover(popover, ref) {
      if (isMobile()) {
        popover.classList.add('emanote-footnote-popup--mobile');
        popover.style.top = '';
        popover.style.left = '';
        return;
      }
      popover.classList.remove('emanote-footnote-popup--mobile');
      var refRect = ref.getBoundingClientRect();
      var popRect = popover.getBoundingClientRect();
      var vw = window.innerWidth;
      var margin = 12;

      var left = refRect.left + refRect.width / 2 - popRect.width / 2;
      if (left < margin) left = margin;
      if (left + popRect.width > vw - margin) left = vw - popRect.width - margin;

      var spaceAbove = refRect.top;
      var top;
      if (spaceAbove > popRect.height + margin) {
        top = refRect.top - popRect.height - 8;
      } else {
        top = refRect.bottom + 8;
      }
      if (top < margin) top = margin;

      popover.style.top = top + 'px';
      popover.style.left = left + 'px';
    }

    function openFor(ref, target) {
      var popover = ensurePopover();
      var body = popover.firstElementChild;
      body.textContent = '';
      body.appendChild(cloneContent(target));
      if (currentRef && currentRef !== ref) {
        currentRef.classList.remove('emanote-footnote-active');
      }
      currentRef = ref;
      ref.classList.add('emanote-footnote-active');
      try { popover.hidePopover(); } catch (_) {}
      try {
        popover.showPopover();
      } catch (err) {
        // Silent return would mean the click appeared to do nothing.
        console.warn('[emanote] footnote popover showPopover failed', err);
        return;
      }
      // Defer to next frame so popover width reflects the content just
      // inserted — measuring immediately after showPopover() can center
      // on the previous frame's width.
      requestAnimationFrame(function () { positionPopover(popover, ref); });
    }

    function onClick(e) {
      if (e.defaultPrevented) return;
      if (e.button !== 0 || e.ctrlKey || e.metaKey || e.shiftKey || e.altKey) return;
      var sup = e.target.closest('sup[data-footnote-ref]');
      if (!sup) return;
      var target = findTarget(sup);
      if (!target) return;
      e.preventDefault();
      openFor(sup, target);
    }

    function onViewportChange() {
      if (popoverEl && popoverEl.matches(':popover-open') && currentRef) {
        positionPopover(popoverEl, currentRef);
      }
    }

    function init() {
      document.addEventListener('click', onClick);
      window.addEventListener('resize', onViewportChange);
      window.addEventListener('scroll', onViewportChange, { passive: true });
    }

    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', init);
    } else {
      init();
    }
  })();
</script>
