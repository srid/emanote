<!-- Footnote popup. Footnote refs have no anchor href and no bottom list
     renders on screen (the hidden <aside data-footnote-list> is the clone
     source for this script, and also the print-mode rendering). Bound to
     data-footnote-ref / data-footnote-id / data-footnote-list /
     data-footnote-scope.

     Popover chrome is styled via Tailwind utility classes applied from
     the script below onto the JS-created <div>; the one CSS escape
     hatch here is the Popover ::backdrop pseudo-element, which has no
     Tailwind variant. -->

<style data-category="footnote-popup">
  #emanote-footnote-popover::backdrop { background: transparent; }
</style>

<script>
  (function () {
    'use strict';

    if (typeof HTMLElement === 'undefined' ||
        !HTMLElement.prototype.hasOwnProperty('popover')) {
      // No Popover API — footnote refs stay inert, no popup.
      return;
    }

    // Keep these in sync with the max-sm: variants below (Tailwind's sm
    // breakpoint is 640px). Parsed as a MediaQueryList once and reused.
    var mobileMQL = window.matchMedia('(max-width: 640px)');

    var POPOVER_CLASS = [
      'fixed z-[9999] m-0 p-0 border-0 bg-transparent',
      'w-max max-w-[min(32rem,calc(100vw-2rem))]',
      'text-[0.95rem] leading-relaxed',
      // Mobile: pin to viewport edge as a full-width bottom sheet.
      'max-sm:top-auto max-sm:left-0 max-sm:right-0 max-sm:bottom-0',
      'max-sm:w-full max-sm:max-w-full',
    ].join(' ');

    var BODY_CLASS = [
      'relative bg-white dark:bg-gray-900',
      'text-gray-800 dark:text-gray-200',
      'px-[1.125rem] py-[0.875rem]',
      'rounded-lg border border-gray-200 dark:border-gray-700',
      'shadow-xl',
      // Cloned pandoc content inherits styling from the surrounding page
      // (paragraphs, stray <li> left from the <ol> the <li> came from).
      '[&>:last-child]:mb-0 [&_p]:mb-2',
      '[&>li]:list-none [&>li]:pl-0 [&>li]:ml-0',
      // Mobile bottom-sheet shape.
      'max-sm:rounded-t-2xl max-sm:rounded-b-none',
      'max-sm:px-5 max-sm:pt-4 max-sm:pb-6',
      'max-sm:max-h-[60vh] max-sm:overflow-y-auto',
      'max-sm:shadow-2xl',
    ].join(' ');

    var popoverEl = null;
    var currentRef = null;
    var topLevelAside = null;

    function ensurePopover() {
      // Emanote's live-server patches the DOM on source changes, which can
      // detach a previously appended popover. Recreate when that happens.
      if (popoverEl && popoverEl.isConnected) return popoverEl;
      popoverEl = document.createElement('div');
      popoverEl.id = 'emanote-footnote-popover';
      popoverEl.setAttribute('popover', 'auto');
      popoverEl.className = POPOVER_CLASS;
      var body = document.createElement('div');
      body.className = BODY_CLASS;
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

    function findOwnAside(scope) {
      // The aside "owned" by a scope is the one whose nearest
      // data-footnote-scope ancestor is the scope itself — not a nested
      // one. When scope is null, the root is the document and owned
      // asides are those with no scope ancestor at all.
      var root = scope || document;
      var asides = root.querySelectorAll('aside[data-footnote-list]');
      for (var i = 0; i < asides.length; i++) {
        if (asides[i].closest('[data-footnote-scope]') === scope) return asides[i];
      }
      return null;
    }

    function findTarget(ref) {
      var idx = ref.getAttribute('data-footnote-ref');
      if (!idx) return null;
      var scope = ref.closest('[data-footnote-scope]');
      var aside;
      if (scope === null) {
        // Re-resolve if the cached aside got swapped out by a live-reload patch.
        if (!topLevelAside || !topLevelAside.isConnected) {
          topLevelAside = findOwnAside(null);
        }
        aside = topLevelAside;
      } else {
        aside = findOwnAside(scope);
      }
      return aside
        ? aside.querySelector('li[data-footnote-id="' + CSS.escape(idx) + '"]')
        : null;
    }

    function positionPopover(popover, ref) {
      if (mobileMQL.matches) {
        // Bottom-sheet layout comes from max-sm: utilities on POPOVER_CLASS;
        // just clear any inline top/left left over from a prior desktop open.
        popover.style.top = '';
        popover.style.left = '';
        return;
      }
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
      body.appendChild(target.cloneNode(true));
      // hidePopover throws InvalidStateError if the popover is already
      // closed. That's the expected case on first open; the throw carries
      // no signal we'd act on, so swallow it.
      try { popover.hidePopover(); } catch (_) {}
      try {
        popover.showPopover();
      } catch (err) {
        // Silent return would mean the click appeared to do nothing.
        console.warn('[emanote] footnote popover showPopover failed', err);
        return;
      }
      // Only dirty the active-state after show succeeds — otherwise a
      // failed show leaves a stale highlight on the ref until next click.
      if (currentRef && currentRef !== ref) {
        currentRef.classList.remove('emanote-footnote-active');
      }
      currentRef = ref;
      ref.classList.add('emanote-footnote-active');
      // Defer to next frame so popover width reflects the content just
      // inserted — measuring immediately after showPopover() can center
      // on the previous frame's width.
      requestAnimationFrame(function () { positionPopover(popover, ref); });
    }

    function activate(e, sup) {
      var target = findTarget(sup);
      if (!target) return;
      e.preventDefault();
      openFor(sup, target);
    }

    function onClick(e) {
      if (e.defaultPrevented) return;
      if (e.button !== 0 || e.ctrlKey || e.metaKey || e.shiftKey || e.altKey) return;
      var sup = e.target.closest('sup[data-footnote-ref]');
      if (sup) activate(e, sup);
    }

    function onKeydown(e) {
      if (e.key !== 'Enter' && e.key !== ' ') return;
      var sup = e.target.closest('sup[data-footnote-ref]');
      if (sup) activate(e, sup);
    }

    // Scroll fires at ≥ 60 Hz; coalesce repositions into the browser's
    // next animation frame so we do one layout read per frame, not per
    // scroll tick.
    var viewportTick = false;
    function onViewportChange() {
      if (viewportTick) return;
      if (!popoverEl || !popoverEl.matches(':popover-open') || !currentRef) return;
      viewportTick = true;
      requestAnimationFrame(function () {
        viewportTick = false;
        if (popoverEl && popoverEl.matches(':popover-open') && currentRef) {
          positionPopover(popoverEl, currentRef);
        }
      });
    }

    function init() {
      document.addEventListener('click', onClick);
      document.addEventListener('keydown', onKeydown);
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
