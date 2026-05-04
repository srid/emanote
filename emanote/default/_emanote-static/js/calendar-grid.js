// Cell-level primitives shared by every calendar/heatmap widget — the
// palette, the size variants, and the filled/empty cell builders. The
// row/grid composition is widget-specific: timeline-heatmap stacks 31-cell
// linear strips, sidebar-calendar lays cells out in a 7-column weekday
// grid. Pulling the cell concept into one module keeps the primary palette
// + sizing in one place; a Tailwind palette refresh edits one file, not N.

export const MONTH_LABELS = [
  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec',
];

export const CELL_BASE = 'block rounded-[1px]';
export const CELL_FILLED_BASE = CELL_BASE + ' group relative bg-primary-500 dark:bg-primary-400 hover:bg-primary-700 dark:hover:bg-primary-300 transition-colors';
export const CELL_EMPTY_BG = ' bg-gray-200 dark:bg-gray-800';

// Two render contexts:
//   - "narrow"  → right-panel column (~150-210px) and sidebar (~200px).
//                 Fixed 4×4px squares (w-1 h-1).
//   - "wide"    → bottom strip at <lg (~700-960px). flex-1 cells with
//                 min-w + h-2.5 stretch as horizontal bars rather than
//                 leaving 70% empty space.
export const SIZE_NARROW = ' w-1 h-1';
export const SIZE_WIDE = ' flex-1 min-w-[6px] h-2.5';

// Hover flyout: outer wrapper with pt-1.5 acts as a transparent
// hover-bridge so cursor traversal between cell and visible card never
// drops `group-hover`. Positioned below the cell (top-full) so flyouts
// near the top don't clip on the viewport edge.
export const FLYOUT_OUTER = 'cell-flyout absolute z-50 hidden group-hover:block group-focus-within:block top-full left-1/2 -translate-x-1/2 pt-1.5';
export const FLYOUT_CARD = 'w-64 max-w-[min(16rem,80vw)] px-3 py-2 border border-gray-200 dark:border-gray-700 rounded-lg bg-white dark:bg-gray-900 shadow-xl text-[0.8rem] leading-[1.5] text-gray-700 dark:text-gray-300 [&_p]:m-0 [&_p+p]:mt-2';
export const FLYOUT_HEADER = 'text-[0.65rem] font-semibold uppercase tracking-wider text-primary-600 dark:text-primary-400 mb-1.5';

// Build a filled cell anchor. `sizeClass` is one of SIZE_NARROW / SIZE_WIDE.
// `flyoutBuilder(headerText)` is optional — if supplied, its return DOM
// node is appended inside the anchor as a CSS-only hover flyout. Widgets
// that don't want a flyout (e.g. sidebar-calendar) just omit it.
export function createFilledCell({ url, headerText, sizeClass, flyoutBuilder }) {
  const a = document.createElement('a');
  a.className = CELL_FILLED_BASE + sizeClass;
  a.href = url;
  // aria-label (not title) so screen readers announce the cell without
  // the browser layering its native gray tooltip on top of any rich
  // hover flyout.
  a.setAttribute('aria-label', headerText);
  if (flyoutBuilder) {
    a.appendChild(flyoutBuilder(headerText));
  }
  return a;
}

export function createEmptyCell(sizeClass) {
  const sp = document.createElement('span');
  sp.className = CELL_BASE + sizeClass + CELL_EMPTY_BG;
  sp.setAttribute('aria-hidden', 'true');
  return sp;
}
