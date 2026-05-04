// When a sidebar tree node holds nothing but daily-note leaves of one
// month (e.g. `Daily/2026/04/` containing 2026-04-21 … 2026-04-30),
// swap the linear list for a 7-column calendar grid for that month.
// Cells with notes are clickable; missing days are muted dots.
//
// Detection seam: the sidebar tree wraps each subtree in
// `<div class="emanote-tree-children">` and stamps every leaf anchor
// with `data-iso-date="YYYY-MM-DD"` (empty for non-daily notes).
// Both come from Heist; this module never re-parses dates from text.
// See `routeTreeSplices` in `Emanote.View.Template`.
//
// Re-runs on @emanote/morph for live-server in-app navigation.

import { ready, onMorph } from '@emanote/morph';
import {
  MONTH_LABELS,
  SIZE_NARROW,
  createFilledCell,
  createEmptyCell,
} from '@emanote/calendar-grid';

const WEEKDAY_LABELS = ['M', 'T', 'W', 'T', 'F', 'S', 'S'];

// Marker class on the rendered calendar element. The DOM is the
// single source of truth for "this wrapper already holds a calendar"
// — see `isAlreadyRendered` below — so any external mutation
// (devtools, a future utility, idiomorph dropping the subtree) leaves
// the gate in sync with what's actually on screen.
const CALENDAR_CLASS = 'emanote-sidebar-calendar';
const WRAPPER_CLASSES = CALENDAR_CLASS + ' my-1.5 px-2 py-2 rounded-md bg-gray-50 dark:bg-gray-900/50';
const HEADER_CLASSES = 'text-[0.7rem] font-semibold tracking-tight text-gray-700 dark:text-gray-300 mb-1.5';
const WEEKDAY_ROW_CLASSES = 'grid grid-cols-7 gap-1 mb-1';
const WEEKDAY_LABEL_CLASSES = 'text-[0.55rem] uppercase tracking-wider text-gray-400 dark:text-gray-500 text-center select-none';
const GRID_CLASSES = 'grid grid-cols-7 gap-1';
const CELL_WRAPPER_CLASSES = 'flex items-center justify-center h-4';

// Returns { year, month, leaves: Map<day, {url, title}> } when every
// child of `wrapper` is a leaf with a parseable iso-date in the same
// year+month. Returns null if the wrapper holds anything else
// (subfolder, non-daily note, mixed months).
function classifyMonthGroup(wrapper) {
  const childNodes = wrapper.querySelectorAll(':scope > div');
  if (childNodes.length === 0) return null;
  let year = null;
  let month = null;
  const leaves = new Map();
  for (const child of childNodes) {
    // A leaf has no actual nested subtree. The wrapper div may still
    // be present (empty) when `tree:open` is true on the leaf — we
    // care only whether the wrapper has content.
    if (child.querySelector(':scope > .emanote-tree-children > *')) return null;
    const a = child.querySelector(':scope > .flex a[data-iso-date]');
    if (!a) return null;
    const iso = a.dataset.isoDate;
    if (!iso) return null;
    const [y, mo, d] = iso.split('-').map(Number);
    if (!Number.isFinite(y) || !Number.isFinite(mo) || !Number.isFinite(d)) return null;
    if (year === null) {
      year = y;
      month = mo;
    } else if (y !== year || mo !== month) {
      return null;
    }
    leaves.set(d, { url: a.href, title: a.getAttribute('title') || iso });
  }
  return { year, month, leaves };
}

function buildCalendar({ year, month, leaves }) {
  const wrapper = document.createElement('div');
  wrapper.className = WRAPPER_CLASSES;

  const header = document.createElement('div');
  header.className = HEADER_CLASSES;
  header.textContent = MONTH_LABELS[month - 1] + ' ' + year;
  wrapper.appendChild(header);

  const weekdayRow = document.createElement('div');
  weekdayRow.className = WEEKDAY_ROW_CLASSES;
  for (const wl of WEEKDAY_LABELS) {
    const span = document.createElement('span');
    span.className = WEEKDAY_LABEL_CLASSES;
    span.textContent = wl;
    weekdayRow.appendChild(span);
  }
  wrapper.appendChild(weekdayRow);

  const grid = document.createElement('div');
  grid.className = GRID_CLASSES;

  // ISO weekday: Mon=1 … Sun=7. Pad leading blanks so day 1 lands in
  // the right weekday column.
  const firstWeekday = new Date(Date.UTC(year, month - 1, 1)).getUTCDay() || 7;
  for (let i = 1; i < firstWeekday; i++) {
    const blank = document.createElement('span');
    blank.className = CELL_WRAPPER_CLASSES;
    blank.setAttribute('aria-hidden', 'true');
    grid.appendChild(blank);
  }

  const lastDay = new Date(Date.UTC(year, month, 0)).getUTCDate();
  for (let day = 1; day <= lastDay; day++) {
    const cellWrap = document.createElement('span');
    cellWrap.className = CELL_WRAPPER_CLASSES;
    const entry = leaves.get(day);
    if (entry) {
      const dStr = String(day).padStart(2, '0');
      const moStr = String(month).padStart(2, '0');
      const dateStr = year + '-' + moStr + '-' + dStr;
      const cell = createFilledCell({
        url: entry.url,
        headerText: dateStr + ' — ' + entry.title,
        sizeClass: SIZE_NARROW,
      });
      cellWrap.appendChild(cell);
    } else {
      cellWrap.appendChild(createEmptyCell(SIZE_NARROW));
    }
    grid.appendChild(cellWrap);
  }
  wrapper.appendChild(grid);

  return wrapper;
}

function isAlreadyRendered(wrapper) {
  return wrapper.firstElementChild?.classList.contains(CALENDAR_CLASS) ?? false;
}

function render() {
  for (const wrapper of document.querySelectorAll('.emanote-tree-children')) {
    if (isAlreadyRendered(wrapper)) continue;
    const group = classifyMonthGroup(wrapper);
    if (!group) continue;
    const calendar = buildCalendar(group);
    wrapper.textContent = '';
    wrapper.appendChild(calendar);
  }
}

ready(render);
onMorph(render);
