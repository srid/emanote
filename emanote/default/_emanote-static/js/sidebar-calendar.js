// When a sidebar tree node holds nothing but daily-note leaves of one
// month (e.g. `Daily/2026/04/` containing 2026-04-21 … 2026-04-30),
// swap the linear list for a 7-column calendar grid for that month.
// Days with notes show as clickable primary-coloured cells; days
// without are muted day-numbers; both carry the day's date as text.
//
// Detection seam: the sidebar tree wraps each subtree in
// `<div class="emanote-tree-children">` and stamps every leaf anchor
// with `data-iso-date="YYYY-MM-DD"` (empty for non-daily notes).
// Both come from Heist; this module never re-parses dates from text.
// See `routeTreeSplices` in `Emanote.View.Template`.
//
// Cell shape is sidebar-specific (day-numbered tile), distinct from
// the timeline-heatmap's text-less colour-only square. Both share
// `MONTH_LABELS` and `formatCellHeader` from `@emanote/calendar-grid`,
// but the cell builders themselves live here.
//
// Re-runs on @emanote/morph for live-server in-app navigation.

import { ready, onMorph } from '@emanote/morph';
import { MONTH_LABELS, formatCellHeader } from '@emanote/calendar-grid';

const WEEKDAY_LABELS = ['M', 'T', 'W', 'T', 'F', 'S', 'S'];

const CALENDAR_CLASS = 'emanote-sidebar-calendar';
const WRAPPER_CLASSES = CALENDAR_CLASS + ' my-1.5 px-2 py-2 rounded-md bg-gray-50 dark:bg-gray-900/50';
const HEADER_CLASSES = 'text-[0.7rem] font-semibold tracking-tight text-gray-700 dark:text-gray-300 mb-1.5';
const WEEKDAY_ROW_CLASSES = 'grid grid-cols-7 mb-1';
const WEEKDAY_LABEL_CLASSES = 'text-[0.55rem] uppercase tracking-wider text-gray-400 dark:text-gray-500 text-center select-none';
const GRID_CLASSES = 'grid grid-cols-7 gap-0.5 place-items-center';
const CELL_BASE = 'flex items-center justify-center text-[0.65rem] w-6 h-6 rounded-sm tabular-nums transition-colors';
const CELL_FILLED = CELL_BASE + ' font-semibold bg-primary-500 dark:bg-primary-400 text-white hover:bg-primary-700 dark:hover:bg-primary-300';
const CELL_TODAY_MARKER = ' underline decoration-2 underline-offset-2';
const CELL_FILLED_TODAY = CELL_FILLED + ' ring-2 ring-primary-300/80 dark:ring-primary-500/80 ring-offset-1 ring-offset-gray-50 dark:ring-offset-gray-900' + CELL_TODAY_MARKER;
const CELL_ACTIVE = CELL_BASE + ' font-bold bg-primary-700 dark:bg-primary-300 text-white dark:text-gray-950 ring-2 ring-primary-300/80 dark:ring-primary-500/80 ring-offset-1 ring-offset-gray-50 dark:ring-offset-gray-900 shadow-sm';
const CELL_ACTIVE_TODAY = CELL_ACTIVE + CELL_TODAY_MARKER;
const CELL_EMPTY = CELL_BASE + ' text-gray-400 dark:text-gray-600';
const CELL_EMPTY_TODAY = CELL_BASE + ' font-semibold text-primary-700 dark:text-primary-300 bg-white dark:bg-gray-950 border border-primary-400/80 dark:border-primary-500/80' + CELL_TODAY_MARKER;

function dateToIso(year, month, day) {
  return [
    String(year).padStart(4, '0'),
    String(month).padStart(2, '0'),
    String(day).padStart(2, '0'),
  ].join('-');
}

function todayIso() {
  const now = new Date();
  return dateToIso(now.getFullYear(), now.getMonth() + 1, now.getDate());
}

function isCurrentRoute(url) {
  return new URL(url, document.baseURI).pathname === window.location.pathname;
}

function refreshDayCell(cell, today) {
  const isToday = cell.dataset.isoDate === today;
  cell.toggleAttribute('data-today', isToday);

  if (cell.tagName === 'A') {
    const isActive = isCurrentRoute(cell.href);
    cell.className = isActive
      ? (isToday ? CELL_ACTIVE_TODAY : CELL_ACTIVE)
      : (isToday ? CELL_FILLED_TODAY : CELL_FILLED);
    cell.toggleAttribute('data-active-route', isActive);
    if (isActive) {
      cell.setAttribute('aria-current', 'page');
    } else {
      cell.removeAttribute('aria-current');
    }
  } else {
    cell.className = isToday ? CELL_EMPTY_TODAY : CELL_EMPTY;
  }
}

function refreshCalendar(calendar) {
  const today = todayIso();
  for (const cell of calendar.querySelectorAll('[data-iso-date]')) {
    refreshDayCell(cell, today);
  }
}

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
    const a = child.querySelector(':scope a[data-iso-date]');
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

// Build one day cell. `entry` is the leaves-map value for this day, or
// undefined when no daily note exists for it. Filled cells link to the
// note; empty cells are non-interactive but still show the day number
// so the calendar reads as a calendar at a glance.
function buildDayCell(year, month, day, entry, today) {
  const iso = dateToIso(year, month, day);
  if (entry) {
    const a = document.createElement('a');
    a.href = entry.url;
    a.dataset.day = String(day);
    a.dataset.isoDate = iso;
    a.setAttribute('aria-label', formatCellHeader(year, month, day, entry.title));
    a.textContent = String(day);
    refreshDayCell(a, today);
    return a;
  }
  const sp = document.createElement('span');
  sp.dataset.day = String(day);
  sp.dataset.isoDate = iso;
  sp.textContent = String(day);
  refreshDayCell(sp, today);
  return sp;
}

function buildCalendar({ year, month, leaves }) {
  const wrapper = document.createElement('div');
  wrapper.className = WRAPPER_CLASSES;
  const today = todayIso();

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
    blank.setAttribute('aria-hidden', 'true');
    grid.appendChild(blank);
  }

  const lastDay = new Date(Date.UTC(year, month, 0)).getUTCDate();
  for (let day = 1; day <= lastDay; day++) {
    grid.appendChild(buildDayCell(year, month, day, leaves.get(day), today));
  }
  wrapper.appendChild(grid);

  return wrapper;
}

function isAlreadyRendered(wrapper) {
  return wrapper.firstElementChild?.classList.contains(CALENDAR_CLASS) ?? false;
}

function render() {
  for (const wrapper of document.querySelectorAll('.emanote-tree-children')) {
    if (isAlreadyRendered(wrapper)) {
      refreshCalendar(wrapper.firstElementChild);
      continue;
    }
    const group = classifyMonthGroup(wrapper);
    if (!group) continue;
    const calendar = buildCalendar(group);
    wrapper.textContent = '';
    wrapper.appendChild(calendar);
  }
}

ready(render);
onMorph(render);
