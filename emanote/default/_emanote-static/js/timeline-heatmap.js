// Render daily-note backlinks (`<ema:note:backlinks:daily>`) as a year-stacked
// heatmap into #timeline-heatmap, using the hidden <ul id="timeline-data"> as
// the data source. Dates are parsed out of each backlink's title via a
// YYYY-MM-DD regex; entries without a parseable date are silently skipped
// (the hidden list is also the screen-reader / no-JS fallback so they're
// still reachable).
//
// Per-year layout: 12 month rows × 31 cells. Cells are 4×4px (Tailwind
// `w-1 h-1`) so 31 × 5px-stride ≈ 155px — fits inside the right-panel's
// narrow ~150px content width at lg, ~210px at xl. Filled cells are anchors
// that navigate to the daily note; hover surfaces date + title via the
// native title attribute.
//
// Re-renders on @emanote/morph navigation so live-server in-app nav picks
// up new backlinks without a hard reload.

import { ready, onMorph } from '@emanote/morph';

const DATE_RE = /(\d{4})-(\d{2})-(\d{2})/;
const MONTH_LABELS = [
  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec',
];

const ROW_CLASSES = 'flex items-center gap-1.5';
const LABEL_CLASSES = 'w-7 text-[0.65rem] uppercase tracking-wider text-gray-500 dark:text-gray-400 select-none';
const CELLS_CLASSES = 'flex gap-px';
const CELL_BASE = 'block w-1 h-1 rounded-[1px]';
const CELL_EMPTY = CELL_BASE + ' bg-gray-200 dark:bg-gray-800';
const CELL_FILLED = CELL_BASE + ' bg-primary-500 dark:bg-primary-400 hover:bg-primary-700 dark:hover:bg-primary-300 transition-colors';
const YEAR_LABEL_CLASSES = 'text-xs font-semibold tracking-tight text-gray-700 dark:text-gray-300 mb-1';

function parseEntries(listEl) {
  const out = new Map(); // year → Map<month, Map<day, {url, title}>>
  for (const li of listEl.querySelectorAll('li')) {
    const title = li.dataset.title || '';
    const url = li.dataset.url || '';
    const m = title.match(DATE_RE);
    if (!m) continue;
    const [, y, mo, d] = m;
    const year = +y, month = +mo, day = +d;
    if (!out.has(year)) out.set(year, new Map());
    const yr = out.get(year);
    if (!yr.has(month)) yr.set(month, new Map());
    yr.get(month).set(day, { url, title });
  }
  return out;
}

function renderMonthRow(year, month, dayMap) {
  const row = document.createElement('div');
  row.className = ROW_CLASSES;

  const label = document.createElement('span');
  label.className = LABEL_CLASSES;
  label.textContent = MONTH_LABELS[month - 1];
  row.appendChild(label);

  const cells = document.createElement('div');
  cells.className = CELLS_CLASSES;
  // 31 cells regardless of month length — empty trailing cells render as
  // "no day" placeholders for visual alignment across rows. (A 30-day month
  // gets one trailing empty; February gets 2 or 3.)
  for (let day = 1; day <= 31; day++) {
    const entry = dayMap && dayMap.get(day);
    if (entry) {
      const a = document.createElement('a');
      a.className = CELL_FILLED;
      a.href = entry.url;
      const dStr = String(day).padStart(2, '0');
      const moStr = String(month).padStart(2, '0');
      a.title = year + '-' + moStr + '-' + dStr + ' — ' + entry.title;
      cells.appendChild(a);
    } else {
      const sp = document.createElement('span');
      sp.className = CELL_EMPTY;
      sp.setAttribute('aria-hidden', 'true');
      cells.appendChild(sp);
    }
  }
  row.appendChild(cells);
  return row;
}

function renderYear(year, monthMap) {
  const wrapper = document.createElement('div');
  wrapper.className = 'mb-3';

  const label = document.createElement('div');
  label.className = YEAR_LABEL_CLASSES;
  label.textContent = String(year);
  wrapper.appendChild(label);

  const rows = document.createElement('div');
  rows.className = 'space-y-[2px]';
  for (let month = 1; month <= 12; month++) {
    rows.appendChild(renderMonthRow(year, month, monthMap.get(month)));
  }
  wrapper.appendChild(rows);
  return wrapper;
}

function render() {
  // Two instances render on most pages: one in the right-panel (lg+) and
  // one in the bottom strip (<lg). Each has its own .timeline-data and
  // .timeline-heatmap children, so we paint them independently.
  for (const section of document.querySelectorAll('.emanote-timeline')) {
    const data = section.querySelector('.timeline-data');
    const target = section.querySelector('.timeline-heatmap');
    if (!data || !target) continue;
    const grouped = parseEntries(data);
    target.textContent = '';
    if (grouped.size === 0) {
      // Hide if no parseable entries — bare "Timeline" header is noise.
      section.hidden = true;
      continue;
    }
    section.hidden = false;
    const years = [...grouped.keys()].sort((a, b) => b - a);
    for (const year of years) {
      target.appendChild(renderYear(year, grouped.get(year)));
    }
  }
}

ready(render);
onMorph(render);
