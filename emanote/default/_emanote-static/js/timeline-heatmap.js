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
//
// Cell primitives (palette, sizes, filled/empty builders) live in
// @emanote/calendar-grid; the row/grid composition is widget-specific.

import { ready, onMorph } from '@emanote/morph';
import {
  MONTH_LABELS,
  SIZE_NARROW,
  SIZE_WIDE,
  FLYOUT_OUTER,
  FLYOUT_CARD,
  FLYOUT_HEADER,
  createFilledCell,
  createEmptyCell,
} from '@emanote/calendar-grid';

const DATE_RE = /(\d{4})-(\d{2})-(\d{2})/;

const ROW_CLASSES = 'flex items-center gap-1.5';
const LABEL_CLASSES = 'w-7 text-[0.65rem] uppercase tracking-wider text-gray-500 dark:text-gray-400 select-none shrink-0';
const YEAR_LABEL_CLASSES = 'text-xs font-semibold tracking-tight text-gray-700 dark:text-gray-300 mb-1';

function parseEntries(listEl) {
  const out = new Map(); // year → Map<month, Map<day, {url, title, contextHTML}>>
  for (const li of listEl.querySelectorAll('li')) {
    const title = li.dataset.title || '';
    const url = li.dataset.url || '';
    // The <li>'s inner HTML is the rendered backlink context (paragraphs
    // around where this note is referenced from the daily note). Used
    // verbatim inside the cell's hover flyout — same context shape as
    // backlinks-margin so timeline + backlinks read as one family.
    const contextHTML = li.innerHTML.trim();
    const m = title.match(DATE_RE);
    if (!m) continue;
    const [, y, mo, d] = m;
    const year = +y, month = +mo, day = +d;
    if (!out.has(year)) out.set(year, new Map());
    const yr = out.get(year);
    if (!yr.has(month)) yr.set(month, new Map());
    yr.get(month).set(day, { url, title, contextHTML });
  }
  return out;
}

function buildFlyout(headerText, contextHTML) {
  // CSS-only flyout: hidden by default, shown on group-hover /
  // group-focus-within (the cell carries `group relative`). Built up
  // programmatically rather than via innerHTML so the cloned pandoc
  // context HTML can't escape into outer attributes.
  const flyout = document.createElement('div');
  flyout.className = FLYOUT_OUTER;
  const card = document.createElement('div');
  card.className = FLYOUT_CARD;
  const header = document.createElement('div');
  header.className = FLYOUT_HEADER;
  header.textContent = headerText;
  card.appendChild(header);
  if (contextHTML) {
    const body = document.createElement('div');
    body.innerHTML = contextHTML;
    card.appendChild(body);
  }
  flyout.appendChild(card);
  return flyout;
}

function renderMonthRow(year, month, dayMap, wide) {
  const row = document.createElement('div');
  row.className = ROW_CLASSES;

  const label = document.createElement('span');
  label.className = LABEL_CLASSES;
  label.textContent = MONTH_LABELS[month - 1];
  row.appendChild(label);

  const sizeClass = wide ? SIZE_WIDE : SIZE_NARROW;

  const cells = document.createElement('div');
  // flex-1 on the row's cells container only matters in the wide
  // context — the cells inside use flex-1 to distribute the row's
  // remaining width (after the month label).
  cells.className = wide ? 'flex flex-1 gap-px' : 'flex gap-px';
  // 31 cells regardless of month length — empty trailing cells render as
  // "no day" placeholders for visual alignment across rows. (A 30-day month
  // gets one trailing empty; February gets 2 or 3.)
  for (let day = 1; day <= 31; day++) {
    const entry = dayMap && dayMap.get(day);
    if (entry) {
      const dStr = String(day).padStart(2, '0');
      const moStr = String(month).padStart(2, '0');
      const dateStr = year + '-' + moStr + '-' + dStr;
      const headerText = dateStr + ' — ' + entry.title;
      cells.appendChild(createFilledCell({
        url: entry.url,
        headerText,
        sizeClass,
        flyoutBuilder: (h) => buildFlyout(h, entry.contextHTML),
      }));
    } else {
      cells.appendChild(createEmptyCell(sizeClass));
    }
  }
  row.appendChild(cells);
  return row;
}

function renderYear(year, monthMap, wide) {
  const wrapper = document.createElement('div');
  wrapper.className = 'mb-3';

  const label = document.createElement('div');
  label.className = YEAR_LABEL_CLASSES;
  label.textContent = String(year);
  wrapper.appendChild(label);

  const rows = document.createElement('div');
  rows.className = 'space-y-[2px]';
  for (let month = 1; month <= 12; month++) {
    rows.appendChild(renderMonthRow(year, month, monthMap.get(month), wide));
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
    // wide layout when the bottom strip hosts the timeline; narrow when
    // it's in the right-panel column. The outer aside carries the
    // .emanote-bottom-strip class (chrome only — its inner sections own
    // the panel IDs).
    const wide = !!section.closest('.emanote-bottom-strip');
    const years = [...grouped.keys()].sort((a, b) => b - a);
    for (const year of years) {
      target.appendChild(renderYear(year, grouped.get(year), wide));
    }
  }
}

ready(render);
onMorph(render);
