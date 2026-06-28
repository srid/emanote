---
slug: task-dashboard
order: 50
---

# Task dashboard (Obsidian Tasks dates) — plan

Tracking issue: [#751](https://github.com/srid/emanote/issues/751). This note is
the plan of record for turning `/-/tasks` into a date-aware, filterable
dashboard, delivered as four small PRs. **Status: Phase 1 proposed** — the rest
is sketched for context so the seams chosen in Phase 1 don't paint later phases
into a corner.

Emanote stays **read-only**: tasks are authored inline (`- [ ] …`) and an editor
(or external tooling) mutates the Markdown. Emanote only renders them more
usefully. The motivating use case is a calm, pull-not-push daily dashboard you
*open*, never a nag.

## Phases at a glance

| Phase | PR | You get |
|---|---|---|
| **1 — Show task dates** | this one | Each task on `/-/tasks` shows its due/scheduled date as a badge; **overdue is red, due-today is highlighted**, computed live from the browser clock so the static site never goes stale. |
| 2 — Filter & sort | next | Filter by tag (hierarchical) and due window, sort by due, toggle group-by-note ⇄ flat agenda, a small "12 open · 3 due" count. |
| 3 — Priority & recurrence | later | Priority badges that sort; recurring tasks shown with their rule and a computed next-due (display only). |
| 4 — Embedded task queries | later | Drop a task query into any note via the existing query mechanism. |

## Phase 1 — what changes

Today `/-/tasks` lists unchecked tasks grouped by note, and any trailing
`📅 2026-07-03` renders as literal text. After Phase 1, the recognised date
tokens are lifted out of the task text and re-emitted as badges:

```markdown
- [ ] Draft the release notes 📅 2026-07-03 #active
```

renders the description as "Draft the release notes #active" followed by a
`📅 2026-07-03` badge that the browser colours red if overdue and highlights if
due today. The Obsidian Tasks date emoji recognised in Phase 1:

| Emoji | Meaning | Field |
|---|---|---|
| `📅` | due | `_taskDueDate` |
| `⏳` | scheduled | `_taskScheduled` |
| `🛫` | start | `_taskStart` |
| `➕` | created | `_taskCreated` |
| `✅` | done | `_taskDone` |
| `❌` | cancelled | `_taskCancelled` |

Overdue/today highlighting is driven by the **due** date (and scheduled when
there's no due), since that's the "needs me" signal. The other dates are parsed
and badged but not coloured. Priority (`🔺⏫🔼🔽⏬`), recurrence (`🔁`), and
`#tag` filtering are explicitly **out of scope** here — they're Phases 2–3.

See the **[look-and-feel mockup](https://htmlpreview.github.io/?https://raw.githubusercontent.com/srid/emanote/phase1-task-dates/docs/dev/task-dashboard.mockup.html)**
(htmlpreview) for the three badge states in light and dark mode.

## The shape (one seam per layer)

The change rides the same three-layer split the sidebar calendar already uses —
parse once on the Haskell side, stamp `data-*`, let a tiny vanilla module add
the time-relative styling from the browser clock. Each layer owns one concern:

1. **Data — `Model/Task.hs`.** `Task` gains the six optional `Day` fields above.
   `noteTasks` already turns each task's inlines into `_taskDescription`; Phase 1
   inserts a pure pass that walks those inlines, recognises `<date-emoji> <Space>
   <YYYY-MM-DD>` runs, moves the date into the matching field, and drops the
   consumed tokens (plus the now-orphaned surrounding space) from the
   description. **No change to the `heist-extra` parser** — this is Emanote's own
   task layer reading a `[Inline]` it already holds.

2. **View — `View/TaskIndex.hs` + `templates/special/tasks.tpl`.** The task
   splice gains `data-due="${task:due-iso}"` (etc.) on the task `<li>` and a
   neutral-styled badge element carrying the date, reusing the existing
   `dayIsoText :: Day -> Text` helper. With JS off this is the whole feature: a
   plain, readable date badge.

3. **Behaviour — new `default/_emanote-static/js/loops.js`.** Modelled directly
   on `sidebar-calendar.js`: `import { ready, onMorph } from '@emanote/morph'`,
   read `data-due`/`data-scheduled`, compute today's ISO from `new Date()`, and
   swap the badge's Tailwind class string to the overdue/today/upcoming variant —
   the same class-string-swap pattern that module uses. Registered by adding
   `"loops"` to `emanoteJsModuleNames` in `View/JsBundle.hs` (one line; `main.js`
   discovers it from the importmap automatically).

### Data flow

```
note Markdown                 Model/Task.hs            tasks.tpl              loops.js (browser)
- [ ] … 📅 2026-07-03   ──►   _taskDueDate = …   ──►   data-due="2026-07-03"  ──►  compare to new Date()
                              _taskDescription         + neutral badge             → red / amber / neutral
                              (date tokens stripped)
```

## How this honours Emanote's design philosophy

- **One source of truth for the date.** The date string is parsed exactly once,
  in the Haskell task layer. `loops.js` reads the machine-readable `data-*`
  attribute and *never re-parses the visible badge text* — the same contract
  `sidebar-calendar.js` keeps with `data-iso-date`. "Now" likewise has one
  source: the browser clock, computed client-side so a statically-built page is
  correct every day without a rebuild.
- **No fallbacks masking failure.** A token is treated as a date only when the
  emoji is immediately followed by a `Str` that parses as `YYYY-MM-DD`. A typo
  like `📅 nextweek` is **not** a silent default-to-something — it simply isn't a
  date token and stays as literal text, exactly as it renders today. Nothing is
  swallowed; the unrecognised case is the *current* behaviour, unchanged.
- **Graceful degradation, not a degraded path.** JS-off still shows every date
  as a neutral badge — that's the server-rendered baseline, with the live
  red/amber colouring as pure enhancement on top, not a second-class fallback.
- **Reuse, don't reinvent.** Badge stamping reuses `dayIsoText`; module wiring
  reuses the importmap manifest; the client module reuses the morph
  `ready`/`onMorph` lifecycle. No new mechanism is introduced where one exists.

## i18n

Per `.claude/rules/emanote.md`, any visible chrome string is i18n work. The badge
itself is a date (locale-neutral digits) plus an emoji, so it needs no
translation. The only candidates are accessibility labels (e.g. a `title`/
`aria-label` like "Due 2026-07-03 — overdue"). Phase 1 keeps these to a minimum
and routes any such word ("Due", "Scheduled", "overdue", "today") through the
existing `template.i18n` keys for **en / fr / zh** and the `i18n.js` `text`/
`message` helpers — no hard-coded visible text.

## Test strategy (feature work)

- **Unit (Haskell).** Pure tests over the new inline-stripping pass in
  `Model/Task.hs`: a task with each emoji parses into the right field and strips
  the tokens; a malformed date is left untouched; a task with no dates is
  unchanged; mixed dates + `#tag` keep the tag in the description.
- **e2e (cucumber + Playwright).** A fixture notebook with dated tasks, a new
  feature file asserting the badges and their `data-*` attributes render on
  `/-/tasks`, and — modelled on `sidebar_calendar_steps.ts` — a mocked
  `Date` to assert the overdue/today classes apply for a pinned "today".
- **Docs as regression sample.** The new syntax is demonstrated in
  [[markdown]] so the live example doubles as a reference and a check.

## Non-goals (whole feature)

- **Reminders / push.** A view you open, not a notification.
- **Editing or completing tasks.** Emanote renders; an editor mutates the file.
