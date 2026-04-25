// Entry point for site-authored interactive behaviors. Loaded once from
// base.tpl as <script type="module">, which is defer-by-default so it
// doesn't block first paint. Each behavior module attaches itself on
// import; this file just enumerates them.
//
// Out of scope (keeps its own inline script for now):
//   - Stork search controller (templates/components/stork/stork-search-head.tpl)
//   - FOUC theme applier (base.tpl) — must run pre-paint, can't be deferred

import './theme-toggle.js';
import './code-copy.js';
import './toc-spy.js';
import './footnote-popup.js';
