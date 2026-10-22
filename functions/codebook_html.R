# codebook_html(): write a memisc codebook as a searchable, self-contained HTML page
# ---------------------------------------------------------------------------------
# Usage:
#   source("codebook_html.R")
#   codebook_html(my_data)                         # data.set, data.frame, or tibble
#   codebook_html(codebook(my_data), "cb.html")    # or an existing codebook() result
#
# Output is ONE .html file (no internet, R, or Quarto needed to view it):
#   - search box + clickable variable list on the left (grouped by name prefix)
#   - each variable: name, label, question wording, value/label table with N and %,
#     min / max / mean / SD, other annotations
#
# Arguments
#   x          a data.set / data.frame / tibble, or the result of memisc::codebook()
#   file       path of the .html file to write
#   title      page title shown at the top of the sidebar
#   hide_text  TRUE  = hide Min/Max for all character (text) variables, because
#                      memisc prints actual responses there (can identify people)
#              FALSE = show them
#              character vector = hide only these variables
#   group      TRUE  = group consecutive variables that share a prefix
#                      (phq1, phq2 ... -> "phq"; gpast_c01 ... -> "gpast")
#              FALSE = one flat list
#              a function(names) returning a group label per variable
#   max_rows   value tables longer than this start collapsed ("Show all N values")
#   open       open the file in your browser when done
#   ...        passed on to codebook() (e.g. weights =) when x is data
#
# Requires: memisc, jsonlite

codebook_html <- function(x, file = "codebook.html", title = "Codebook",
                          hide_text = TRUE, group = TRUE, max_rows = 30,
                          open = interactive(), ...) {
  stopifnot(requireNamespace("memisc", quietly = TRUE),
            requireNamespace("jsonlite", quietly = TRUE))
  old <- options(useFancyQuotes = FALSE)
  on.exit(options(old), add = TRUE)
  
  cb <- if (methods::is(x, "codebook")) x else memisc::codebook(x, ...)
  entries <- cb@.Data
  vnames  <- names(cb)
  
  # ---- helpers -------------------------------------------------------------
  unq <- function(s) {                       # strip ' " ‘ ’ “ ” around a string
    s <- trimws(s)
    gsub("^['\"‘“](.*)['\"’”]$", "\\1", s)
  }
  fmt_num <- function(v, digits = 1)
    ifelse(is.na(v), "", formatC(as.numeric(v), format = "f", digits = digits))
  parse_rowname <- function(rn) {
    # memisc row names look like "  1   'Not at all'", "-99 M 'Refused'", "NA M", "1 'Asian'"
    re <- "^\\s*([^\\s'‘(]\\S*)?\\s*(M(?=\\s|$))?\\s*(['‘].*['’]|\\(unlab\\.\\w+\\.\\))?\\s*$"
    m  <- regmatches(rn, regexec(re, rn, perl = TRUE))[[1]]
    if (!length(m)) return(list(code = trimws(rn), miss = FALSE, label = ""))
    code <- m[2]; miss <- nzchar(m[3]); lab <- m[4]
    if (identical(code, "M") && startsWith(lab, "(")) { code <- ""; miss <- TRUE }
    list(code = code, miss = miss || identical(code, "NA"),
         label = if (startsWith(lab, "(")) lab else unq(lab))
  }
  
  hide_vars <- if (isTRUE(hide_text)) {
    vnames[vapply(entries, function(e) identical(unname(e@spec["Storage mode:"]), "character"), logical(1))]
  } else if (is.character(hide_text)) hide_text else character(0)
  
  # ---- convert each codebookEntry into a plain list -------------------------
  to_list <- function(e, name) {
    ann  <- e@annotation
    desc <- if ("description" %in% names(ann)) unname(ann["description"]) else ""
    word <- if ("wording" %in% names(ann)) unname(ann["wording"]) else ""
    other <- ann[!names(ann) %in% c("description", "wording")]
    spec <- lapply(seq_along(e@spec), function(i)
      c(sub(":$", "", trimws(names(e@spec)[i])), unname(e@spec[i])))
    if (!length(desc) || is.na(desc)) desc <- ""
    if (!length(word) || is.na(word)) word <- ""
    
    tables <- list()
    tab <- e@stats$tab
    if (length(tab) && length(dim(tab)) == 3) {
      ttl <- attr(tab, "title"); if (is.null(ttl)) ttl <- ""
      dn  <- dimnames(tab)
      for (k in seq_len(dim(tab)[3])) {
        sl <- tab[, , k, drop = TRUE]
        if (is.null(dim(sl))) sl <- matrix(sl, nrow = dim(tab)[1], dimnames = dn[1:2])
        cols <- colnames(sl)
        ncol1_digits <- if (all(is.na(sl[, 1]) | sl[, 1] == round(sl[, 1]))) 0 else 1
        rows <- lapply(seq_len(nrow(sl)), function(i) {
          r <- if (ttl == "Valid and missing values")
            list(code = "", miss = trimws(rownames(sl)[i]) != "Valid", label = trimws(rownames(sl)[i]))
          else parse_rowname(rownames(sl)[i])
          r$vals <- c(fmt_num(sl[i, 1], ncol1_digits), fmt_num(sl[i, -1], 1))
          r
        })
        tables[[length(tables) + 1]] <- list(
          title = ttl, slice = if (!is.null(dn[[3]])) dn[[3]][k] else "",
          cols = cols, rows = rows)
      }
    }
    
    descr <- NULL
    d <- e@stats$descr
    if (is.matrix(d) && length(d)) {
      vals <- if (is.numeric(d)) matrix(formatC(d, format = "f", digits = 3), nrow(d)) else
        matrix(unq(as.character(d)), nrow(d))
      # data.frame columns arrive pre-formatted with many digits; tidy plain numbers
      isnum <- grepl("^\\s*-?[0-9]+\\.[0-9]{4,}\\s*$", vals)
      vals[isnum] <- formatC(as.numeric(vals[isnum]), format = "f", digits = 3)
      vals[is.na(d) | vals == "NA"] <- ""
      if (name %in% hide_vars) vals[] <- ifelse(nzchar(vals), "(hidden)", "")
      descr <- list(cols = if (is.null(colnames(d))) character(0) else colnames(d),
                    rows = lapply(seq_len(nrow(d)), function(i) c(rownames(d)[i], vals[i, ])))
    }
    
    list(name = name, label = desc, wording = word,
         notes = lapply(seq_along(other), function(i) c(names(other)[i], unname(other[i]))),
         spec = spec, tables = tables, descr = descr)
  }
  data <- Map(to_list, entries, vnames)
  names(data) <- NULL
  
  # ---- grouping ------------------------------------------------------------
  grp <- if (is.function(group)) as.character(group(vnames))
  else if (isTRUE(group)) sub("[0-9]+$", "", sub("[_.].*$", "", vnames))
  else rep("", length(vnames))
  grp[is.na(grp) | !nzchar(grp)] <- if (isTRUE(group) || is.function(group)) vnames[is.na(grp) | !nzchar(grp)] else ""
  for (i in seq_along(data)) data[[i]]$group <- if (isFALSE(group)) "" else grp[i]
  
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, null = "null", na = "string")
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  esc  <- function(s) gsub("<", "&lt;", gsub("&", "&amp;", s, fixed = TRUE), fixed = TRUE)
  
  html <- .codebook_html_template
  html <- sub("__TITLE__", esc(title), html, fixed = TRUE)
  html <- sub("__TITLE__", esc(title), html, fixed = TRUE)
  html <- sub("__MAXROWS__", as.integer(max_rows), html, fixed = TRUE)
  html <- sub("__DATA__", json, html, fixed = TRUE)
  writeLines(html, file, useBytes = TRUE)
  message("Wrote ", normalizePath(file), " (", length(data), " variables",
          if (length(hide_vars)) paste0("; Min/Max hidden for ", length(hide_vars), " text variables") else "", ")")
  if (open) utils::browseURL(normalizePath(file))
  invisible(file)
}

.codebook_html_template <- r"---(<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>__TITLE__</title>
<style>
:root{
  --bg:#f7f7f5; --panel:#ffffff; --side:#efeeea; --ink:#1d1d1b; --muted:#6b6a65; --line:#dddbd4;
  --accent:#2f5d8a; --accent-soft:#e3ecf5; --bar:#9db7d1; --miss:#a5512f; --mark:#fbe7a1;
  --mono:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
  --sans:system-ui,-apple-system,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
}
@media (prefers-color-scheme:dark){
  :root:not([data-theme="light"]){
    --bg:#17181a; --panel:#1f2023; --side:#1b1c1e; --ink:#e8e6e1; --muted:#9a988f; --line:#333438;
    --accent:#8fb6dc; --accent-soft:#25313d; --bar:#3f5f7e; --miss:#e0987a; --mark:#5c4d16;
  }
}
*{box-sizing:border-box}
html,body{margin:0;height:100%}
body{background:var(--bg);color:var(--ink);font:15px/1.5 var(--sans)}
.layout{display:flex;height:100vh}
aside{width:340px;flex:none;background:var(--side);border-right:1px solid var(--line);display:flex;flex-direction:column}
.side-head{padding:16px 16px 10px;border-bottom:1px solid var(--line)}
.side-head h1{font-size:17px;margin:0 0 2px}
.side-head .sub{font-size:12.5px;color:var(--muted);margin-bottom:10px}
#q{width:100%;padding:8px 10px;border:1px solid var(--line);border-radius:6px;background:var(--panel);color:var(--ink);font:inherit}
#q:focus{outline:2px solid var(--accent);outline-offset:-1px}
.side-tools{display:flex;justify-content:space-between;align-items:center;margin-top:8px;font-size:12.5px;color:var(--muted)}
.side-tools button{background:none;border:none;color:var(--accent);cursor:pointer;font:inherit;padding:0}
nav{overflow-y:auto;flex:1;padding:6px 0 30px}
nav summary{list-style:none;cursor:pointer;padding:5px 16px;font-size:13px;font-weight:600;display:flex;justify-content:space-between;gap:8px;user-select:none}
nav summary::-webkit-details-marker{display:none}
nav summary::before{content:"\25B8";color:var(--muted);width:12px;flex:none;transition:transform .15s}
nav details[open]>summary::before{transform:rotate(90deg)}
nav summary .gname{flex:1;font-family:var(--mono)}
nav summary .cnt{color:var(--muted);font-weight:400;font-size:12px}
nav a{display:block;padding:3px 16px 3px 34px;color:var(--ink);text-decoration:none;font-size:13px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;border-left:3px solid transparent}
nav > a{padding-left:28px}
nav a .vn{font-family:var(--mono);font-weight:600}
nav a .vl{color:var(--muted);margin-left:6px}
nav a:hover{background:var(--accent-soft)}
nav a.active{background:var(--accent-soft);border-left-color:var(--accent)}
nav .none{padding:12px 16px;color:var(--muted);font-size:13px}
main{flex:1;overflow-y:auto;padding:24px 32px 80vh}
.card{background:var(--panel);border:1px solid var(--line);border-radius:8px;padding:16px 20px;margin:0 auto 16px;max-width:900px;scroll-margin-top:16px}
.card.hit{box-shadow:0 0 0 2px var(--accent)}
.card h2{margin:0;font:600 17px var(--mono);display:flex;align-items:center;gap:10px;flex-wrap:wrap}
.card h2 .copy{font:12px var(--sans);color:var(--muted);background:none;border:1px solid var(--line);border-radius:4px;padding:1px 7px;cursor:pointer}
.card h2 .copy:hover{color:var(--accent);border-color:var(--accent)}
.label{margin:6px 0 0;white-space:pre-wrap}
.label.long{max-height:7.5em;overflow:hidden;position:relative}
.label.long::after{content:"";position:absolute;left:0;right:0;bottom:0;height:2.5em;background:linear-gradient(transparent,var(--panel))}
.wording{margin:6px 0 0;color:var(--muted);font-style:italic;white-space:pre-wrap}
.more{background:none;border:none;color:var(--accent);cursor:pointer;padding:2px 0;font:13px var(--sans)}
.meta{display:flex;gap:6px;flex-wrap:wrap;margin:10px 0 0}
.chip{font-size:12px;background:var(--side);border:1px solid var(--line);border-radius:999px;padding:1px 9px;color:var(--muted)}
.slice{margin:14px 0 0;font-size:12px;font-weight:600;color:var(--muted);text-transform:uppercase;letter-spacing:.03em}
table{border-collapse:collapse;margin-top:12px;font-size:13.5px;width:100%}
th,td{padding:3px 10px;text-align:left;border-bottom:1px solid var(--line)}
th{font-weight:600;color:var(--muted);font-size:12px;text-transform:uppercase;letter-spacing:.03em}
td.num,th.num{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}
td.code{font-family:var(--mono);white-space:nowrap}
td.barc{width:120px}
.bar{height:8px;background:var(--bar);border-radius:2px}
tr.missing td{color:var(--miss)}
tr.zero td{color:var(--muted)}
.stats{display:grid;grid-template-columns:repeat(auto-fill,minmax(150px,1fr));gap:6px 18px;margin-top:12px;font-size:13.5px}
.stats div span{color:var(--muted);margin-right:6px}
.stats div b{font-weight:600;font-variant-numeric:tabular-nums}
.vm{font-size:13.5px;margin-top:10px;color:var(--muted)}
.vm b{color:var(--ink);font-weight:600}
.notes{margin-top:12px;font-size:13.5px}
.notes div span{color:var(--muted);margin-right:6px}
mark{background:var(--mark);color:inherit;border-radius:2px}
.menu-btn{display:none}
.toast{position:fixed;bottom:18px;left:50%;transform:translateX(-50%);background:var(--ink);color:var(--bg);padding:6px 14px;border-radius:6px;font-size:13px;opacity:0;transition:opacity .2s;pointer-events:none}
.toast.show{opacity:1}
@media (max-width:800px){
  .layout{display:block;height:auto}
  aside{position:fixed;inset:0 auto 0 0;width:min(86vw,340px);z-index:20;transform:translateX(-100%);transition:transform .2s;box-shadow:0 0 30px rgba(0,0,0,.25)}
  body.open aside{transform:none}
  main{padding:60px 16px 60vh;height:auto;overflow:visible}
  .menu-btn{display:block;position:fixed;top:10px;left:16px;z-index:10;background:var(--panel);color:var(--ink);border:1px solid var(--line);border-radius:6px;padding:6px 12px;font:600 14px var(--sans)}
  td.barc{display:none}
  .card{padding:14px}
}
@media print{aside,.menu-btn,.copy,.more{display:none} main{overflow:visible;padding:0} .layout{display:block;height:auto} .label.long{max-height:none}.label.long::after{display:none} tr[hidden]{display:table-row}}
</style>
</head>
<body>
<button class="menu-btn" id="menuBtn">&#9776; Variables</button>
<div class="layout">
  <aside>
    <div class="side-head">
      <h1>__TITLE__</h1>
      <div class="sub">Click a variable to jump to it &middot; press / to search</div>
      <input id="q" type="search" placeholder="Search variable name or question text&hellip;" autocomplete="off">
      <div class="side-tools"><span id="count"></span><span><button id="expand">Expand all</button> &middot; <button id="collapse">Collapse all</button></span></div>
    </div>
    <nav id="nav"></nav>
  </aside>
  <main id="main"></main>
</div>
<div class="toast" id="toast">Copied</div>
<script>
const DATA = __DATA__;
const MAX_ROWS = __MAXROWS__;
const $ = s => document.querySelector(s);
const esc = s => String(s == null ? '' : s).replace(/[&<>"]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[c]));
const firstLine = s => (s || '').split('\n').map(x => x.trim()).filter(Boolean).join(' ');
const arr = a => a == null ? [] : (Array.isArray(a) ? a : [a]);

const groups = [];
DATA.forEach(v => {
  v.id = 'v-' + v.name;
  const g = groups[groups.length - 1];
  if (g && v.group && g.prefix === v.group) g.items.push(v); else groups.push({prefix: v.group, items: [v]});
});

const colName = c => ({Valid: 'Valid %', Total: 'Total %', Percent: '%'}[c] || c);
function renderTable(t) {
  const cols = arr(t.cols), rows = arr(t.rows);
  if (t.title === 'Valid and missing values') {
    return `<div class="vm">${rows.map(r => { const vals = arr(r.vals); return `${esc(r.label)}: <b>${esc(vals[0])}</b>${vals[1] ? ` (${esc(vals[1])}%)` : ''}`; }).join(' &middot; ')}</div>`;
  }
  const iValid = cols.indexOf('Valid'), iTotal = cols.indexOf('Total'), iPct = cols.indexOf('Percent');
  const many = rows.length > MAX_ROWS;
  let h = t.slice ? `<div class="slice">${esc(t.slice)}</div>` : '';
  h += `<table><thead><tr><th>Value</th><th>Label</th>${cols.map(c => `<th class="num">${esc(colName(c))}</th>`).join('')}<th class="barc"></th></tr></thead><tbody>`;
  rows.forEach((r, i) => {
    const vals = arr(r.vals);
    let p = iValid >= 0 && vals[iValid] !== '' ? vals[iValid] : iTotal >= 0 ? vals[iTotal] : iPct >= 0 ? vals[iPct] : 0;
    p = parseFloat(p) || 0;
    const extra = many && i >= MAX_ROWS;
    const cls = [r.miss ? 'missing' : '', parseFloat(vals[0]) === 0 ? 'zero' : '', extra ? 'extra' : ''].join(' ').trim();
    const lab = r.label || (r.code === 'NA' ? 'Missing (NA)' : '');
    h += `<tr class="${cls}"${extra ? ' hidden' : ''}><td class="code">${esc(r.code)}${r.miss && r.code !== 'NA' ? ' <span title="declared missing">M</span>' : ''}</td><td>${esc(lab)}</td>${vals.map(x => `<td class="num">${esc(x)}</td>`).join('')}<td class="barc"><div class="bar" style="width:${Math.min(100, p)}%"></div></td></tr>`;
  });
  h += '</tbody></table>';
  if (many) h += `<button class="more" data-act="rows" data-total="${rows.length}">Show all ${rows.length} values</button>`;
  return h;
}
function renderCard(v) {
  const c = document.createElement('section');
  c.className = 'card'; c.id = v.id;
  let h = `<h2>${esc(v.name)} <button class="copy" data-n="${esc(v.name)}" title="Copy variable name">copy</button></h2>`;
  if (v.label) {
    const long = v.label.length > 400 || v.label.split('\n').length > 6;
    h += `<p class="label${long ? ' long' : ''}">${esc(v.label)}</p>`;
    if (long) h += `<button class="more" data-act="label">Show full text</button>`;
  } else h += `<p class="label" style="color:var(--muted)">(no label)</p>`;
  if (v.wording) h += `<p class="wording">${esc(v.wording)}</p>`;
  const chips = arr(v.spec).filter(s => !(s[0] === 'Measurement' && s[1] === 'undefined')).map(s => s[0].startsWith('Factor') || s[0].startsWith('Ordered') ? `${s[0]} ${s[1]}` : `${s[0]}: ${s[1]}`);
  if (chips.length) h += `<div class="meta">${chips.map(x => `<span class="chip">${esc(x)}</span>`).join('')}</div>`;
  arr(v.tables).forEach(t => h += renderTable(t));
  if (v.descr) {
    const dc = arr(v.descr.cols);
    if (dc.length > 1) {
      h += `<table><thead><tr><th></th>${dc.map(c => `<th class="num">${esc(c)}</th>`).join('')}</tr></thead><tbody>${arr(v.descr.rows).map(r => `<tr><td>${esc(r[0])}</td>${r.slice(1).map(x => `<td class="num">${esc(x)}</td>`).join('')}</tr>`).join('')}</tbody></table>`;
    } else {
      h += `<div class="stats">${arr(v.descr.rows).map(r => `<div><span>${esc(r[0])}</span><b>${esc(r[1])}</b></div>`).join('')}</div>`;
    }
  }
  if (arr(v.notes).length) h += `<div class="notes">${arr(v.notes).map(n => `<div><span>${esc(n[0])}:</span>${esc(n[1])}</div>`).join('')}</div>`;
  c.innerHTML = h;
  return c;
}
const main = $('#main');
const frag = document.createDocumentFragment();
DATA.forEach(v => frag.appendChild(renderCard(v)));
main.appendChild(frag);

main.addEventListener('click', e => {
  const b = e.target.closest('button'); if (!b) return;
  if (b.classList.contains('copy')) {
    const t = b.dataset.n;
    (navigator.clipboard ? navigator.clipboard.writeText(t) : Promise.reject()).catch(() => {
      const ta = document.createElement('textarea'); ta.value = t; document.body.appendChild(ta); ta.select();
      try { document.execCommand('copy'); } catch (_) {} ta.remove();
    });
    const tt = $('#toast'); tt.textContent = 'Copied ' + t; tt.classList.add('show'); setTimeout(() => tt.classList.remove('show'), 1200);
  } else if (b.dataset.act === 'label') {
    const p = b.previousElementSibling; const shut = p.classList.toggle('long');
    b.textContent = shut ? 'Show full text' : 'Show less';
  } else if (b.dataset.act === 'rows') {
    const rows = b.previousElementSibling.querySelectorAll('tr.extra');
    const show = rows[0].hidden; rows.forEach(r => r.hidden = !show);
    b.textContent = show ? 'Show fewer values' : 'Show all ' + b.dataset.total + ' values';
  }
});

const nav = $('#nav');
let query = '';
function hl(text, q) {
  const t = esc(text); if (!q) return t;
  const i = text.toLowerCase().indexOf(q); if (i < 0) return t;
  return esc(text.slice(0, i)) + '<mark>' + esc(text.slice(i, i + q.length)) + '</mark>' + esc(text.slice(i + q.length));
}
function link(v, q) {
  const lab = firstLine(v.label);
  return `<a href="#${esc(v.id)}" data-id="${esc(v.id)}" title="${esc(v.name + (lab ? ' - ' + lab : ''))}"><span class="vn">${hl(v.name, q)}</span><span class="vl">${hl(lab.slice(0, 120), q)}</span></a>`;
}
function renderNav() {
  const q = query.trim().toLowerCase();
  let shown = 0, html = '';
  const match = v => !q || v.name.toLowerCase().includes(q) || (v.label || '').toLowerCase().includes(q) || (v.wording || '').toLowerCase().includes(q);
  groups.forEach(g => {
    const items = g.items.filter(match); if (!items.length) return;
    shown += items.length;
    if (g.items.length === 1) { html += link(items[0], q); return; }
    html += `<details${q ? ' open' : ''}><summary><span class="gname">${esc(g.prefix)}</span><span class="cnt">${q ? items.length + '/' : ''}${g.items.length}</span></summary>${items.map(v => link(v, q)).join('')}</details>`;
  });
  nav.innerHTML = html || '<div class="none">No variables match.</div>';
  $('#count').textContent = q ? `${shown} of ${DATA.length} match` : `${DATA.length} variables`;
  markActive(currentId);
}
let t;
$('#q').addEventListener('input', e => { clearTimeout(t); t = setTimeout(() => { query = e.target.value; renderNav(); }, 120); });
$('#q').addEventListener('keydown', e => { if (e.key === 'Enter') { const a = nav.querySelector('a'); if (a) a.click(); } });
$('#expand').onclick = () => nav.querySelectorAll('details').forEach(d => d.open = true);
$('#collapse').onclick = () => nav.querySelectorAll('details').forEach(d => d.open = false);
nav.addEventListener('click', e => {
  const a = e.target.closest('a'); if (!a) return;
  e.preventDefault(); go(a.dataset.id, true); document.body.classList.remove('open');
});
function go(id, push) {
  const el = document.getElementById(id); if (!el) return;
  el.scrollIntoView({block: 'start'});
  document.querySelectorAll('.card.hit').forEach(c => c.classList.remove('hit'));
  el.classList.add('hit'); setTimeout(() => el.classList.remove('hit'), 1400);
  if (push) try { history.replaceState(null, '', '#' + id); } catch (_) {}
  markActive(id);
}
let currentId = null;
function markActive(id, scrollNav) {
  currentId = id;
  nav.querySelectorAll('a.active').forEach(a => a.classList.remove('active'));
  if (!id) return;
  const a = nav.querySelector(`a[data-id="${CSS.escape(id)}"]`);
  if (a) {
    a.classList.add('active');
    if (scrollNav) { const r = a.getBoundingClientRect(), nr = nav.getBoundingClientRect(); if (r.top < nr.top || r.bottom > nr.bottom) a.scrollIntoView({block: 'nearest'}); }
  }
}
const io = new IntersectionObserver(ents => {
  const vis = ents.filter(e => e.isIntersecting).sort((a, b) => a.boundingClientRect.top - b.boundingClientRect.top);
  if (vis.length) markActive(vis[0].target.id, true);
}, {root: window.matchMedia('(max-width:800px)').matches ? null : main, rootMargin: '0px 0px -75% 0px'});
document.querySelectorAll('.card').forEach(c => io.observe(c));
$('#menuBtn').onclick = () => document.body.classList.toggle('open');
document.addEventListener('keydown', e => {
  if (e.key === '/' && document.activeElement !== $('#q')) { e.preventDefault(); $('#q').focus(); }
  if (e.key === 'Escape') document.body.classList.remove('open');
});
renderNav();
if (location.hash) setTimeout(() => go(decodeURIComponent(location.hash.slice(1))), 50);
</script>
</body>
</html>
)---"