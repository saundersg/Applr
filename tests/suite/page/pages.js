// The wording page and the situation list (layouts from
// for_devs/prototypes/PROTOTYPE_review_ui, version A of each). Drawn by
// app.js's render(); they share its DATA, ui, esc() and arr().
"use strict";

// ---- wording ----
const PH = /\{[^{}]*(?:\{[^{}]*\}[^{}]*)*\}/g;
const ROLES = { column: "Column name", value: "Value", fn: "Function", model: "Model name", response: "Response", cls: "Class" };
const SCENARIOS = {
  tests: { name: "From the tests", fill: null },
  mtcars: { name: "mtcars", fill: { column: "hp", value: "146.7", fn: "geom_slice", model: "model", response: "mpg", cls: "Date" } },
  long: { name: "Long names", fill: { column: "average_household_income_2019", value: "58213.4567", fn: "geom_slice_subtitle",
    model: "fit_household_income_full", response: "log(median_rent_usd)", cls: "POSIXct" } },
  factor: { name: "A factor", fill: { column: "species", value: '"Adelie"', fn: "geom_slice_text", model: "penguin_fit",
    response: "bill_length_mm", cls: "character" } },
  custom: { name: "Custom", fill: null }
};
const store = {
  get(k, d) { try { const v = sessionStorage.getItem(k); return v ? JSON.parse(v) : d; } catch { return d; } },
  set(k, v) { try { sessionStorage.setItem(k, JSON.stringify(v)); } catch {} }
};
const W = { scen: "tests", custom: store.get("applr.custom", {}), open: {}, code: {}, kind: "all", width: null, rows: null, at: null };

const roleOf = e => DATA.wording?.roles?.[e] || null;

// Every call site, with the values its variables took in a test, when one
// of the condition forms holds it.
function wordingRows() {
  if (W.at === DATA) return W.rows;
  const forms = Object.entries(DATA.wording?.examples || {});
  const example = r => {
    const parts = (r.what || "").split(PH);
    if (parts.join("").length < 8) return null;
    const lit = parts.map(p => p.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"));
    const re = new RegExp(lit.slice(0, -1).map((p, i) => p + (i === lit.length - 2 && !parts.at(-1) ? "([^\\n]+)" : "([^\\n]+?)")).join("") + lit.at(-1));
    for (const [id, text] of forms) { const m = text.match(re); if (m) return { id, values: m.slice(1) }; }
    return null;
  };
  W.rows = arr(DATA.wording?.sites).map(r => ({ ...r, loc: `${r.file}:${r.line}`, example: example(r) }));
  W.at = DATA;
  return W.rows;
}

function fillIn(r, text, offset) {
  let n = offset;
  const s = W.scen === "custom" ? Object.fromEntries(Object.entries(W.custom).filter(([, v]) => v)) : SCENARIOS[W.scen].fill;
  return esc(text || "").replace(PH, ph => {
    const e = ph.slice(1, -1).replace(/&quot;/g, '"').replace(/&#39;/g, "'").replace(/&amp;/g, "&").replace(/&lt;/g, "<").replace(/&gt;/g, ">");
    const i = n++;
    let v = null;
    if (!s && r.example && i < r.example.values.length) v = r.example.values[i];
    if (s && roleOf(e)) v = s[roleOf(e)] ?? null;
    return v != null ? `<span class="var" title="${esc(e)}">${esc(v)}</span>` : `<span class="var raw" title="not filled: ${esc(e)}">${esc(e)}</span>`;
  });
}

function message(r) {
  const n = ((r.what || "").match(PH) || []).length;
  return `<div class="msg">${fillIn(r, r.what, 0)}${r.hint ? `\n    <span class="hint">${fillIn(r, r.hint, n)}</span>` : ""}</div>`;
}

// Rows that read the same apart from names fold together.
const shape = r => (r.what || "").replace(PH, "{}").replace(/`[^`]*`/g, "{}").replace(/"[^"]*"/g, "{}").replace(/[\w.]+\(\)/g, "{}()");
const kindPill = r => `<span class="kind k-${r.kind}">${r.kind}</span>`;
const markPill = r => r.status === "new" ? `<span class="pill mark-new">New</span>`
  : r.status === "changed" ? `<span class="pill mark-changed">Changed</span>` : "";
const nextMark = m => !m ? "ok" : m === "ok" ? "bad" : "";
const tick = (m, keys) => `<span class="check ${m === "ok" ? "on" : m === "bad" ? "bad" : ""}" data-wmark="${esc(keys.join(" "))}" data-wnext="${nextMark(m)}"
  title="Click: ✓ reads well, ✗ needs work, then empty">${m === "ok" ? "✓" : m === "bad" ? "✗" : ""}</span>`;

function wordingRow(r) {
  return `<div class="row ${r.mark || ""}">${tick(r.mark, [r.key])}<div>${kindPill(r)} ${markPill(r)}</div>
    <div>${message(r)}${r.was ? `<div class="small"><span class="muted">was:</span> <span class="was msg">${esc(r.was)}</span></div>` : ""}
      ${W.code[r.key] ? `<pre class="code">${esc(r.code)}</pre>` : ""}</div>
    <div class="loc">${esc(r.loc)}<br><a href="#" data-wcode="${esc(r.key)}">${W.code[r.key] ? "▾ hide code" : "▸ code"}</a></div></div>`;
}

function scenarioBar() {
  const ghost = k => {
    const names = Object.entries(DATA.wording?.roles || {}).filter(([, v]) => v === k).map(([e]) => e);
    return names.slice(0, 3).join(", ") + (names.length > 3 ? ", …" : "");
  };
  return `<div class="scen"><span class="small muted">Fill the variables with:</span>${Object.entries(SCENARIOS).map(([k, s]) =>
      `<button class="${W.scen === k ? "on" : ""}" data-wscen="${k}">${esc(s.name)}</button>`).join("")}</div>
    ${W.scen === "custom" ? `<div class="custom">${Object.entries(ROLES).map(([k, label]) => `<label>${label}<input data-wcustom="${k}"
      placeholder="${esc(ghost(k))}" style="width:${Math.max(12, ghost(k).length + 2)}ch" value="${esc(W.custom[k] || "")}"></label>`).join("")}
      <span class="small muted">Kept for this session only. Empty fields leave the R expression showing. What each expression stands for is in <span class="mono">tests/suite/wording_roles.csv</span>.</span></div>` : ""}`;
}

function wordingView() {
  const all = wordingRows();
  const f = ui.filter.toLowerCase();
  const rows = all.filter(r => (!f || (r.what + (r.hint || "") + r.loc).toLowerCase().includes(f)) && (W.kind === "all" || r.kind === W.kind));
  const marked = rows.filter(r => r.status);
  const files = [...new Set(rows.map(r => r.file))];
  const ok = all.filter(r => r.mark === "ok").length, bad = all.filter(r => r.mark === "bad").length;
  const groupRows = list => {
    const by = {};
    list.forEach(r => (by[shape(r)] ||= []).push(r));
    return Object.values(by).map(g => {
      if (g.length === 1) return wordingRow(g[0]);
      const k = "fold:" + g[0].key, open = W.open[k];
      const m = g.every(r => r.mark === g[0].mark) ? g[0].mark : "";
      return `<div class="row fold">${tick(m, g.map(r => r.key))}<div>${kindPill(g[0])}</div>
        <div class="members"><a href="#" data-wopen="${esc(k)}">${open ? "▾" : "▸"} ${g.length} messages read the same apart from names</a>
        ${message(g[0])}${open ? g.map(wordingRow).join("") : ""}</div></div>`;
    }).join("");
  };
  const n = k => all.filter(r => k === "all" || r.kind === k).length;
  const section = (k, head, body) => {
    const open = W.open[k] ?? true;
    return `<div class="tbl"><div class="h" data-wopen="${esc(k)}" data-wdefault="1">${open ? "▾" : "▸"} ${head}</div>${open ? body : ""}</div>`;
  };
  return `<div class="head"><h2>Wording</h2><span style="flex:1"></span>
      <span class="small muted">${ok} of ${all.length} read well · ${bad} need work · doesn't block CI · blocks the CRAN release check</span></div>
    <p class="muted" style="margin:0">Every message the package can print, as a user sees it. Tick a row when it reads well; fix wording in the R file itself.</p>
    ${ui.error ? `<div class="error">${esc(ui.error)}</div>` : ""}
    <div class="pin">${scenarioBar()}${W.width ? `<div class="snap-top"><button class="snapbtn" data-wsnap>↔ Snap back to full width</button>
      <div class="small muted">${W.width}px wide</div></div>` : ""}</div>
    <div class="chiprow">${["all", "error", "warning", "message"].map(k => `<button class="${W.kind === k ? "on" : ""}" data-wkind="${k}">${k === "all" ? "All" : k[0].toUpperCase() + k.slice(1) + "s"} (${n(k)})</button>`).join("")}</div>
    <div id="squish" style="${W.width ? `width:${W.width}px` : ""}"><div class="grip" title="Drag left to see the messages wrap narrower">
      ${W.width ? `<button class="snap-dot" style="left:${W.releaseX}px;top:${W.releaseY}px" title="Snap back to full width" data-wsnap>›</button>` : ""}</div>
      <div class="stack">
      ${marked.length ? section("marked", `New or changed (${marked.length}) <span class="small muted">also listed under their file</span>`, marked.map(wordingRow).join("")) : ""}
      ${files.map(file => {
        const list = rows.filter(r => r.file === file);
        return section(file, `<span class="mono">${esc(file)}</span> <span class="small muted">${list.filter(r => r.mark === "ok").length}/${list.length} read well</span>`, groupRows(list));
      }).join("")}
      ${rows.length ? "" : `<p class="muted">No messages match.</p>`}</div></div>`;
}

async function markWording(keys, mark) {
  const sites = Object.fromEntries(wordingRows().map(r => [r.key, r]));
  ui.error = null;
  try {
    for (const k of keys) {
      const r = await fetch("/api/wording", { method: "POST", headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ key: k, fingerprint: sites[k].fingerprint, mark }) });
      if (!r.ok) { ui.error = (await r.json()).error || "The server refused the tick."; break; }
    }
    await load();
  } catch (e) {
    ui.error = "Couldn't reach the review server: " + e.message;
  }
  render();
}

function wordingEvents(app) {
  app.querySelectorAll("[data-wmark]").forEach(b => b.addEventListener("click", () => markWording(b.dataset.wmark.split(" "), b.dataset.wnext)));
  app.querySelectorAll("[data-wcode]").forEach(a => a.addEventListener("click", e => { e.preventDefault(); W.code[a.dataset.wcode] = !W.code[a.dataset.wcode]; render(); }));
  app.querySelectorAll("[data-wopen]").forEach(a => a.addEventListener("click", e => {
    e.preventDefault(); const k = a.dataset.wopen; W.open[k] = !(W.open[k] ?? !!a.dataset.wdefault); render();
  }));
  app.querySelectorAll("[data-wscen]").forEach(b => b.addEventListener("click", () => { W.scen = b.dataset.wscen; render(); }));
  app.querySelectorAll("[data-wkind]").forEach(b => b.addEventListener("click", () => { W.kind = b.dataset.wkind; render(); }));
  app.querySelectorAll("[data-wsnap]").forEach(b => b.addEventListener("click", () => { W.width = null; render(); }));
  app.querySelectorAll("[data-wcustom]").forEach(input => input.addEventListener("input", () => {
    const k = input.dataset.wcustom;
    W.custom[k] = input.value; store.set("applr.custom", W.custom); render();
    const again = document.querySelector(`[data-wcustom="${k}"]`);
    again.focus(); again.setSelectionRange(again.value.length, again.value.length);
  }));
  const grip = app.querySelector("#squish .grip");
  grip?.addEventListener("mousedown", e => {
    if (e.target !== grip) return;
    e.preventDefault();
    const box = document.getElementById("squish"), left = box.getBoundingClientRect().left;
    const move = ev => { W.width = Math.max(220, Math.round(ev.clientX - left)); box.style.width = W.width + "px";
      W.releaseY = ev.clientY; W.releaseX = Math.round(box.getBoundingClientRect().right + 2); };
    const up = () => { document.removeEventListener("mousemove", move); document.removeEventListener("mouseup", up); render(); };
    document.addEventListener("mousemove", move);
    document.addEventListener("mouseup", up);
  });
}

// ---- situation list ----
const S = { open: {}, code: {} };
const readMarks = () => { try { return JSON.parse(localStorage.getItem("applr.read") || "{}"); } catch { return {}; } };

function situationsView() {
  const groups = arr(DATA.situations);
  const read = readMarks();
  const f = ui.filter.toLowerCase();
  const total = groups.reduce((n, s) => n + arr(s.tests).length, 0);
  // A Read tick lapses when the file's list of tests changes.
  const sig = s => arr(s.tests).map(t => t.title + "|" + t.sentence).join("\n");
  return `<div class="head"><h2>Numeric tests</h2><span style="flex:1"></span>
      <span class="small muted">${groups.filter(s => read[s.name] === sig(s)).length} of ${groups.length} files read · ${total} tests</span></div>
    <p class="muted" style="margin:0">What the numeric tests check, in plain words. Each compares what Applr drew with the rule in
      <span class="mono">tests/testthat/helper-core.R</span>. Open a test's code only when a line makes you doubt it.</p>
    <div class="chiprow"><button data-sall="1">Unfold all</button><button data-sall="0">Fold all</button></div>
    <div class="outline">${groups.map(s => {
      const tests = arr(s.tests).filter(t => !f || (s.name + t.title + t.sentence).toLowerCase().includes(f));
      if (f && !tests.length) return "";
      const open = S.open[s.name] || !!f, done = read[s.name] === sig(s);
      return `<div class="grp"><div class="h" data-sopen="${esc(s.name)}"><span>${open ? "▾" : "▸"}</span>
        <div><b>${esc(s.name.replace(/^num_/, "").replace(/_/g, " "))} <span class="small muted">· ${arr(s.tests).length} tests</span></b><span>${esc(s.heading)}</span></div>
        <label class="small" data-sread="${esc(s.name)}"><span class="check ${done ? "on" : ""}">${done ? "✓" : ""}</span> Read</label></div>
        ${open ? tests.map((t, n) => {
          const k = s.name + "#" + n;
          return `<div class="t"><span class="n">${n + 1}</span><div><div class="cap">${esc(t.title || t.sentence)}</div>
            ${t.title ? `<div class="sentence">${esc(t.sentence)}</div>` : ""}</div>
            <a href="#" class="small" data-scode="${esc(k)}">${S.code[k] ? "▾ code" : "▸ code"}</a>
            ${S.code[k] ? `<pre class="code">${esc(t.code)}</pre>` : ""}</div>`;
        }).join("") : ""}</div>`;
    }).join("")}</div>`;
}

function situationsEvents(app) {
  app.querySelectorAll("[data-sopen]").forEach(h => h.addEventListener("click", e => {
    if (e.target.closest("[data-sread]")) return;
    S.open[h.dataset.sopen] = !S.open[h.dataset.sopen]; render();
  }));
  app.querySelectorAll("[data-sall]").forEach(b => b.addEventListener("click", () => {
    arr(DATA.situations).forEach(s => S.open[s.name] = b.dataset.sall === "1"); render();
  }));
  app.querySelectorAll("[data-scode]").forEach(a => a.addEventListener("click", e => { e.preventDefault(); S.code[a.dataset.scode] = !S.code[a.dataset.scode]; render(); }));
  app.querySelectorAll("[data-sread]").forEach(l => l.addEventListener("click", e => {
    e.preventDefault();
    const s = arr(DATA.situations).find(x => x.name === l.dataset.sread), read = readMarks();
    const sig = arr(s.tests).map(t => t.title + "|" + t.sentence).join("\n");
    if (read[s.name] === sig) delete read[s.name]; else read[s.name] = sig;
    try { localStorage.setItem("applr.read", JSON.stringify(read)); } catch {}
    render();
  }));
}
