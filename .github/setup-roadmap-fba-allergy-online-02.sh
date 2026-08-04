#!/usr/bin/env bash
#
# setup-roadmap-fba-allergy-online-02.sh
# --------------------------------------
# Roadmap from the `fba-allergy-online-01` pilot to a pre-registered, sampled
# `fba-allergy-online-02`, the large-scale online replication of Beesley &
# Le Pelley (2011), which must land before the Paskewitz & Jones (2020)
# extension (`fba-allergy-online-03`) can be pre-registered.
#
# Creates eight milestones (six dated, two undated placeholders) and 
# thirty six issues. Retrofits the naming of the existing issues and
# milestones, closes one stale milestone and optionally wires everything into
# Project #6 -- CATLAS Pipeline.
#
# THE BOUNDARY RULE, which decides what may be an issue in this repository:
#
#   CATLAS       text and science; the causal argument, the analysis strategy,
#                the pre-registration, the manuscript. Ships *claims*.
#   catlaslab    experiment code and running the studies.
#   {catlasr}    data import, preprocessing and the models.
#   {mathpsychr} whatever generalises past the study after this one.
#
# Several issues below name work in the sibling repositories. Those are
# *tracking* issues: they exist so that a CATLAS milestone cannot silently
# depend on something nobody is doing. They carry `blocked-external` and they
# are closed when the sibling repository's issue closes, not when someone here
# writes code. Nothing in CATLAS should ever contain an experiment or a model.
#
# Repository layout the issue bodies assume (Quarto manuscript project):
#
#   common/references.bib                     shared bibliography
#   common/csl/apa.csl                        shared citation style
#   manuscripts/forward-blocking-attention-01/
#     _quarto.yml                             notebooks must be registered here
#     index.qmd                               the manuscript
#     notebooks/estimands.qmd                 the DAG saying what we are estimating, causally
#     notebooks/estimates.qmd                 blocking indexes  saying how we estimate it
#     notebooks/replication-online-power.qmd  retrospective design analysis
#     notebooks/replication-online-prereg.qmd pre-registration (exists, stub)
#   outputs/abstracts/2026-cognitive-school/  Cognitive School poster, presented
#                                             2026-02-06; also on CATLAS's OSF at
#                                             outputs/posters/20260206-Cognitive-School.pdf
#
# Both estimand notebooks come from splitting the existing
# notebooks/estimands-estimates.qmd along the estimand/estimate seam.
#
# New work in this roadmap:
#   notebooks/estimands.qmd                 the DAG and its provenance table, split out of estimands-estimates.qmd
#   notebooks/estimates.qmd                 the surviving index half of that file
#   notebooks/replication-online-pilot.qmd  the fba-allergy-online-01 analysis
#
# Dates are pegged to the {mathpsychr} roadmap, not to effort estimates:
#
#   2026-08-14  scope lock            (CATLAS-internal; unowned decisions)
#   2026-08-25  causal model          -> mathpsychr v0.1.9002 (2026-08-31)
#   2026-09-10  pilot analysis        -> mathpsychr v0.1.9003 (2026-09-18)
#   2026-09-30  design analysis  HARD -> mathpsychr v0.1.9004 (2026-10-09)
#   2026-10-30  pre-registration      -> mathpsychr v0.1.0    (2026-10-30)
#   2026-12-15  data collection
#
# Usage:
#   bash .github/setup-roadmap-fba-allergy-online-02.sh
#   DRY_RUN=1 bash .github/setup-roadmap-fba-allergy-online-02.sh   # print only
#
# Re-runs are no-ops: milestones are matched by title and patched, issues are
# matched by exact title and skipped, retrofits check before writing.

set -uo pipefail

REPO="${REPO:-josefmana/CATLAS}"
OWNER="${OWNER:-josefmana}"
PROJECT_NUMBER="${PROJECT_NUMBER:-6}"
DRY_RUN="${DRY_RUN:-0}"

command -v gh >/dev/null 2>&1 || { echo "gh CLI not found" >&2; exit 1; }
gh auth status >/dev/null 2>&1 || { echo "gh not authenticated" >&2; exit 1; }

# run <cmd...>
# Under DRY_RUN the trace goes to stderr, so that callers which redirect our
# stdout to /dev/null (to swallow gh's JSON) do not also swallow the trace.
run() {
  if [ "${DRY_RUN}" = "1" ]; then
    printf '    DRY: %s\n' "$1 $2 $3 ..." >&2
  else
    "$@"
  fi
}

echo "Repository: ${REPO}"
[ "${DRY_RUN}" = "1" ] && echo "*** DRY RUN, nothing will be written ***"
echo

# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

# ms <title> <due YYYY-MM-DD | ""> <description>
# Creates or updates a milestone. An empty due date creates an undated
# milestone (used for the programme placeholders, which must be visible
# without pretending to a schedule they do not have).
# Sets the global M to the title, which mk() then attaches to every issue it
# creates until M is reassigned.
ms() {
  local title="$1" due="$2" desc="$3" num
  local -a args=(-f title="${title}" -f state=open -f description="${desc}")
  [ -n "${due}" ] && args+=(-f due_on="${due}T23:59:59Z")
  num=$(gh api "repos/${REPO}/milestones?state=all&per_page=100" \
          --jq ".[] | select(.title == \"${title}\") | .number" 2>/dev/null | head -n1)
  if [ -n "${num}" ]; then
    run gh api -X PATCH "repos/${REPO}/milestones/${num}" "${args[@]}" >/dev/null \
      && echo "  = milestone: ${title}${due:+ (${due})}"
  else
    run gh api -X POST "repos/${REPO}/milestones" "${args[@]}" >/dev/null \
      && echo "  + milestone: ${title}${due:+ (${due})}"
  fi
  M="${title}"
}

# msrename <old title> <new title>
msrename() {
  local old="$1" new="$2" num
  num=$(gh api "repos/${REPO}/milestones?state=all&per_page=100" \
          --jq ".[] | select(.title == \"${old}\") | .number" 2>/dev/null | head -n1)
  [ -z "${num}" ] && { echo "  . milestone absent, nothing to rename: ${old}"; return 0; }
  run gh api -X PATCH "repos/${REPO}/milestones/${num}" -f title="${new}" >/dev/null \
    && echo "  ~ milestone renamed: ${old} -> ${new}"
}

# msclose <title> <description>
msclose() {
  local title="$1" desc="$2" num state
  num=$(gh api "repos/${REPO}/milestones?state=all&per_page=100" \
          --jq ".[] | select(.title == \"${title}\") | .number" 2>/dev/null | head -n1)
  [ -z "${num}" ] && { echo "  . milestone absent: ${title}"; return 0; }
  state=$(gh api "repos/${REPO}/milestones/${num}" --jq .state 2>/dev/null)
  [ "${state}" = "closed" ] && { echo "  . already closed: ${title}"; return 0; }
  run gh api -X PATCH "repos/${REPO}/milestones/${num}" \
    -f state=closed -f description="${desc}" >/dev/null \
    && echo "  x milestone closed: ${title}"
}

# mk <title> <labels csv> <body>
# Title-deduped: an issue with this exact title, open or closed, is left alone.
mk() {
  local title="$1" labels="$2" body="$3"
  if gh issue list --repo "${REPO}" --state all --limit 500 --json title \
       --jq '.[].title' 2>/dev/null | grep -Fxq "${title}"; then
    echo "  . exists: ${title}"
    return 0
  fi
  run gh issue create --repo "${REPO}" \
    --title "${title}" --body "${body}" \
    --label "${labels}" --milestone "${M}" >/dev/null \
    && echo "  + ${title}"
}

# ret <issue number> <new title>
# Renames only if the current title differs, so re-runs are silent.
ret() {
  local num="$1" new="$2" cur
  cur=$(gh issue view "${num}" --repo "${REPO}" --json title --jq .title 2>/dev/null)
  [ -z "${cur}" ] && { echo "  . issue #${num} not found"; return 0; }
  [ "${cur}" = "${new}" ] && { echo "  . #${num} already: ${new}"; return 0; }
  run gh issue edit "${num}" --repo "${REPO}" --title "${new}" >/dev/null \
    && echo "  ~ #${num}: ${cur} -> ${new}"
}

# bodysub <issue number>
# Rewrites the legacy study names inside an existing issue body.
bodysub() {
  local num="$1" body new tmp
  body=$(gh issue view "${num}" --repo "${REPO}" --json body --jq .body 2>/dev/null)
  [ -z "${body}" ] && { echo "  . #${num} has no body"; return 0; }
  new=$(printf '%s' "${body}" \
    | sed -e 's/forward-blocking-replication-online-pilot/fba-allergy-online-01/g' \
          -e 's/forward-blocking-generalisation-online/fba-[alt]-online-01/g' \
          -e 's/forward-blocking-extension-online/fba-allergy-online-03/g' \
          -e 's/forward-blocking-replication-online/fba-allergy-online-02/g' \
          -e 's/fb-allergy-online-/fba-allergy-online-/g')
  [ "${new}" = "${body}" ] && { echo "  . #${num} body already migrated"; return 0; }
  if [ "${DRY_RUN}" = "1" ]; then echo "    DRY: rewrite body of #${num}"; return 0; fi
  tmp=$(mktemp) && printf '%s' "${new}" > "${tmp}"
  gh issue edit "${num}" --repo "${REPO}" --body-file "${tmp}" >/dev/null \
    && echo "  ~ #${num} body migrated"
  rm -f "${tmp}"
}

# mvi <issue number> <milestone title>
# Retrofits a milestone onto an issue that predates it.
mvi() {
  local num="$1" milestone="$2"
  run gh issue edit "${num}" --repo "${REPO}" --milestone "${milestone}" >/dev/null \
    && echo "  > #${num} -> ${milestone}"
}

# --------------------------------------------------------------------------
# 0 · Retrofits
#
# Two vocabularies are in circulation: `forward-blocking-*-online` in issue
# titles and`fba-allergy-online-0X` in the Study field of Project #6. The
# Project field is authoritative. Everything else is migrated to it.
# --------------------------------------------------------------------------

echo "0 · Retrofitting existing issues and milestones"

ret  5 "Power analysis (\`fba-allergy-online-02\`)"
ret  6 "Data import (\`fba-allergy-online-01\`)"
ret  9 "\`fba-allergy-online-01\` pilot"
ret 10 "\`fba-allergy-online-02\` study"
ret 11 "\`fba-allergy-online-03\` study"
ret 12 "\`fba-[alt]-online-01\` study"

for n in 2 5 6 7 8 9 10 11 12; do bodysub "${n}"; done

msrename "\`forward-blocking-replication-online-pilot\` data collection" \
         "\`fba-allergy-online-01\` · data collection"
msrename "\`forward-blocking-replication-online-pilot\` data analysis" \
         "\`fba-allergy-online-01\` · pilot analysis"
msrename "\`forward-blocking-extension\` next step pre-registration" \
         "\`fba-allergy-online-02\` · scope lock"

msclose "\`fba-allergy-online-01\` · data collection" \
"Closed retrospectively. Pilot data were collected and the milestone carried no issues; it was overdue only because nothing ever closed it. Kept for the record rather than deleted, because the roadmap's dates are easier to read against a complete history."

echo

# --------------------------------------------------------------------------
# 1 · Scope lock 2026-08-14
# --------------------------------------------------------------------------

echo "1 · \`fba-allergy-online-02\` · scope lock (2026-08-14)"

ms "\`fba-allergy-online-02\` · scope lock" "2026-08-14" \
"**Must contain**

* A written record that the full replication is committed, naming the falsifiers that would have reversed it
* Study naming migrated to the \`fba-<task>-<mode>-0X\` vocabulary across issues, milestones and the handoff
* Ownership recorded: every decision and sign-off on this roadmap is Jožka's
* Pre-registration platform confirmed and the DAG-registrability question answered
* The expected relation between our DAG and the original analysis recorded

This milestone exists because the critical path carries several items that are cheap to settle in August and expensive to settle in October. Three were open when the roadmap was drafted and are now closed by decision: ownership, the replication-versus-extension fork and the framing of our formalism against Beesley & Le Pelley's. They stay here as issues because a decision that is made but unwritten is indistinguishable from one that was never made. It replaces \`forward-blocking-extension\` next step pre-registration, whose fork, i.e., full replication, or straight to the extension, is no longer open."

mk "Record the decision to run the full replication" "decision,register" \
"The milestone \`forward-blocking-extension\` next step pre-registration framed this as a live fork: run a full pre-registered replication *(default)*, or pre-register the extension directly without a large-scale replication first. That fork is closed following exploratory look at \`fba-allergy-online-01\` data. \`fba-allergy-online-02\` **is** the replication.

Write the decision down, and write down what would have reversed it: pilot attrition beyond a stated threshold, failure of the Stage 1–3 learning checks or evidence that the three-stage allergy task does not survive unsupervised online administration. Then confirm, against the pilot, that none of those occurred.

A decision that stays implicit cannot be audited afterwards and this one commits roughly four months of work and a lot of entire recruitment budget."

mk "Migrate study naming to the \`fba-<task>-<mode>-0X\` vocabulary" "writing" \
"Two vocabularies are in circulation:

| Where | Vocabulary |
|---|---|
| Issue and milestone titles | \`forward-blocking-replication-online\`, \`forward-blocking-extension-online\`, \`forward-blocking-generalisation-online\` |
| \`Study\` field, Project #6 | \`fba-allergy-online-01\` / \`-02\` / \`-03\`, \`fba-[alt]-online-01\`, \`study-agnostic\` |

The Project field wins: \`fba\` reads as forward-blocking-attention and matches the \`forward-blocking-attention-01\` series name.

- [ ] Retitle CATLAS #5, #6, #9, #10, #11, #12 and migrate their bodies
- [ ] Retitle the three open milestones
- [ ] CATLAS #6 carries \`Study = study-agnostic\` but names a specific study in its title, one of the two is wrong
- [ ] Check the sibling repositories: catlaslab #7 and #9 and catlasr #4, #7 all carry \`forward-blocking-*\` names"

mk "Record sole ownership of decisions and sign-offs" "decision" \
"Answered and recorded here because the handoff flags it twice (§3.3, §8) as unowned and on the critical path.

**Jožka owns every decision and sign-off on this roadmap**, including the plausible-effect range, the Type M target, the pre-registration platform, the exclusion rules and the framing of the causal argument. There is no second signature to wait for and no co-author gate anywhere in the chain.

This is worth one paragraph rather than nothing, for two reasons. It closes an open question the upstream document will otherwise keep raising. And it changes what the risk actually is: with a single owner, nothing on this roadmap can be delayed by someone else's calendar, so every date that slips slips for a reason that is visible from inside the repository. That is a better failure mode than the alternative, but only if it is stated, because an unstated single owner looks exactly like an unassigned task."

mk "Confirm the pre-registration platform and check whether it can register a DAG" "register,long-lead,blocked-external" \
"Prereg or Prospero for the **registration**, explicitly not OSF.

Note that this does not mean avoiding OSF: CATLAS already hosts artefacts there (\`outputs/posters/20260206-Cognitive-School.pdf\`). The constraint is on where the study is registered, not on where files live, and conflating the two would throw away a useful timestamping service.

Question in a need of answer: can either registry hold a causal graph as a first-class object or does the DAG enter as prose plus an appendix figure?

This is not cosmetic. It decides whether the graph freeze in the causal-model milestone is enforced by a third party or only by us. If only by us, the freeze needs its own mechanism, e.g., a signed tag, a timestamped commit, something with a date that we cannot quietly move. This mechanism has to exist before any implication test runs."

mk "Record that the DAG is expected to formalise, not revise, the original analysis" "decision,writing" \
"Beesley & Le Pelley (2011) ran a well-designed experiment. Their estimand and their estimate are very likely to pair exactly as ours do causally and our DAG will most likely do nothing more interesting than write down formally what they argued in prose. **The primary endpoint is their estimate, unchanged.** There is therefore no comparison for a limitation to attach to.

Where our work adds something is at the secondary endpoints: \`fba-allergy-online-01\`-inspired measurement improvements, each carrying a formal causal identification argument that the original could not state because it had no identification framework. That is an addition, not a correction.

So the framing is: **formalisation, with additions at the margin.** Not a critique of the literature and not an apology for a mismatch.

- [ ] If the derived adjustment sets *do* diverge from the original analysis, that is a surprise and it invalidates the expectation recorded here. Reopen this issue rather than quietly reframing the manuscript around it."

echo

# --------------------------------------------------------------------------
# 2 · Causal model — 2026-08-25
# --------------------------------------------------------------------------

echo "2 · \`fba-allergy-online-01\` · causal model (2026-08-25)"

ms "\`fba-allergy-online-01\` · causal model" "2026-08-25" \
"**Must contain**

* The three-stage blocking DAG declared in \`mpr_dag\` terms, with node roles and latents marked
* A citation or quotation justifying every edge, and an explicit note wherever a latent is posited by us rather than forced by the design
* The graph frozen (i.e., committed, tagged, timestamped) before any implication test is run
* Adjustment sets and testable implications derived for the pilot analysis
* The graph handed to {mathpsychr} as the v0.1.9002 vignette example and test fixture

Due 2026-08-25 so that {mathpsychr} v0.1.9002 can ship on 2026-08-31 with a worked example.

The source literature argues causal implications in prose, informally. This graph is therefore a formalisation rather than a transcription and **there is no answer key**: adjustment sets derived from it cannot be checked against what the original papers adjusted for, because those papers reasoned on informal grounds.

**We expect concordance.** Beesley & Le Pelley (2011) designed their experiment well and the likely outcome is that our graph formalises exactly the estimand/estimate pairing they already used. Divergence is the surprising case, not the anticipated one. If it does occur, it is a finding about the literature rather than evidence that our encoding is wrong. However, it should be treated as a signal to re-check the encoding first, because the prior is against it.

Forking paths remain live regardless. The graph is ours and is being drawn after the original results are known, so it can be adjusted until its implications pass and no code will stop that. Concordance being expected makes this *more* pressing rather than less, because a graph that is supposed to agree with the original is a graph with an obvious target to drift towards.

**Built from scratch.** The Cognitive School poster (2026-02-06) contains hand-drawn causal graphs for attention-as-cause and attention-as-epiphenomenon. They are unvalidated and superseded, and are not the starting point for this work. They do, however, exist publicly on OSF, which is a record-keeping problem rather than a modelling one (see the freeze issue).

**\`fba-allergy-online-02\` measures no eye-gaze.** It is a behavioural replication by design. Any gaze node in the graph is therefore unmeasured in this study and adjustment sets that require one are not available here."

mk "Declare the three-stage blocking DAG" "theory,statistics" \
"Nodes, roles and latents for the Le Pelley (2007) / Beesley & Le Pelley (2011) three-stage design as instantiated in \`fba-allergy-online-01\`.

{mathpsychr} wants this as the vignette anchoring its pkgdown site because **the alternative anchor, the eye-tracking vignette, depends on external data** that cannot ship with the package. The DAG vignette needs none, since it is literal \`dag_nodes()\` / \`dag_edges()\` calls. That reasoning is about data dependency alone and does not change if a future eye-tracking study acquires a DAG of its own.

The **content** lives in CATLAS. {mathpsychr} ships only the machinery (see the boundary rule). A capability belongs upstream if the study *after* \`fba-allergy-online-02\` will call it unmodified. A graph of this specific design will not be.

Notebook: \`manuscripts/forward-blocking-attention-01/notebooks/estimands.qmd\`, i.e., the causal half of the current \`estimands-estimates.qmd\`, split per the issue below. It sits upstream of \`notebooks/estimates.qmd\`, which carries the index definitions and the validity argument this graph has to be consistent with."

mk "Split \`estimands-estimates.qmd\` into \`estimands\` and \`estimates\`" "writing,theory" \
"\`notebooks/estimands-estimates.qmd\` currently carries two things that belong apart: what we are estimating (the causal quantity) and how we estimate it (the blocking indexes).

Split it along that seam:

| New file | Takes |
|---|---|
| \`notebooks/estimands.qmd\` | the DAG, its provenance table, the causal identification argument |
| \`notebooks/estimates.qmd\` | BI definitions, the premise-by-premise validity argument, the index comparison |

- [ ] Register both in \`_quarto.yml\` and remove the old entry
- [ ] Update the cross-reference in \`index.qmd\` (currently points at \`estimands-estimates.qmd\` for the BI₂ definition)
- [ ] Update the footnote in \`replication-online-power.qmd\` that links to the *Dependent Variables* notebook
- [ ] Check \`_manuscript/\` for stale rendered artefacts of the old name

The rename is worth doing before the DAG is written rather than after, because the estimand/estimate distinction is exactly what the causal argument is about and a file whose name straddles it will keep inviting the two to be conflated."

mk "Attach provenance to every edge and latent" "theory,writing" \
"{mathpsychr} v0.1.9002 adds per-edge and per-latent justification fields, surfaced by \`as_gt()\`. CATLAS has to supply the content: a quotation or citation per arrow, and an explicit note wherever a latent is posited by us rather than forced by the design.

This is not bookkeeping. The provenance table becomes the causal subsection of the Methods and it is the only available defence against the objection that the graph was drawn to yield the adjustment set we wanted.

Coordinate with the upstream issue *Attach provenance to edges and latent nodes* so the field names match on both sides."

mk "Freeze the graph before running any implication test" "register,decision" \
"CATLAS owns this constraint; {mathpsychr} will only emit a caveat.

Either the graph is registered **before** the implication tests are run, or every implication test in the manuscript is reported as exploratory.

Mechanism depends on the platform answer from the scope-lock milestone. Note that CATLAS already hosts artefacts on OSF (\`outputs/posters/20260206-Cognitive-School.pdf\`) and the *not OSF* applies to the **registration**, not to artefact hosting. Consequently, a timestamped OSF deposit of the frozen graph is available as a freeze mechanism even if the registration lives on Prereg or Prospero. A signed git tag referenced by hash is the fallback.

- [ ] Record that the Cognitive School poster's graphs are superseded. They are public on OSF and differ from whatever we register. A reviewer who finds both should find our note first, not the discrepancy. One paragraph, dated, in the causal-model notebook."

mk "Derive adjustment sets and testable implications for the pilot" "statistics" \
"Run \`process()\` on the declared graph to obtain the adjustment sets the pilot analysis will actually use and the implications it will actually test. \`compute()\` runs the implication tests, \`as_gt()\` renders them in pre-registration shape.

If a derived set differs from what Beesley & Le Pelley (2011)'s analyses, record the difference in a table."

mk "Hand the graph to {mathpsychr} as vignette example and test fixture" "blocked-external" \
"Deadline ~2026-08-25, because it blocks the v0.1.9002 vignette and test fixture and that release ships 2026-08-31.

Send the declaration calls and the provenance table, not a rendered figure. Confirm the fixture shape upstream before freezing this milestone."

echo

# --------------------------------------------------------------------------
# 3 · Pilot analysis — 2026-09-10
# --------------------------------------------------------------------------

echo "3 · \`fba-allergy-online-01\` · pilot analysis (2026-09-10)"

ms "\`fba-allergy-online-01\` · pilot analysis" "2026-09-10" \
"**Must contain**

* Learning checks across Stages 1--3, graphical certainly and statistical if defensible
* Primary: BI₂ from the **first test trial only**, against the no-blocking null, using the adjustment set derived from the registered graph
* Calibration: BI₂ across all test trials, compared against the trial 1 estimate
* Alternatives: BI₃/PJ; meta-d′ if the MLE lands. **BI₁ is not computable under 2AFC** and is reported from the literature only
* Optional measurement work: meta-d′ as a non-ad-hoc SDT alternative and lightweight GLMMs as assumption checkers for every averaged index
* Point estimates **with standard errors**, in a form {mathpsychr} can consume
* A feasibility audit: attrition, completion time, comprehension, exclusion-rule yield
* Exclusion rules fixed in language that can be pre-registered verbatim for \`fba-allergy-online-02\`

Retitled and re-dated from \`forward-blocking-replication-online-pilot\` data analysis, which was due 2026-07-31.

The standard errors are the load-bearing output. They fill the \`REF\` slot in {mathpsychr}'s \`test-gelman-carlin-power.R\`."

mk "Compute learning checks for Stages 1–3" "data,statistics" \
"Did participants learn the contingencies at all? Graphical checks are non-negotiable; statistical ones are optional and should be argued for rather than assumed.

This is the check that would have vetoed the whole programme had it failed, so it is worth reporting in full even now that it has not. Note the preprocessing constraint recorded in {catlasr}: the \`response\` column holds surface stimulus values and must be joined against the \`stimuli\` mapping via \`.resolve_response_outcomes()\`, and Stage 1 contingency types must be inferred from \`cue1\` rather than from \`type\`. Skipping either produces a learning curve that looks fine and means nothing."

mk "Estimate the primary BI₂ from the first test trial only" "statistics" \
"BI₂ = E[Conf × (I{o2} − I{o1})], the confidence-weighted 2AFC index, computed **from the first test trial only**. That is how Beesley & Le Pelley (2011) did it and matching them exactly is the entire point of a replication (the primary endpoint is their estimate, not an improved one).

Test against the no-blocking null, adjusting per the set derived from the registered graph. Report the estimate, its standard error and its interval, not only whether it cleared a threshold.

Everything else in this milestone is calibration, alternative or exploratory. This is the only confirmatory number and it is the one the pre-registration will name.

- [ ] Confirm the test-trial count against the implemented design before writing the code. \`index.qmd\` records that each of eight test trial types was presented **twice** in \`fba-allergy-online-01\`, with a note questioning whether the second presentation is justified given Beesley & Le Pelley presented each once. Change appropriately according our decision to present **six rounds** of test trials.

Notebook: \`manuscripts/forward-blocking-attention-01/notebooks/replication-online-pilot.qmd\`, registered in \`_quarto.yml\`. Implement the definition as written in \`notebooks/estimates.qmd\`; if the two disagree, fix the notebook rather than the code silently."

mk "Calibrate BI₂ across all test trials against the trial 1 estimate" "statistics" \
"The primary endpoint uses one trial because that is what the original did. It is not obvious that this is the *better* measurement and the pilot is the right place to find out.

There is no strong theoretical reason to think the later test replicates measure a different construct, as **no feedback is given during the test phase**, so nothing in the procedure updates the latent quantity between replicates. Against that, scores could drift from their true latent value for reasons outside the construct (e.g., boredom, fatigue, disengagement) which is an empirical question rather than a theoretical one.

So compute BI₂ across all test trials and compare it against the trial 1 estimate. Report both, with standard errors.

This is a **data-mining exercise on pilot data** and must be labelled as such. It informs how \`fba-allergy-online-02\` gets pre-registered; it is not itself a test of anything. Two outcomes are useful:

* the two estimates agree → the averaged index gains credibility and the trial 1 primary costs us precision for nothing but fidelity
* they disagree → that is a measurement finding worth its own paragraph and it bears directly on whether Beesley & Le Pelley's published magnitude is the quantity our effect range should be built from

Either way the primary endpoint for \`-02\` stays trial 1 unless the pre-registration argues otherwise explicitly."

mk "Report pilot estimates with standard errors for {mathpsychr}" "statistics,blocked-external" \
"Needed upstream by ~2026-09-10 for the v0.1.9003 vignette and to fill the \`REF\` slot in \`test-gelman-carlin-power.R\`.

Send estimates **and** standard errors. Gelman–Carlin design analysis takes the standard error of the observed effect as a first-class input; an estimate on its own is useless to it.

Send **the primary trial 1 BI₂** as the headline figure, since that is the endpoint \`fba-allergy-online-02\` will be powered for. Send the calibration estimate alongside it, labelled, because if the two diverge the design analysis should be run against both rather than silently against whichever was more convenient."

mk "Compute alternative indices: BI₃/PJ and meta-d′ if available" "statistics" \
"**BI₁ cannot be computed here.** It needs each cue rated separately and our test phase is 2AFC. It stays in the manuscript because it is the field's default index and the reader will expect to see it discussed, but it is reported *from the literature* and never estimated from our data. Anywhere the roadmap or the notebooks imply otherwise is a bug.

What we can compute:

* **BI₃/PJ**: the Paskewitz & Jones-style 2AFC proportion score. The main alternative and the most similar to the one Griffiths & Mitchell (2009, Exp. 3) used to get the largest effect in the comparison table.
* **meta-d′**: see the separate measurement issue; it is listed here because it doubles as an alternative index, not only as an improvement.

Label these exploratory, in the code and in the text. The primary index was fixed in advance; these were not.

Blocker on meta-d′: \`metaSDT\` is unavailable for the current R version, so the self-contained \`stats::optim()\` MLE implementation in {catlasr} has to be finished and validated first. If it does not land in time, drop meta-d′ from the pilot rather than shipping an unvalidated implementation. It is optional here by design."

mk "Fit lightweight GLMMs as assumption checkers for the averaged indices" "statistics" \
"Every index in this milestone rests on an untested assumption. The calibration BI₂, BI₃ and meta-d′ all assume **the average across test trials is a good measure**. The primary BI₂ assumes **the first trial is**. Neither assumption has been checked, in our data or in the literature.

A random-effects GLMM on the test-phase data checks both, cheaply:

1. **Are random slopes needed?** If not, responses are likely stable enough across test trials and the averaged indices (calibration BI₂, BI₃, meta-d′) gain credibility as measures of a single latent quantity.
2. **Does the random-intercept estimate sit near the trial-1 result?** If not, Beesley & Le Pelley's trial-1 BI₂ and therefore our primary endpoint may be biased, which is a finding that belongs in the text rather than in a footnote.

Keep these **lightweight**. They are diagnostics for the indices above, not a modelling programme, and they should be reported as assumption checks.

**Explicitly out of scope for this manuscript**, recorded so the boundary is deliberate rather than accidental: latent-state models of Stage 3 learning quality predicting test-phase decision tendency; decision and confidence as correlated latent variables; integration with EXIT or with Paskewitz & Jones (2023)'s Bayesian derived-attention model, which is already a Bayesian regression and would slot in naturally. That programme is larger than this project can carry and belongs to a later manuscript. Note also that the modelling would live in {catlasr}, not here.

Boundary rule: if the GLMM code generalises past the next study it belongs in {mathpsychr}, otherwise {catlasr}. Either way CATLAS carries the argument, not the implementation, and the handoff warns that \`{lme4}\`/\`{brms}\` wrappers may never arrive upstream, so assume they will not."

mk "Audit online feasibility of the allergy task" "data" \
"Attrition by stage, completion time distribution, comprehension-check performance and how many participants each candidate exclusion rule removes.

The three-stage design is long and it was built for supervised laboratory administration. Whether it survives unsupervised online delivery is an empirical question the pilot can answer and the replication cannot afford to ask.

This audit also supplies the expected-yield figure the Prolific/JATOS sampling plan needs in November."

mk "Fix exclusion rules in pre-registrable language" "data,register" \
"Write the exclusion rules so they can be pasted verbatim into the \`fba-allergy-online-02\` registration and applied by someone who has not seen the data.

Every rule needs a threshold, not an adjective. \"Excluded for inattention\" is not a rule; \"excluded if fewer than 6 of 8 Stage 1 comprehension trials correct in the final two blocks\" is.

Report the yield of each rule on the pilot, so the registered version is calibrated rather than guessed."

echo

# --------------------------------------------------------------------------
# 4 · Design analysis — 2026-09-30 (HARD)
# --------------------------------------------------------------------------

echo "4 · \`fba-allergy-online-02\` · design analysis (2026-09-30, HARD)"

ms "\`fba-allergy-online-02\` · design analysis and effect range" "2026-09-30" \
"**Must contain**

* Retrospective design analysis of the pilot (power, Type S, Type M) via {mathpsychr} v0.1.9003
* A plausible-effect **range** for forward blocking, sourced from the literature and signed off by a named person
* A written statement of what prospective power actually needs from {mathpsychr}
* A fallback that does not require {mathpsychr} to grow a prospective component
* An acceptable Type M exaggeration ratio, chosen and defended

**This date is hard.** It gates {mathpsychr} v0.1.9004 (2026-10-09). Letting it slip does not quietly resolve to *do nothing*, it moves the argument into {catlasr} with less time to have it.

Note the reframing inherited from upstream: sufficiency for \`fba-allergy-online-02\` is an acceptable **exaggeration ratio at plausible effect sizes**, not 80% power. The API takes a range as a first-class input, so a single d will not do."

mk "Run retrospective design analysis on the pilot" "statistics,blocked-external" \
"Gelman–Carlin power, Type S and Type M on the pilot estimates, using {mathpsychr} v0.1.9003 (ships 2026-09-18, this issue cannot start before then).

\`manuscripts/forward-blocking-attention-01/notebooks/replication-online-power.qmd\` already does this for the *published* Beesley & Le Pelley (2011) numbers at A = x̄, x̄/2, x̄/4, via \`mathpsychr::gelman_carlin_power()\` and \`compute()\`. Extend that notebook rather than starting another.

Repeat the exercise against our own pilot and reconcile the two: if our pilot and their publication disagree about the plausible magnitude, that disagreement is the most informative thing the pilot produced and belongs in the text.

Note the notebook ends with an empty \`## Prospective power analysis\` heading. That heading is this milestone's other three issues."

mk "Source and sign off the plausible-effect range" "statistics,decision" \
"\`replication-online-power.qmd\` currently proposes **d ∈ [0.15, 0.30]**, derived as x̄/4 to x̄/2 of Beesley & Le Pelley (2011), with 0.40 and 0.50 as best cases. The supporting evidence is assembled but the range is not yet signed off:

| Study | Measure | Cohen's d |
|---|---|---|
| Beesley & Le Pelley (2011) | BI₂ | ≈ 0.55, back-derived from t(25) = 2.74 |
| Le Pelley et al. (2007) | BI₁ | from F(1,7) = 7, wide interval |
| Griffiths & Mitchell (2009) Exp. 1 | BI₁ | from t(31) = 2.07, lower bound near zero |
| Griffiths & Mitchell (2009) Exp. 3 | BI₃ | from t(39) = 5.03, larger than Beesley |

The spread is wide and partly confounded with which index each study used. Two further reasons to discount the published magnitudes: replication effects run about half their originals (Open Science Collaboration, 2015) and Maes et al. (2016) failed to detect blocking across fifteen rodent replications.

Turn this into a defended range. **Jožka signs it off**, per the ownership issue in the scope-lock milestone. There is no second signature to wait for, which means the only thing that can delay this is the writing.

One consistency point worth stating in the notebook: Beesley & Le Pelley's *d* ≈ 0.55 is derived from their trial 1 BI₂, which is **exactly our primary endpoint**. The range therefore applies to the primary endpoint directly, with no measure conversion in between. The BI₁ and BI₃ rows in the table above do not have that property and are weaker evidence for it. This point may be sharpen by the calibration analysis.

It is the input to every sample-size number in the registration."

mk "State what prospective power actually needs" "decision,blocked-external" \
"**Hard deadline 2026-09-30.** This is the {mathpsychr} v0.1.9004 gate.

Write down what a prospective design-analysis component would have to do for \`fba-allergy-online-02\`: inputs, outputs and whether the study *after* this one would call it unmodified. That last clause is the boundary rule and it decides whether the component belongs in {mathpsychr} or in {catlasr}.

Three outcomes are all legitimate:
  
  - a general prospective component upstream,
  - a project-specific one in {catlasr}, or
   a written decision that neither is needed.

What is not legitimate is silence, which resolves to the same argument happening in October with less room."

mk "Write the fallback that does not need {mathpsychr}" "statistics" \
"CATLAS must not assume a prospective component ever arrives. {mathpsychr}'s option (c) upstream — {catlasr} calls \`{pwrss}\` directly — is possible and legitimate.

Write that path now, while it is a precaution, rather than in October, when it would be a scramble. \`replication-online-power.qmd\` already loads \`{pwrss}\`, so the fallback is mostly a matter of deciding it is acceptable and saying so.

Same applies to hierarchical modelling: wrappers around \`{lme4}\` / \`{brms}\` may never arrive upstream. Plan for LMM work in {catlasr}, with {mathpsychr} support as a bonus and not a dependency."

mk "Set the Type M exaggeration target and defend it" "decision,statistics" \
"Sufficiency has been reframed upstream: not 80% power, but an acceptable exaggeration ratio across the plausible-effect range.

Pick the ratio. Defend it in a paragraph that a reviewer can disagree with (a target of 1.1 and a target of 1.5 may imply very different sample sizes and the choice is a judgement, not a convention to be cited).

Then state what happens at the bottom of the range: if d = 0.15 requires an N that recruitment cannot reach, that is a finding about the design and needs saying out loud before the registration, not after."

echo

# --------------------------------------------------------------------------
# 5 · Pre-registration — 2026-10-30
# --------------------------------------------------------------------------

echo "5 · \`fba-allergy-online-02\` · pre-registration (2026-10-30)"

ms "\`fba-allergy-online-02\` · pre-registration" "2026-10-30" \
"**Must contain**

* Sample size solved to the Type M target across the plausible-effect range
* An analysis plan: estimand, model, adjustment set, exclusions, stopping rule
* The pre-registration drafted *against* the {mathpsychr} API, not against an imagined one
* A deviations log against \`fba-allergy-online-01\`
* Ethics and data-protection clearance
* The registration submitted

Dated to {mathpsychr}'s v0.1.0 tag, which is deliberately held until this draft exists: the pre-registration is the only acceptance test that catches an API that is technically complete and practically unusable.

Platform is Prereg or Prospero — explicitly not OSF."

mk "Solve sample size to the Type M target" "statistics" \
"Solve N against the exaggeration-ratio target across the whole plausible-effect range, not at a point estimate.

Report the resulting N at each end of the range and state which one is registered and why. Registering the N that works at d = 0.30 and hoping the effect is not 0.15 is the failure mode this entire milestone chain exists to prevent."

mk "Write the analysis plan" "register,statistics" \
"Estimand, model, the adjustment set from the registered graph, exclusion rules from the pilot and the stopping rule.

The estimand is **BI₂ from the first test trial**, matching Beesley & Le Pelley (2011). Name the tier of everything else explicitly (i.e., calibration, alternative, exploratory) so that the registration cannot later be read as having pre-registered four endpoints and picked one.

Say explicitly which analyses are confirmatory and which are exploratory and commit to reporting the exploratory ones as exploratory even if they are the interesting results. Given how wide the literature's effect estimates are, that commitment will be tested.

Goes in \`notebooks/replication-online-prereg.qmd\`. Cite \`notebooks/estimands.qmd\` for the causal quantity and \`notebooks/estimates.qmd\` for the index definition rather than restating either: two divergent statements of the same estimand is precisely the kind of drift a registration is supposed to prevent."

mk "Draft the pre-registration against the {mathpsychr} API" "register,writing,blocked-external" \
"Write the registration by *calling* the package, not by describing what it will presumably do.

This is the upstream acceptance test: the v0.1.0 tag is held until this draft exists, on the reasoning that a pre-registration is the only thing that reliably exposes an API that passes its own tests and cannot be used for real work. Report the friction back upstream, as found here, it is the deliverable, not a nuisance.

The notebook already exists: \`manuscripts/forward-blocking-attention-01/notebooks/replication-online-prereg.qmd\`. It renders to a preview but has no executed output in \`_manuscript/\`, so it is currently prose without computation. Change it."

mk "Log deviations from \`fba-allergy-online-01\`" "register" \
"Every difference between the pilot and the registered study: task changes from catlaslab #7 and #9, exclusion-rule changes, index changes, sampling changes.

Justify each one. Unlogged deviations are indistinguishable from post-hoc tuning by the time anyone asks and the pilot's whole value is that it makes the differences visible."

mk "Clear ethics and data protection" "long-lead,blocked-external" \
"Latency, not effort. Starting this in late October means the registration waits on it.

Cover: consent flow for an online sample, GDPR basis and retention for Prolific/JATOS IDs and whether the pilot's approval extends to a large-scale replication or a fresh application is required."

mk "Submit the registration" "register" \
"Submit to the chosen registry, record the registration identifier and timestamp, and cross-reference the frozen graph by tag or hash.

Nothing in the data-collection milestone starts before this issue closes."

echo

# --------------------------------------------------------------------------
# 6 · Data collection — 2026-12-15
# --------------------------------------------------------------------------

echo "6 · \`fba-allergy-online-02\` · data collection (2026-12-15)"

ms "\`fba-allergy-online-02\` · data collection" "2026-12-15" \
"**Must contain**

* The experiment released with the pilot fixes merged (catlaslab #7, #9)
* A Prolific/JATOS sampling plan: batching, screening, payment (PROLIFIC), expected yield against the registered N
* Import and validation through {catlasr}, with \`validate_fb_rawdata()\` passing
* Quality monitoring during collection that does not read the estimand

Begins only once the registration is filed. Sequenced after the CATLAS-facing {mathpsychr} milestones so that nothing in the analysis pipeline is still moving while data arrive."

mk "Track: experiment released with pilot fixes" "experiment,blocked-external" \
"**Tracking issue.** The work is catlaslab #7 (scale pages relatively) and #9 (fix bugs encountered during pilot); both carry \`Study = fba-allergy-online-02\` and both sit in Backlog. No experiment code is written in CATLAS. Close this when those close.

It is here because the registration cannot be submitted against a task that is still changing. Merge, tag and deploy before recruitment opens."

mk "Write the Prolific/JATOS sampling plan" "data,long-lead" \
"Batching, screening criteria, payment rate (Prolific) and expected yield derived from the pilot's attrition and exclusion figures.

Recruit against *post-exclusion* N. The registered N is participants whose data survive the exclusion rules, not participants who start the task, and the pilot audit is what converts one into the other."

mk "Track: import and validation through {catlasr}" "data,blocked-external" \
"**Tracking issue.** The pipeline is {catlasr}'s. Download, import and preprocess, with \`validate_fb_rawdata()\` passing on the full \`catlas_rawdata\` bundle (\`meta\`, \`stimuli\`, \`trials\`). No preprocessing code is written in CATLAS.

Depends on {catlasr} #4 and #7, both in progress. Worth resolving the flagged inconsistency first: \`catlas_jatos()\` constructs a \`catlas_trials\` subclass although the constructor name signals platform origin rather than data type.

What CATLAS owes here is the acceptance criterion, not the implementation: state what *validated* means for this study before the data arrive."

mk "Monitor data quality without reading the estimand" "data,register" \
"Attrition, completion time and comprehension can be watched during collection. BI₂ cannot.

Write the distinction down before the first batch launches and say what would justify stopping early. Argued afterwards, this is indistinguishable from optional stopping, however honest the intention was."

echo

# --------------------------------------------------------------------------
# 7 · Programme placeholders — undated
#
# Two workstreams the Cognitive School poster commits to publicly and which no
# issue in any repository tracks. Both are beyond `fba-allergy-online-02` and
# neither gets a date. They are here so the next roadmap inherits them as a
# record rather than as a rediscovery.
# --------------------------------------------------------------------------

echo "7 · Programme placeholders (undated)"

ms "\`programme\` · attention-model translation (Python → R)" "" \
"**Must contain**

* Nothing until \`fba-allergy-online-03\` (extension) and \`fba-[alt]-online-01\` (generalisation) are done or underway

Undated deliberately and not part of the \`fba-allergy-online-02\` roadmap.

The Cognitive School poster lists *modelling expected responses under attention shift and attention competition accounts (Python)* as a programme workstream. Paskewitz & Jones (2020) ship their EXIT dissection in Python, so this will most likely take the form of a Python → R translation.

**It will not live in CATLAS.** Models belong in {catlasr}, with anything that generalises past the following study promoted to {mathpsychr}. CATLAS carries text and science only. This milestone holds the requirement until the roadmap after next picks it up."

mk "Placeholder: attention-model translation is not scheduled" "theory,long-lead" \
"Do not start. This issue exists so the milestone is visible on the board and in issue lists rather than sitting at zero items.

Activates after \`fba-allergy-online-03\` and \`fba-[alt]-online-01\`. When it does, the first question is which parts of the translated model generalise past the next study.

Close this and replace it with real issues when the next roadmap is written."

ms "\`programme\` · eye-tracking (\`fba-[alt]-eye-01\`)" "" \
"**Must contain**

* Nothing yet — prospective, belongs to the second planned manuscript

Undated deliberately. \`fba-[alt]-eye-01\` already exists as an option on the Project \`Study\` field and the poster lists an eye-tracking study using both the original and the adjusted designs. It is two roadmaps away.

One thing is worth recording now, because it is a cross-repository contradiction rather than a scheduling question. **{mathpsychr} has parked its eye-tracking maturation (v0.2.0)** on the stated grounds that it serves *tripletviewing* and not CATLAS. That reasoning holds exactly as long as CATLAS has no eye-tracking study and it stops holding here. Nothing needs doing about it in 2026, but the parking decision should be revisited deliberately when this manuscript is scheduled, rather than discovered at that point.

Experiment code will live in catlaslab (Experiment Builder / PsychoPy per the poster, not jsPsych); gaze preprocessing in {catlasr} or {mathpsychr} per the boundary rule.

Note that this does **not** bear on {mathpsychr}'s pkgdown anchoring. The DAG vignette anchors the site because the eye-tracking vignette depends on external data, and that stays true whether or not this study ever acquires a causal graph of its own."

mk "Placeholder: eye-tracking study is not scheduled" "design,long-lead" \
"Do not start. Second planned manuscript, two roadmaps out.

Carries one live dependency worth remembering: {mathpsychr}'s eye-tracking work is parked because it was judged to serve *tripletviewing* rather than CATLAS. Scheduling this study invalidates that judgement and the lead time on eye-tracking infrastructure is long enough that the conversation should happen before the study is planned, not after.

Close this and replace it with real issues when that roadmap is written."

echo

# --------------------------------------------------------------------------
# 8 · Project #6 wiring (optional)
#
# Needs the `project` scope:  gh auth refresh -s project
# Skipped cleanly if unavailable, since the roadmap is complete without it.
# --------------------------------------------------------------------------

echo "8 · Project #${PROJECT_NUMBER} wiring"

if ! command -v jq >/dev/null 2>&1; then
  echo "  . jq not found — skipping Project wiring"
elif ! PROJECT_JSON=$(gh project view "${PROJECT_NUMBER}" --owner "${OWNER}" --format json 2>/dev/null); then
  echo "  . no project scope — skipping. Enable with: gh auth refresh -s project"
else
  PROJECT_ID=$(printf '%s' "${PROJECT_JSON}" | jq -r '.id')
  FIELDS=$(gh project field-list "${PROJECT_NUMBER}" --owner "${OWNER}" --format json --limit 100 2>/dev/null)

  fid() { printf '%s' "${FIELDS}" | jq -r --arg n "$1" '.fields[] | select(.name==$n) | .id'; }
  oid() { printf '%s' "${FIELDS}" | jq -r --arg n "$1" --arg o "$2" \
            '.fields[] | select(.name==$n) | .options[] | select(.name==$o) | .id'; }

  F_STATUS=$(fid "Status"); F_PHASE=$(fid "Phase"); F_STUDY=$(fid "Study")

  # pj <issue number> <Status> <Phase> <Study>
  pj() {
    local num="$1" status="$2" phase="$3" study="$4" item
    item=$(gh project item-add "${PROJECT_NUMBER}" --owner "${OWNER}" \
             --url "https://github.com/${REPO}/issues/${num}" --format json 2>/dev/null | jq -r '.id')
    if [ -z "${item}" ] || [ "${item}" = "null" ]; then
      echo "  . #${num} not added"
      return 0
    fi
    for pair in "${F_STATUS}:${status}:Status" "${F_PHASE}:${phase}:Phase" "${F_STUDY}:${study}:Study"; do
      local f="${pair%%:*}" rest="${pair#*:}" val="${rest%%:*}" fname="${rest#*:}" opt
      [ -z "${val}" ] && continue
      opt=$(oid "${fname}" "${val}")
      [ -n "${opt}" ] && run gh project item-edit --id "${item}" --project-id "${PROJECT_ID}" \
        --field-id "${f}" --single-select-option-id "${opt}" >/dev/null
    done
    echo "  > #${num} on board (${status} / ${phase} / ${study})"
  }

  echo "  Project ${PROJECT_ID}; fields resolved."
  echo "  New issues are not auto-added: item IDs are only knowable after creation."
  echo "  Re-run with ADD_NEW=1 once the issues above exist, or add them from the UI."

  if [ "${ADD_NEW:-0}" = "1" ]; then
    # Every issue created by this script, mapped to Phase and Study.
    # Status is left at Backlog; you move things to Ready yourself.
    gh issue list --repo "${REPO}" --state open --limit 500 --json number,title,labels \
      --jq '.[] | [.number, (.labels | map(.name) | join(","))] | @tsv' \
    | while IFS=$'\t' read -r num labels; do
        case "${labels}" in
          *theory*)     ph="theory" ;;
          *statistics*) ph="statistics" ;;
          *register*)   ph="design" ;;
          *experiment*) ph="design" ;;
          *data*)       ph="data" ;;
          *writing*)    ph="writing" ;;
          *)            ph="" ;;
        esac
        pj "${num}" "Backlog" "${ph}" ""
      done
  fi
fi

echo
echo "Done."
echo
echo "Milestones:"
gh api "repos/${REPO}/milestones?state=all&per_page=100" \
  --jq '.[] | "  \(.state == "open" | if . then "o" else "x" end)  \(.due_on[0:10] // "no date")  \(.title)  [\(.open_issues) open, \(.closed_issues) closed]"' \
  2>/dev/null | sort -k3
  