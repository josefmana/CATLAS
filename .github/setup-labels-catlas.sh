#!/usr/bin/env bash
#
# setup-labels-catlas.sh
# ----------------------
# Idempotent label taxonomy for josefmana/CATLAS.
#
# CATLAS is a study and manuscript repository with an intentionally unbounded
# number of manuscripts. That is the design constraint the taxonomy has to
# survive: labels must not encode study identity, because study identity is the
# axis that grows without limit. Study identity is carried by (a) the `Study`
# single-select field on Project #6 "CATLAS Pipeline", and (b) milestone titles.
#
# The eight existing labels are kept as-is. Three are added:
#
#   long-lead         work whose constraint is latency, not effort
#   blocked-external  waiting on something outside this repository
#   decision          the deliverable is a documented choice, not an artefact
#
# Deliberately NOT added: `docs`, `testing`, `branding`. Those exist as options
# on the Project `Phase` field and adding them here would widen an already
# near-total duplication between labels and Phase for no query benefit.
#
# Usage:  bash .github/setup-labels-catlas.sh
#
# Re-runs are no-ops except for description drift.

set -uo pipefail

REPO="${REPO:-josefmana/CATLAS}"

command -v gh >/dev/null 2>&1 || { echo "gh CLI not found" >&2; exit 1; }
gh auth status >/dev/null 2>&1 || { echo "gh not authenticated" >&2; exit 1; }

echo "Repository: ${REPO}"
echo

# lbl <name> <colour> <description>
#
# Non-destructive by design: an existing label keeps its colour and only has
# its description brought into line. A missing label is created with the colour
# given here.
lbl() {
  local name="$1" colour="$2" desc="$3"
  if gh label list --repo "${REPO}" --limit 200 --json name \
       --jq '.[].name' | grep -Fxq "${name}"; then
    gh label edit "${name}" --repo "${REPO}" --description "${desc}" >/dev/null \
      && echo "  = ${name}" \
      || echo "  ! ${name} (edit failed)"
  else
    gh label create "${name}" --repo "${REPO}" --color "${colour}" --description "${desc}" >/dev/null \
      && echo "  + ${name}" \
      || echo "  ! ${name} (create failed)"
  fi
}

echo "Existing taxonomy (descriptions reconciled, colours untouched):"
lbl "theory"      "0e8a16" "Developing substantial theory"
lbl "design"      "006b75" "Document experimental designs"
lbl "experiment"  "5319e7" "Write experiment code"
lbl "data"        "1d76db" "Collect or preprocess data"
lbl "statistics"  "d93f0b" "Compute statistical estimates"
lbl "register"    "a90050" "Pre-register a study"
lbl "writing"     "fbca04" "Write manuscript"
lbl "duplicate"   "cfd3d7" "This issue or pull request already exists"

echo
echo "Added for the fba-allergy-online-02 roadmap:"
lbl "long-lead"        "c5def5" "Latency, not effort, is the constraint; start early or it lands late"
lbl "blocked-external" "e99695" "Waiting on {mathpsychr}, {catlasr}, catlaslab, Prolific, JATOS, ethics or a registry"
lbl "decision"         "0052cc" "Deliverable is a documented choice with reasons, not an artefact"

echo
echo "Done. Label set:"
gh label list --repo "${REPO}" --limit 200
