# `forward-blocking-attention-01`

*Disentangling Attentional Shift and Attentional Competition Accounts of
Associative Blocking: Replication and Extension of Beesley & Le Pelley
([2011](https://doi.org/10.1037/a0019526))*

Status: in preparation. The pre-registration of Experiment 1 is written and
awaits submission. The rendered manuscript and notebooks are available at
<https://josefmana.github.io/CATLAS/forward-blocking-attention-01/>.

### Notebooks

| Notebook | Content |
|:---|:---|
| `01-estimands-estimates.qmd` | Dependent variables: blocking indices and existing estimates |
| `02-feasibility-study.qmd` | The pilot `fba-allergy-online-01` |
| `03-replication-power.qmd` | Power analysis and group-sequential design of Experiment 1 |
| `A1-replication-prereg.qmd` | Pre-registration of Experiment 1 (`fba-allergy-online-02`) in the PRP-QUANT template |

The functions of notebooks `02` and `03` live in `R/`, helpers shared with
other manuscripts in `../../common/R/`.

### Studies

| Study | Role | Status |
|:---|:---|:---|
| `fba-allergy-online-01` | Pilot | Completed |
| `fba-allergy-online-02` | Experiment 1: pre-registered replication | Data collection pending |
| `fba-allergy-online-03` | Experiment 2: extension | Planned |
| `fba-[alt]-online-01` | Experiment 3: generalisation | Planned |

The experiment code lives in
[`catlaslab`](https://github.com/josefmana/catlaslab), in
`studies/forward-blocking-attention-01/fba-allergy-online/`. Every deployed
build is attached to the release of the same name. The data are imported and
scored with [`{catlasr}`](https://github.com/josefmana/catlasr).

### Rendering

Render the notebooks in order (or from `index.qmd`), since each reads what the
previous ones saved to `outputs/`:

| Notebook | Reads | Saves to `outputs/` |
|:---|:---|:---|
| `01` | nothing | nothing |
| `02` | pilot data (private folder) | `02-feasibility-estimates.rds`, `02-feasibility-funnel.rds`, `02-feasibility-rules.rds` |
| `03` | `02-feasibility-estimates.rds`, `02-feasibility-funnel.rds` | `03-replication-design.rds` |
| `A1` | `02-feasibility-*.rds`, `03-replication-design.rds` | `A1-replication-prereg.pdf`, `A1-replication-prereg-zpid.rds`, `A1-replication-rules.rds` |

The four aggregate files `02-feasibility-estimates.rds`,
`02-feasibility-funnel.rds`, `02-feasibility-rules.rds` (the wording of the
pilot's exclusion rules) and `03-replication-design.rds` are tracked (see
`.gitignore`), so notebooks `01`, `03` and `A1` render from a fresh clone.
Only notebook `02` reads the pilot data, from the private folder
(`CATLAS_PRIVATE_DIR`), which holds participant-level data and is thzs not
public.

Results are frozen (`freeze: auto` in `_quarto.yml`). A notebook is
re-executed only when its source changes and its results are kept in
`_freeze/`, which is tracked and from which the website is built. After
changing a notebook, render it and commit it together with `_freeze/`.

The exports of `A1` are, in turn: the registration as a PDF; its state file
for the [PreReg](https://prereg-psych.org/) web form, from which it is
submitted; and the wording of the exclusion rules, against which the data set
of `fba-allergy-online-02` is checked.
