# CATLAS
## **C**onditioning **A**s a **T**heory of **L**earned **A**ttention, **S**emantics and more

This repository is the central hub of a research program on **associative
learning theories of attention,schema induction,** and possibly more. The
overarching aim is to develop and evaluate theoretical and computational
accounts of how learned associations shape attentional allocation and the
acquisition of structured knowledge. It contains the manuscripts of the program,
written as [Quarto manuscripts](https://quarto.org/docs/manuscripts/), and
publishes them as a website: <https://josefmana.github.io/CATLAS/>.

### Manuscripts

- [`forward-blocking-attention-01`](manuscripts/forward-blocking-attention-01/):
  *Disentangling Attentional Shift and Attentional Competition Accounts of
  Associative Blocking* (in preparation).

Progress is tracked in the
[issues and milestones](https://github.com/josefmana/CATLAS/milestones) of this
repository.

### Repository structure

```
.github/workflows/      # publish.yml builds and deploys the website
common/
├── R/                  # helpers shared by all manuscripts (printing, tables,
│                       # statistics, outputs, pre-registration)
├── references.bib      # one bibliography for all manuscripts
└── csl/                # citation style (APA)
manuscripts/
└── <manuscript>/
    ├── README.md       # notebooks, studies and how to render them
    ├── index.qmd       # the article
    ├── notebooks/      # analysis notebooks
    ├── R/              # functions specific to the notebooks
    ├── outputs/        # results that later notebooks read
    └── _freeze/        # frozen results of the notebooks, for the website
outputs/                # abstracts and other outputs outside the manuscripts
site/                   # the website's landing page
```

### Rendering

Requirements: R (≥ 4.3), Quarto (≥ 1.4, for the manuscript project type) and
the R packages the notebooks attach, including `{catlasr}`:

```r
remotes::install_github("josefmana/catlasr@v0.0.1")
```

The notebooks of a manuscript pass their results on to later notebooks through
the manuscript's `outputs/` folder. The aggregate results that later notebooks
need are tracked, so that those notebooks render from a fresh clone.
Participant-level data are kept in a private folder (`CATLAS_PRIVATE_DIR`)
that is not public, bur the anonymised data of the studies are shared as data
sets of `{catlasr}`. Each manuscript's `README.md` says which notebooks need
which data.

The results of every notebook are frozen, i.e., a notebook is re-executed only
when its source changes and its results are kept in the manuscript's `_freeze/`
folder, which is tracked. The website is built from these frozen results by
GitHub Actions, without R or the private data. Consequently, anytime a notebook
is changed, it needs to be rendered locally and its `.qmd` commited together
with `_freeze/`, otherwise the website build fails.

### Related repositories

- [`{catlasr}`](https://github.com/josefmana/catlasr)
  ([site](https://josefmana.github.io/catlasr/)): the R package for importing,
  validating, scoring and sharing the data of CATLAS studies.
- [`catlaslab`](https://github.com/josefmana/catlaslab): the experiments
  (jsPsych on JATOS), with every deployed build attached to a release.
- [`{mathpsychr}`](https://github.com/josefmana/mathpsychr): theory-agnostic
  mathematical psychology machinery in R.

### Inspirations

* **Paskewitz & Jones (2023)**  
  Present a model of *derived attention* based on Bayesian hierarchical
  regression, accounting for phenomena such as learned predictiveness or
  backward blocking. <https://doi.org/10.1016/j.jmp.2022.102728>
* **Le Pelley et al. (2016)**  
  Provide a comprehensive review of the evidence supporting learned
  predictiveness accounts of attention, and propose a model of *derived
  attention* based on their findings. <https://doi.org/10.1037/bul0000064>
* **Foster & Jones (2017)**  
  Introduce a model of schema induction guided by reinforcement learning.
  <https://doi.org/10.48550/arXiv.1712.10070>

### Licence

The code in this repository is licensed under the MIT licence (see
[`LICENSE`](LICENSE)). The text and figures of the manuscripts and notebooks
are licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

### Author

Josef Mana, josef.mana@psu.cas.cz, Institute of Psychology of the Czech Academy
of Sciences
([ORCID 0000-0002-7817-3978](https://orcid.org/0000-0002-7817-3978))
