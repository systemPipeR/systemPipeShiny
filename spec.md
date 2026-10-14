# systemPipeShiny — Product Spec

Precedence: `spec.md` > sub-spec > `Plans.md`. This file defines what is correct; `Plans.md` tracks what to do.

## Purpose

systemPipeShiny (SPS) is a Bioconductor R package that provides a Shiny GUI on top of
systemPipeR (SPR). Users who don't know R can create, configure, and run SPR workflows,
visualize the results, and build their own Shiny tabs without writing Shiny code.

## Distribution

- Released through Bioconductor. The canonical source is `https://git.bioconductor.org/packages/systemPipeShiny`
  (git remote `upstream`); the GitHub mirror is `systemPipeR/systemPipeShiny` (git remote `origin`).
- Branch `devel` tracks Bioconductor devel. Bioconductor's build system pushes version bumps
  (`x.y.z`, with even `y` for release and odd `y` for devel), so pull from `upstream` before starting work.
- The package must pass `R CMD build` and `R CMD check` (including BiocCheck) with no ERRORs or WARNINGs.
- AI, agent, and planning files must never be part of the built tarball (see `.Rbuildignore`).

## Public API (contract)

The exported functions in `NAMESPACE` are the public contract. Changing a function's signature or behavior counts as a breaking change and needs a `NEWS.md` entry.

| Area | Exports |
|------|---------|
| App lifecycle | `spsInit()` creates an app project from `inst/app`; `sps()` builds the UI/server |
| Options | `spsOptions()`, `spsOptDefaults()` |
| Tabs | `spsNewTab()`, `spsEzUI()`, `spsEzServer()`, `spsTabInfo()`, `removeSpsTab()`, `spsCoreTabReplace()` |
| Data/UI helpers | `dynamicFile()`, `dynamicFileServer()`, `loadDF()`, `genGallery()`, `genHrefTable()`, `canvasBtn()` |
| Accounts / DB (R6) | `spsDb`, `spsEncryption`, `spsAccount` (SQLite-backed, `openssl` encryption) |

## Functional modules

1. **Workflow**: target/metadata editor, workflow step selection, CWL editing, running workflows via systemPipeR.
2. **Visualization (RNA-seq)**: normalization, DEG (DESeq2), and plots (PCA, t-SNE, GLM-PCA, heatmap, UpSet, dendrogram).
3. **Quick ggplot**: esquisse-based plotting.
4. **Canvas**: collect, arrange, and export plots across tabs.
5. **Admin and login**: optional user/admin pages, backed by `spsAccount`.
6. **Custom tabs**: templates in `inst/app/templates`, registered through `config/tabs.csv`.

## Invariants

- Heavy and module-specific packages (systemPipeR, DESeq2, esquisse, …) stay in `Suggests`, and
  modules check for them at runtime (`checkModulePkgs_internal`). They must not move into `Imports`.
- The app project scaffold (`inst/app`) and `config/sps_options.yaml` must stay in sync with `spsOptDefaults()`.
- Tests under `tests/testthat` must skip, not fail, when the session is non-interactive and an interactive dependency is missing.
- Credentials and DB keys are never committed. The SQLite DB is created at `spsInit()` time.
- R code in `R/` never references raw files on external hosting platforms (`raw.githubusercontent.com`, GitLab, Bitbucket, Dropbox). BiocCheck treats these as an ERROR. Ship the asset in `inst/` instead, or read the installed copy (e.g. `system.file("NEWS.md", package = "systemPipeShiny")`).

## Quality gates

- `R CMD build . && R CMD check --as-cran systemPipeShiny_*.tar.gz` passes with 0 ERRORs and 0 WARNINGs.
- `BiocCheck::BiocCheck()` reports no ERRORs.
- `testthat` passes.
- No lint/formatter baseline is configured yet. Any plan that changes R code should first add a `lintr` setup task.
