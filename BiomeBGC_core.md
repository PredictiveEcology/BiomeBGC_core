---
title: "BiomeBGC_core Manual"
subtitle: "v.0.0.0.9000"
date: "Last updated: 2026-10-05"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
link-citations: true
always_allow_html: true
---



# BiomeBGC_core Module

[![made-with-Markdown](figures/markdownBadge.png)](https://commonmark.org)

## Module Overview

Biome-BGC is an ecophysiological model that simulates the carbon, nitrogen, and
water cycles of a forest stand through time, driven by daily weather. This
module runs Biome-BGC version 4.2 through the R package
`PredictiveEcology/BiomeBGCR`, which wraps the underlying C model.

The module simulates each spatial unit twice:

- a **spinup** run, which cycles the available weather record repeatedly until
  the simulated ecosystem's slow-changing carbon and nitrogen pools (e.g., soil
  carbon) stop drifting and reach a steady state appropriate for the site's
  climate and vegetation type, and
- a **main simulation** run, which starts from that steady state and runs
  forward over the calendar years of interest, producing the daily, monthly,
  and annual outputs used downstream.

Simulations are organized by **pixelGroup**: a pixelGroup is a group of pixels
sharing the same simulation inputs (e.g., same climate, vegetation, and soil
conditions), so that Biome-BGC only needs to be run once per group rather than
once per pixel. The spinup and main `.ini` input objects (see below) are each
named lists, one entry per pixelGroup, and these names are the pixelGroup
identifiers used throughout the module's outputs.

### Module inputs and parameters

The module's two required inputs, `bbgcSpinup.ini` and `bbgc.ini`, are parsed
Biome-BGC initialization ("ini") objects — one per pixelGroup — as returned by
`BiomeBGCR::iniRead()`. These describe, among other things, the ecophysiological
constants, meteorological data, and site parameters for each pixelGroup's run.
They are typically prepared by another module (e.g., `BiomeBGC_dataPrep`), but
can also be assembled manually with `BiomeBGCR::iniRead()` provided the ini
files' other referenced inputs (ecophysiological constants file, meteorological
data, etc.) are available in the project folder pointed to by the
`bbgcInputPath` parameter.

Two further inputs, `pixelGroupParameters` and `pixelGroupMap`, are optional
and used only for plotting: `pixelGroupMap` gives the spatial extent/
resolution/projection used to turn per-pixelGroup results into a raster, and
`pixelGroupParameters` (with `pixelGroup`, `dominantSpecies`, and
`climatePolygon` columns) lets the trend plot facet or color by dominant
species and climate polygon.

Table \@ref(tab:moduleInputs-BiomeBGC-core) lists the module's inputs in full,
and Table \@ref(tab:moduleParams-BiomeBGC-core) lists its parameters. Notable
parameters include `bbgcPath` (the working directory used for the simulation's
temporary input/output files), `parallel.cores` (number of pixelGroups to run
concurrently), `saveYears` (which years to retain/write out), and
`purgeBGCdirs` (whether to delete the temporary input/output folders once a run
finishes).


Table: (\#tab:moduleInputs-BiomeBGC-core)List of BiomeBGC_core input objects and their description.

|objectName           |objectClass |desc                                                                                                                                               |sourceURL |
|:--------------------|:-----------|:--------------------------------------------------------------------------------------------------------------------------------------------------|:---------|
|bbgcSpinup.ini       |list        |Biome-BGC initialization files for the spinup. Parsed ini object as returned by `BiomeBGCR::iniRead()`, one per pixelGroup, named by pixelGroup id |NA        |
|bbgc.ini             |list        |Biome-BGC initialization files. Parsed ini object as returned by `BiomeBGCR::iniRead()`, one per pixelGroup, named by pixelGroup id                |NA        |
|pixelGroupParameters |data.frame  |Optional. A table with pixelGroup, dominantSpecies and climatePolygon columns, used only by OutputTrendPlot() for plot faceting/coloring.          |NA        |
|pixelGroupMap        |SpatRaster  |Optional. A raster defining the extent, resolution, projection of the study area. Only used for plotting purposes.                                 |NA        |


Table: (\#tab:moduleParams-BiomeBGC-core)List of BiomeBGC_core parameters and their description.

|paramName              |paramClass |default      |min |max |paramDesc                                                                                                                                                                                        |
|:----------------------|:----------|:------------|:---|:---|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|argv                   |character  |-v3          |NA  |NA  |Arguments for the BiomeBGC library (same as 'bgc' commandline application).                                                                                                                      |
|bbgcPath               |character  |/tmp/Rtm.... |NA  |NA  |Path to base directory to use for simulations.                                                                                                                                                   |
|bbgcInputPath          |character  |/tmp/Rtm.... |NA  |NA  |Path to the Biome-BGC input directory.                                                                                                                                                           |
|purgeBGCdirs           |logical    |TRUE         |NA  |NA  |If TRUE (default), delete the 'inputs' and 'outputs' subfolders underbbgcPath at the end of Init(). Set to FALSE to keep them for inspection(e.g., when using a custom, non-temporary bbgcPath). |
|returnDailyEstimates   |logical    |TRUE         |NA  |NA  |Controls whether dailyOutput object is returned by the simulation.                                                                                                                               |
|returnMonthlyEstimates |logical    |TRUE         |NA  |NA  |Controls whether monthlyAverages object is returned by the simulation.                                                                                                                           |
|parallel.cores         |integer    |1            |1   |NA  |Number of cores used to execute the simulation                                                                                                                                                   |
|saveYears              |numeric    |NA           |NA  |NA  |Controls the years for which the output variables are saved.                                                                                                                                     |
|.plots                 |character  |screen       |NA  |NA  |Used by Plots function, which can be optionally used here                                                                                                                                        |
|.plotInitialTime       |numeric    |0            |NA  |NA  |Describes the simulation time at which the first plot event should occur.                                                                                                                        |
|.plotInterval          |numeric    |NA           |NA  |NA  |Describes the simulation time interval between plot events.                                                                                                                                      |
|.saveInitialTime       |numeric    |NA           |NA  |NA  |Describes the simulation time at which the first save event should occur.                                                                                                                        |
|.saveInterval          |numeric    |NA           |NA  |NA  |This describes the simulation time interval between save events.                                                                                                                                 |
|.studyAreaName         |character  |NA           |NA  |NA  |Human-readable name for the study area used - e.g., a hash of the studyarea obtained using `reproducible::studyAreaName()`                                                                       |
|.seed                  |list       |             |NA  |NA  |Named list of seeds to use for each event (names).                                                                                                                                               |
|.useCache              |logical    |FALSE        |NA  |NA  |Should caching of events or module be used?                                                                                                                                                      |

### Events

The module schedules three events:

- **`init`** — runs `Init()`, which carries out the spinup and main simulation
  for every pixelGroup (in parallel across pixelGroups when
  `parallel.cores > 1`), and collects the daily, monthly, and annual results
  into `sim$dailyOutput`, `sim$monthlyAverages`, and `sim$annualAverages`.
  Plotting and saving events are then scheduled if requested (see below).
- **`plot`** — scheduled at the end of the simulation if `P(sim)$.plots`
  requests any plotting. Produces a time-trend plot of annual net primary
  productivity (NPP) across pixelGroups, and NPP raster maps for the first and
  last simulated years, all written to `BiomeBGC_figures` under the output
  path.
- **`save`** — scheduled at the end of the simulation if `saveYears` is set.
  Writes `sim$dailyOutput`, `sim$monthlyAverages` (if requested via
  `returnDailyEstimates`/`returnMonthlyEstimates`), and `sim$annualAverages`,
  filtered to the requested `saveYears`, to `.qs` files under the output path.

### Plotting

Plotting is controlled by the `.plots` parameter (passed to
`SpaDES.core::Plots()`) together with `.plotInitialTime`/`.plotInterval`.
When the `annualAverages` output includes a `daily_npp` column, the module
produces an NPP trend plot across years (faceted/colored by dominant species
and climate polygon when `pixelGroupParameters` is supplied) and two raster
maps of landscape NPP (first and last simulated year), all saved as PNG files.

### Saving

Saving is controlled by the `saveYears` parameter together with
`.saveInitialTime`/`.saveInterval`. When `saveYears` contains any non-`NA`
year, the `save` event writes the daily, monthly, and annual outputs
(restricted to those years) to `.qs` files via `qs2::qs_save()`. Whether daily
and monthly estimates are written (and computed at all) is further controlled
by `returnDailyEstimates` and `returnMonthlyEstimates`.

### Module outputs

The module returns three `data.table` outputs, one row per pixelGroup and time
step: `dailyOutput` (daily values), `monthlyAverages` (daily values averaged by
month), and `annualAverages` (daily values averaged by year). Units for each
output variable follow Biome-BGC's own output variable definitions (see
[`bgc_struct.h`](https://raw.githubusercontent.com/PredictiveEcology/BiomeBGCR/refs/heads/development/src/Biome-BGC/src/include/bgc_struct.h)
in the `BiomeBGCR` source).


Table: (\#tab:moduleOutputs-BiomeBGC-core)List of BiomeBGC_core outputs and their description.

|objectName      |objectClass |desc                                                                                                                                                                                                     |
|:---------------|:-----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|dailyOutput     |data.table  |The ouput variables for each pixelGroup and day. The units can be find here: https://raw.githubusercontent.com/PredictiveEcology/BiomeBGCR/refs/heads/development/src/Biome-BGC/src/include/bgc_struct.h |
|monthlyAverages |data.table  |The daily output variables averaged for each month. The same units than the dailyOutput.                                                                                                                 |
|annualAverages  |data.table  |The daily output variables averaged for each month. The same units than the dailyOutput.                                                                                                                 |

### Links to other modules

- `BiomeBGC_dataPrep` (upstream) — prepares the `bbgcSpinup.ini`/`bbgc.ini`
  input objects (and optionally `pixelGroupParameters`/`pixelGroupMap`) that
  this module consumes.
- `BiomeBGC_validationFluxTower` (downstream) — validates this module's daily/
  monthly/annual outputs against flux tower observations.

### Getting help

- Please file an issue on the module's
  [GitHub repository](https://github.com/PredictiveEcology/BiomeBGC_core/issues)
  if you run into problems or have questions.

## References

<!-- autogenerated from bibliography -->

<!-- Drafted with assistance from Claude (Posit Assistant). -->
