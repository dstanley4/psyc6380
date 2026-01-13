# Plan: Updating workflow.qmd and qualtrics.qmd

## Implementation Status: COMPLETED (2025-01-13)

This document was originally a plan. It now reflects what was actually implemented.

**Git checkpoint created:** Tag `pre-qmd-updates` allows reverting all changes if needed.

---

## Pedagogical Goal

Build students' understanding progressively across three stages:

1. **workflow.qmd** - Introduce core data preparation principles using a single script with simple data ✅ COMPLETED
2. **qualtrics.qmd** - Apply and extend those principles to real-world Qualtrics survey data ✅ COMPLETED
3. **Future lecture** - Break the single script into modular files (my-workflow-project structure) 📋 PLANNED

---

## Stage 1: workflow.qmd (Foundations) ✅ COMPLETED

### Changes Made

| Concept | Status | What Was Done |
|---------|--------|---------------|
| Simplified packages | ✅ Done | Removed apaTables, Hmisc, psych; kept tidyverse, janitor, skimr |
| Missing value codes in import | ✅ Done | Added `na = c("", "NA", "999")` parameter |
| Creating participant_id | ✅ Done | Added section using `row_number()` and `relocate()` |
| Factor safety check | ✅ Done | Added `setdiff()` pattern with warning |
| Directory structure preview | ✅ Done | Added new section introducing data-raw/, data-processed/, output/ |
| Reverse coding callout | ✅ Done | Added callout noting data is already numeric |
| Saving processed data | ✅ Done | Added `write_rds()` section with explanation |
| Function explanations | ✅ Done | Added explanations for `row_number()`, `relocate()`, `setdiff()` |

### What Was NOT Included (as planned)
- Qualtrics-specific import (skip rows, remove metadata columns)
- Likert text-to-number conversion
- Missing data evaluation with naniar
- Exclusion criteria
- Modular script structure

---

## Stage 2: qualtrics.qmd (Applied to Qualtrics) ✅ COMPLETED

### Changes Made

| Concept | Status | What Was Done |
|---------|--------|---------------|
| Transition text | ✅ Done | Added paragraph connecting to workflow chapter |
| `na` parameter in read_csv | ✅ Done | Added `na = c("", "NA", "999")` |
| Simplified participant_id | ✅ Done | Replaced complex approach with `row_number()` + `relocate()` |
| Factor safety check | ✅ Done | Added `setdiff()` pattern (with reference to workflow chapter) |
| Likert conversion | ✅ Done | Streamlined to named vector approach only |
| Exclusion tracking | ✅ Done | Added `nrow()` and `message()` for tracking dropped participants |
| Save step | ✅ Done | Added `write_rds()` at end |
| Complete Script | ✅ Done | Updated to reflect all changes |
| Modular preview | ✅ Done | Added "Looking ahead" section at end |

### Deviation from Original Plan

| Original Plan | Actual Implementation | Reason |
|--------------|----------------------|--------|
| Add naniar for missing data | ❌ NOT ADDED | Deferred to future lecture per user request |

**Missing data evaluation with naniar will be introduced in the future lecture about modular scripts**, keeping qualtrics.qmd focused on Qualtrics-specific challenges.

### Building on workflow.qmd (Final State)

| Concept | workflow.qmd | qualtrics.qmd |
|---------|-------------|---------------|
| Import | Basic `read_csv()` | Skip Qualtrics header rows, remove metadata columns |
| Missing codes | `na = c(...)` | Same, reinforced |
| participant_id | `row_number()` | Same pattern, applied to Qualtrics data |
| Factors | Character to factor | Same, plus Qualtrics quirks |
| Factor safety | `setdiff()` check | Same pattern (references workflow chapter) |
| Likert data | Already numeric | **NEW**: Text-to-number conversion with named vector |
| Reverse coding | Numeric formula | Same formula, applied after text conversion |
| Scale creation | `rowwise()` method | Same pattern |
| Missing data | Not covered | **DEFERRED** to future lecture |
| Exclusions | Not covered | **NEW**: Speed checks with tracking |
| Saving data | `write_rds()` | Same pattern |

---

## Stage 3: Future Lecture (Modular Scripts) 📋 PLANNED

### Concepts to Introduce
- Master script pattern (`00-script-master.R`)
- Numbered script naming convention (01-, 02-, etc.)
- Interim saves between steps (`.rds` files)
- Full directory structure: `data-raw/`, `data-interim/`, `data-processed/`, `output/`
- **Missing data evaluation with naniar** (moved from qualtrics.qmd)
- Benefits: reproducibility, debugging, collaboration

### Structure Based on my-workflow-project
```
project/
├── 00-script-master.R      # Runs all scripts in order
├── 01-import.R             # Load and anonymize
├── 02-clean-recode.R       # Factors, Likert conversion
├── 03-missing-data.R       # Evaluate missingness (naniar introduced here)
├── 04-create-scales.R      # Compute scale scores
├── 05-exclusions.R         # Apply exclusion criteria
├── 06-analysis-wrapper.R   # Capture analysis output
├── 07-analysis.R           # Actual analyses
├── data-raw/               # Original untouched data
├── data-interim/           # Between-step saves
├── data-processed/         # Final analytic data
└── output/                 # Tables, figures, reports
```

### Transition from Single Script
Show how the single script from qualtrics.qmd maps to the modular files:
- Import section → `01-import.R`
- Factor/Likert section → `02-clean-recode.R`
- Missing data section → `03-missing-data.R` (introduces naniar)
- Scale creation → `04-create-scales.R`
- Exclusions → `05-exclusions.R`
- Analysis → `07-analysis.R`

---

## Summary: Learning Progression (Final)

```
workflow.qmd                    qualtrics.qmd                   Future Lecture
─────────────────────────────────────────────────────────────────────────────────
Core principles                 Apply to Qualtrics              Modular structure
Simple CSV data                 Real survey data                Same data
Numeric Likert items            Text → Number conversion        Same conversion
Basic import                    Skip rows, remove columns       Separate import script
Factor basics                   Same + Qualtrics quirks         Same
participant_id creation         Same pattern                    Same
Reverse coding (numeric)        Same (after conversion)         Same
Scale creation                  Same pattern                    Separate script
---                            ---                              Missing data (naniar)
---                            Exclusions (speeders)            Separate script
Single script                   Single script                   7+ scripts
write_rds() intro              write_rds() reinforced          Interim saves
Directory preview              Directory reinforced             Full structure
```

---

## Additional Files Updated

- **script_qualtrics.R** - Updated to match Complete Script in qualtrics.qmd
- **_quarto.yml** - Removed twoway-reg-anova.qmd (had render error unrelated to this work)

---

## Git History

1. **Tag: `pre-qmd-updates`** - Checkpoint before any changes
2. **Commit: workflow.qmd updates** - All Stage 1 changes
3. **Commit: qualtrics.qmd updates** - All Stage 2 changes
4. **Commit: script_qualtrics.R sync** - Standalone script matches qmd
5. **Pushed to GitHub**

---

## Files Reference

- `my-workflow-project/00-script-master.R` through `07-analysis.R` - Templates for future lecture
- `workflow.qmd` - Updated with foundations
- `qualtrics.qmd` - Updated with Qualtrics application
- `script_qualtrics.R` - Standalone script matching qualtrics.qmd
- `data_item_scoring.csv` - workflow.qmd data
- `data_qualtrics.csv` - qualtrics.qmd data
