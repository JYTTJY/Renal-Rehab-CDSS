# Renal Rehab CDSS

**A clinical decision support tool for exercise safety screening and prescription in chronic kidney disease (CKD), built with R Shiny and aligned with the UK Kidney Association (UKKA) exercise guideline.**

[![R](https://img.shields.io/badge/R-%E2%89%A5%204.0-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/built%20with-Shiny-blue)](https://shiny.posit.co/)
![Status](https://img.shields.io/badge/status-prototype-orange)
![Use](https://img.shields.io/badge/use-clinical%20decision%20support%20only-lightgrey)

> **In one sentence:** a physiotherapist enters a patient's vitals, blood results and symptoms, and the app answers three questions in seconds: *Is it safe to exercise today? What should I do next if not? What exercise prescription fits this patient?* It then produces a printable clinical record.


> **Live demo:** [(https://greencare-physio.shinyapps.io/Renal_Rehab_CDSS/)](https://greencare-physio.shinyapps.io/Renal_Rehab_CDSS/)

---

## Screenshots
Pre-test safety screening and FITT 
Post-exercise discharge check 
Longitudinal trends 

## Why it matters

Exercise is one of the most effective treatments for people with CKD, yet clinicians in a busy rehabilitation clinic rarely have time to cross-check every patient against a long guideline. The risks are real: a patient on dialysis may have a potassium that makes exercise unsafe, a blood pressure above the guideline limit, or a fever that nobody has recorded.

This tool is designed around the way a physiotherapist actually works:

- **One screen, one decision.** Vitals and blood results go in; a single status comes out: **Safe**, **Caution** (go ahead with modifications) or **Contraindicated** (do not exercise today).
- **"Not safe" is never a dead end.** Every stop reason comes with *what to do, when to re-test, and whether to notify a doctor*, so the next step is clear.
- **Patient safety is built into the workflow, not added on top.**
  - No pre-filled "normal" values: every vital sign must be entered, so a forgotten field cannot silently become "safe".
  - Changing the MRN invalidates the previous result, so one patient's result cannot be applied to the next.
  - Every evaluation, re-test and handling decision (who was informed, what was decided) is logged and printed in the report.
- **Evidence is visible.** An in-app panel lists every threshold with its source, so a clinician can check any rule in seconds.

## Features

| Area | What it does |
|---|---|
| **Pre-exercise screening** | Checks BP, heart rate, SpO2, temperature, glucose, potassium, haemoglobin, red-flag symptoms and sub-population rules (haemodialysis, peritoneal dialysis, transplant, pre-transplant, non-dialysis CKD). |
| **Three-tier status** | `SAFE`, `CAUTION` (proceed with the listed precautions), `CONTRAINDICATED` (with reasons). |
| **Next steps for each stop reason** | Action, re-test timing and notification guidance (draft content for medical-team approval). |
| **Re-test and evaluation log** | Same-day re-tests are kept in an audit log; one click clears only the vital signs for re-entry. |
| **Handling record** | Records who was informed, when, the decision and any rescheduling. |
| **FITT prescription** | Frequency, intensity (Borg RPE), time and type, adapted to the patient group and a clinician-chosen starting level. |
| **Post-exercise discharge check** | Flags post-exercise BP, SpO2, heart rate and symptoms, and suggests the next-session adjustment. |
| **Trends** | eGFR (CKD-EPI 2021, race-free) and BP trends across sessions, with a possible-AKI alert. |
| **Clinical report** | A4 printable report with letterhead, identity and page number on every page, evaluation log and handling record. |
| **Guidelines and thresholds panel** | Every rule, its basis (guideline, standard definition or clinician decision) and its source. |

## Clinical and evidence basis

The decision rules in `logic.R` follow published guidance. Where no international standard exists, the app does **not** invent a number: the item is shown for information, switched off, or left to the clinician.

| Topic | Source |
|---|---|
| Exercise in CKD, haemodialysis safety, intensity (RPE 12-14, not above 15), timing, contraindications | UKKA / Renal Association, *Clinical practice guideline: exercise and lifestyle in chronic kidney disease* (Baker et al., BMC Nephrology 2022; [doi:10.1186/s12882-021-02618-1](https://doi.org/10.1186/s12882-021-02618-1)) |
| Resting BP and heart-rate contraindications | ACSM / AACVPR exercise contraindication lists, as cited in the UKKA guideline |
| Glucose before and during exercise | ADA Standards of Care; ACSM |
| Potassium severity classes | UKKA clinical practice guideline on hyperkalaemia (2023) |
| Severe anaemia, hypoxaemia | WHO definitions |
| Peritoneal dialysis (not covered by UKKA 2021) | ISPD / Global Renal Exercise Network practice points (2022), expert consensus |
| eGFR | CKD-EPI 2021 equation (race-free) |
| Functional norms | CDC STEADI chair-stand norms; EWGSOP2 handgrip cut-offs |

Please read the [limitations](#limitations-and-disclaimer) before any clinical use.

## Technical stack

- **Language:** R (4.0 or later; developed on 4.3)
- **Framework:** [Shiny](https://shiny.posit.co/) with `shinydashboard` and `shinyBS`
- **Packages:** `shiny`, `shinydashboard`, `shinyBS`, `plotly` (charts), `DT` (tables)
- **Design:** clinical rules are separated from the interface.
  - `logic.R`: all medical logic (thresholds, status rules, FITT, post-exercise rules, trend analysis). It has no UI code, so it can be unit-tested and reviewed by clinicians on its own.
  - `app.R`: user interface, input validation, data storage and the printable report.
- **Storage:** plain local CSV files; no external database or network calls.

```
.
├── app.R              # Shiny UI, server, validation, report
├── logic.R            # Clinical rules and thresholds (with a source table)
├── www/
│   └── logo.png       # Optional letterhead logo for the printed report
├── data/              # Created at runtime (see "Data safety and privacy")
└── CKD_CDSS_Project.Rproj
```

## Data safety and privacy

- **No real patient data is shipped with this repository.** The trend page shows clearly labelled **DEMO DATA** (simulated) until real sessions are saved for a given MRN.
- **Minimum data stored.** Local CSV files in `data/` hold the MRN, clinical values and decisions. **Patient name and IC / passport number are never written to disk**; they appear only on screen and in the printed report.
- **No external services.** The app does not send data anywhere.
- **Do not enter real patient data into a public demo.** A public host such as shinyapps.io is not a hospital-approved system. Use simulated patients for demonstrations. For clinical use, deploy only inside your organisation's approved environment and follow local information-governance rules.
- Add `data/` to your `.gitignore` so that saved records are never committed:

```gitignore
data/
.Rproj.user/
.Rhistory
```

## Quick start

**Online:** open the [live demo](https://YOUR-ACCOUNT.shinyapps.io/YOUR-APP/). No installation needed.

**Locally:**

```bash
# 1. Clone
git clone https://github.com/YOUR-USERNAME/YOUR-REPO.git
cd YOUR-REPO
```

```r
# 2. Install dependencies (once)
install.packages(c("shiny", "shinydashboard", "shinyBS", "plotly", "DT"))

# 3. Run (from the project folder)
shiny::runApp()
```

Or open `CKD_CDSS_Project.Rproj` in RStudio, open `app.R` and click **Run App**.

## Disclaimer

- **Decision support only.** This tool does not replace clinical judgement or local protocols, and it is **not a certified medical device**.
- **Prototype.** It has not been clinically validated, and a formal clinical safety assessment has not been carried out.
- **Draft content needs approval.** The "next steps" guidance for stop reasons is draft content that must be reviewed and approved by your nephrology / medical team before clinical use.
- **Clinician decisions are flagged.** Some actions (for example holding exercise at potassium of 6.0 mmol/L or above, or haemoglobin below 8 g/dL) rest on severity classifications rather than an exercise-specific guideline value. They are marked as clinician decisions in the in-app source table.
- **Guideline currency.** The UKKA exercise and lifestyle guideline (final version April 2021) lists a review date of April 2026. Check for a newer version and re-verify the thresholds before relying on them.
- **Peritoneal dialysis** advice follows expert consensus (ISPD / GREX 2022), not UKKA, and should be checked against the source document and local practice.


## Acknowledgements

Built on the UKKA / Renal Association exercise guideline authors' work (Baker LA, March DS, Wilkinson TJ, et al.), and on the ACSM, ADA, WHO, ISPD and GREX resources cited above.
