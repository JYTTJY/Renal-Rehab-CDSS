# Renal Rehab CDSS

**A clinical decision support tool for exercise safety screening and prescription in chronic kidney disease (CKD), built with R Shiny and aligned with the UK Kidney Association (UKKA) exercise guideline.**

[![R](https://img.shields.io/badge/R-%E2%89%A5%204.0-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/built%20with-Shiny-blue)](https://shiny.posit.co/)
![Status](https://img.shields.io/badge/status-prototype-orange)
![Use](https://img.shields.io/badge/use-clinical%20decision%20support%20only-lightgrey)

> **In one sentence:** a physiotherapist enters a patient's vitals, blood results and symptoms, and the app answers three questions in seconds: *Is it safe to exercise today? What should I do next if not? What exercise prescription fits this patient?* It then produces a printable clinical record.

> **Live demo:** [Launch Interactive App (shinyapps.io)](https://greencare-physio.shinyapps.io/Renal_Rehab_CDSS/)

---

## Screenshots

**Pre-test safety screening and FITT**
<img width="939" height="440" alt="Pre-test safety screening and FITT" src="https://github.com/user-attachments/assets/561abbd8-e3cd-47e9-8e3b-ad843cbf3ae2" />

**Post-exercise discharge check**
<img width="935" height="434" alt="Post-exercise discharge check" src="https://github.com/user-attachments/assets/1714745d-5ca5-42ad-b7e0-2dc990d05de2" />

**Longitudinal trends**
<img width="930" height="439" alt="Longitudinal trends" src="https://github.com/user-attachments/assets/d61eb3ba-fc67-464f-bd4a-124982eada43" />

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

Please read the [limitations and disclaimer](#limitations-and-disclaimer) before any clinical use.

## Technical stack

- **Language:** R (4.0 or later; developed on 4.3)
- **Framework:** [Shiny](https://shiny.posit.co/) with `shinydashboard` and `shinyBS`
- **Packages:** `shiny`, `shinydashboard`, `shinyBS`, `plotly` (charts), `DT` (tables)
- **Design:** clinical rules are separated from the interface.
  - `logic.R`: all medical logic (thresholds, status rules, FITT, post-exercise rules, trend analysis). It has no UI code, so it can be unit-tested and reviewed by clinicians on its own.
  - `app.R`: user interface, input validation, data storage and the printable report.
- **Storage:** plain local CSV files; no external database or network calls.

```text
.
├── app.R              # Shiny UI, server, validation, report
├── logic.R            # Clinical rules and thresholds (with a source table)
├── www/
│   └── logo.png       # Optional letterhead logo for the printed report
├── data/              # Created at runtime (see "Data safety and privacy")
└── CKD_CDSS_Project.Rproj
