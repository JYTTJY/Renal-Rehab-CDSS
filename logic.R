# ==============================================================================
# Clinical Decision Support System (CDSS) Logic Module (logic.R)
# Aligned with the UK Kidney Association / Renal Association Exercise & Lifestyle in CKD guideline (2021)
# v2: three-tier status (SAFE / CAUTION / CONTRAINDICATED), NA-safe inputs,
#     shared thresholds, adaptive RPE targets, age/sex-adjusted functional tiers
# ==============================================================================

# --- 0. Shared thresholds & helpers (single source of truth for logic + report) ---
# Every rule below is taken from an international guideline or a standard clinical definition.
# Items that NO guideline defines are NOT assumed: they are shown for information only, or left
# switched off (NA) until your unit sets its own policy.  See THR_SOURCES for the full list.
THRESHOLDS_VERSION <- "2026-11"
THR <- list(
  sbp_stop = 200, dbp_stop = 110,     # UKKA 2021 / ACSM / AACVPR: resting SBP > 200 or DBP > 110 -> do not exercise
  sbp_low = 90,                       # hypotension definition (SBP < 90); symptomatic hypotension = stop (UKKA)
  hr_stop = 120, hr_caution = 100,    # ACSM / AACVPR: uncontrolled sinus tachycardia > 120; tachycardia > 100 (AHA)
  spo2_low = 90,                      # WHO definition of hypoxaemia (< 90%)
  temp_fever = 38.0,                  # UKKA: avoid exercise with infection / fever; fever >= 38.0 C (NICE)
  glu_low = 3.9, glu_target_low = 5.0, glu_high = 13.9, glu_very_high = 19.4,   # ADA / ACSM (70, 90, 250, 350 mg/dL)
  k_stop = 6.0, k_caution = 5.5, k_low = 3.5,   # UKKA hyperkalaemia guideline 2023 severity classes; hypokalaemia < 3.5
  hb_low = 8.0,                       # WHO definition of severe anaemia (adults)
  dial_early_hr = 0.5,                # UKKA 2021: avoid the first 30 minutes of haemodialysis
  post_hr_high = 120,                 # same ACSM / AACVPR tachycardia criterion applied before discharge
  # No international standard exists for how old a blood result may be: OFF unless your unit sets a limit
  lab_stale_days_dialysis = NA, lab_stale_days_other = NA
)

# Where every rule comes from (shown in the app: Guidelines & thresholds)
THR_SOURCES <- list(
  list(item = "Resting BP", rule = "Stop if SBP > 200 or DBP > 110 mmHg", basis = "Guideline",
       source = "UKKA 2021 exercise guideline (haemodialysis safety section, citing AHA / ACSM); ACSM / AACVPR contraindication lists"),
  list(item = "Symptomatic BP", rule = "Stop if symptomatic hypertension or hypotension (e.g. headache, visual disturbance, light-headedness)", basis = "Guideline",
       source = "UKKA 2021 exercise guideline"),
  list(item = "Low SBP", rule = "Caution if SBP < 90 mmHg (stop if symptomatic)", basis = "Definition + guideline",
       source = "Standard hypotension definition (SBP < 90); symptomatic hypotension: UKKA 2021"),
  list(item = "Resting heart rate", rule = "Stop if > 120 bpm; caution if > 100 bpm", basis = "Guideline",
       source = "ACSM / AACVPR cardiac-rehabilitation contraindication lists (uncontrolled sinus tachycardia > 120); tachycardia > 100 (AHA)"),
  list(item = "SpO2", rule = "Stop if < 90%", basis = "Definition",
       source = "WHO definition of hypoxaemia"),
  list(item = "Temperature", rule = "Stop if >= 38.0 C, or infection", basis = "Guideline",
       source = "UKKA 2021 (avoid exercise with infection / fever); fever >= 38 C (NICE)"),
  list(item = "Glucose", rule = "Stop if < 3.9 mmol/L; on insulin / sulfonylurea and < 5.0: take carbohydrate first; > 13.9: check ketones; >= 19.4: test ketones before any exercise", basis = "Guideline",
       source = "ADA Standards of Care / ACSM (70, 90, 250, 350 mg/dL)"),
  list(item = "Potassium", rule = "Stop if >= 6.0 mmol/L; caution 5.5-5.9 or < 3.5", basis = "Classification + clinician decision",
       source = "UKKA hyperkalaemia guideline 2023: mild 5.5-5.9, moderate 6.0-6.4, severe >= 6.5. No exercise guideline gives a potassium cut-off, so holding exercise at moderate / severe hyperkalaemia is a clinician decision"),
  list(item = "Haemoglobin", rule = "Stop (medical clearance) if < 8.0 g/dL", basis = "Definition + clinician decision",
       source = "WHO definition of severe anaemia (< 8.0 g/dL). No exercise guideline gives a haemoglobin cut-off, so holding exercise is a clinician decision"),
  list(item = "Haemodialysis vintage", rule = "Stop if haemodialysis started < 3 months ago", basis = "Guideline", source = "UKKA 2021 exercise guideline"),
  list(item = "Unstable conditions", rule = "Stop: MI within 2 weeks, undiagnosed chest pain, clinically unstable condition, infection / fever", basis = "Guideline", source = "UKKA 2021 exercise guideline"),
  list(item = "Intradialytic timing", rule = "Caution in the first 30 minutes of dialysis; no restriction in the second half", basis = "Guideline", source = "UKKA 2021 exercise guideline"),
  list(item = "Exercise intensity", rule = "Haemodialysis: moderate RPE 12-14, never above 15. Others: moderate RPE 12-13. Light start: RPE 9-11", basis = "Guideline",
       source = "UKKA 2021 (RPE 12-14, not above 15); ACSM Borg classification (light 9-11, moderate 12-13)"),
  list(item = "Sit-to-stand / handgrip", rule = "30s sit-to-stand vs age / sex norm; handgrip < 27 kg (men) / < 16 kg (women) = low strength", basis = "Guideline",
       source = "CDC STEADI chair-stand norms; EWGSOP2 sarcopenia cut-offs"),
  list(item = "Vascular access / transplant / PD status", rule = "Follow the vascular access, transplant or PD team's restrictions (no time limits are assumed)", basis = "Clinician decision",
       source = "UKKA 2021 (no time limit defined); ISPD / GREX 2022 practice points (PD)"),
  list(item = "Blood-result age", rule = "Not enforced (shown for information)", basis = "No standard",
       source = "No international standard; set THR$lab_stale_days_* if your unit has a policy"),
  list(item = "Interdialytic weight gain", rule = "Shown for information only (no flag)", basis = "No standard",
       source = "No exercise guideline defines an IDWG cut-off; assess breathlessness, oedema and BP instead"),
  list(item = "After exercise", rule = "BP > 200 / 110 or SBP < 90, SpO2 < 90%: medical review. HR > 120: observe. Symptoms: dizziness / breathlessness = medical review", basis = "Guideline + clinician decision",
       source = "Same resting criteria as above applied before discharge; discharge handling is a clinician decision")
)

# TRUE only for a single, non-missing number
num_ok <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)

# Creatinine unit conversion (mg/dL <-> umol/L)
scr_to_mgdl <- function(value, unit = "mgdl") {
  if (!num_ok(value)) return(NA_real_)
  if (identical(unit, "umol")) value / 88.4 else value
}

# eGFR is not valid for patients on dialysis
egfr_applicable <- function(ckd_category) {
  !(ckd_category %in% c("dialysis", "peritoneal"))
}

# --- 0b. "What to do next" guidance for every STOP reason --------------------------
# DRAFT content: must be reviewed and approved by the nephrology / medical team
# before clinical use. Edit this table to match local protocols.
NEXT_STEPS_STATUS <- "DRAFT guidance - to be reviewed and approved by the nephrology / medical team before clinical use."

NEXT_STEPS <- list(
  sbp_high = list(
    actions = c("Withhold exercise.",
                "Seat the patient quietly for at least 5 minutes, then repeat the BP (correct cuff size, non-access arm).",
                "Ask about headache, chest pain, visual change and missed antihypertensive doses."),
    retest = "After at least 5 minutes of seated rest (standard BP measurement procedure).",
    notify = "Inform the medical officer if SBP is still > 200 mmHg on repeat, or at once if the patient is symptomatic.",
    notify_level = "conditional"),
  dbp_high = list(
    actions = c("Withhold exercise.",
                "Seat the patient quietly for at least 5 minutes, then repeat the BP (correct cuff size, non-access arm).",
                "Ask about headache, chest pain, visual change and missed antihypertensive doses."),
    retest = "After at least 5 minutes of seated rest (standard BP measurement procedure).",
    notify = "Inform the medical officer if DBP is still > 110 mmHg on repeat, or at once if the patient is symptomatic.",
    notify_level = "conditional"),
  hr_abn = list(
    actions = c("Withhold exercise. Seat the patient and rest for at least 5 minutes.",
                "Check the pulse manually (regular / irregular); ask about palpitations, dizziness and chest pain.",
                "Check temperature, recent medication (e.g. beta-blockers) and caffeine / stimulant intake."),
    retest = "After at least 5 minutes of seated rest.",
    notify = "Inform the medical officer if the resting heart rate stays above 120 bpm, the rhythm is irregular, or the patient is symptomatic.",
    notify_level = "conditional"),
  spo2_low = list(
    actions = c("Withhold exercise. Sit the patient upright.",
                "Check probe position, warm cold hands and re-measure.",
                "Assess breathing, cough and leg swelling; consider fluid overload (dialysis patients)."),
    retest = "After 5 minutes seated upright.",
    notify = "Inform the medical officer if SpO2 stays below 90% or the patient is breathless.",
    notify_level = "conditional"),
  fever = list(
    actions = c("Withhold exercise.",
                "Ask about infection symptoms: cough, urinary symptoms, wound / access-site redness or pain. PD patients: check for cloudy effluent or abdominal pain.",
                "Encourage rest and fluids within the patient's fluid restriction."),
    retest = "Re-check temperature per local ward protocol.",
    notify = "Inform the medical officer today for an infection screen (urgent for PD patients with cloudy effluent or abdominal pain).",
    notify_level = "now"),
  hypoglycemia = list(
    actions = c("Withhold exercise.",
                "If alert and able to swallow, give 15-20 g fast-acting oral carbohydrate.",
                "Do not leave the patient alone; follow the hospital hypoglycaemia protocol if drowsy or unable to swallow."),
    retest = "Re-check glucose after 15 minutes and repeat treatment if still < 3.9 mmol/L (ADA \"rule of 15\"). Exercise only once glucose is above 5.0 mmol/L.",
    notify = "Inform the medical officer if glucose stays low after two treatments, the patient is drowsy, or hypoglycaemia is recurrent.",
    notify_level = "conditional"),
  hyperkalaemia = list(
    actions = c("Withhold exercise.",
                "Check the result is recent and the sample was not haemolysed (confirm with the ward / laboratory).",
                "Ask about weakness and palpitations; arrange urgent medical review and ECG as instructed."),
    retest = "Repeat blood potassium as directed by the medical team.",
    notify = "Inform the medical officer / nephrologist now.",
    notify_level = "now"),
  anaemia = list(
    actions = c("Withhold exercise.",
                "Check the result is recent (see the blood test date).",
                "Ask about breathlessness, dizziness, chest pain, palpitations, bleeding or dark stools."),
    retest = "Repeat haemoglobin as directed by the medical team.",
    notify = "Inform the medical officer the same day.",
    notify_level = "now"),
  hd_new = list(
    actions = c("Do not start the exercise programme yet.",
                "Explain that exercise is deferred during the first 3 months of haemodialysis (UKKA guideline).",
                "Encourage light daily activity as tolerated and review at 3 months."),
    retest = "Not applicable - review once 3 months of haemodialysis are completed.",
    notify = "Inform the nephrologist only if the patient asks to start exercising earlier.",
    notify_level = "no"),
  recent_mi = list(
    actions = c("Withhold exercise.",
                "Confirm the date of the event and whether cardiology clearance has been given.",
                "Arrange medical review before any exercise."),
    retest = "Not applicable - medical clearance required.",
    notify = "Notify the medical team today.",
    notify_level = "now"),
  unstable_condition = list(
    actions = c("Withhold exercise.",
                "Document the condition (e.g. decompensated heart failure, uncontrolled arrhythmia, undiagnosed chest pain, infection).",
                "Arrange medical review before any exercise."),
    retest = "Not applicable - medical review required.",
    notify = "Notify the medical team today.",
    notify_level = "now"),
  chest_pain = list(
    actions = c("Stop all activity. Sit or lie the patient down.",
                "Call the emergency / resuscitation team according to hospital protocol.",
                "Monitor BP, HR and SpO2; prepare ECG / AED."),
    retest = "Not applicable - the medical team decides.",
    notify = "Notify the medical team immediately.",
    notify_level = "now"),
  severe_sob = list(
    actions = c("Stop all activity. Sit the patient upright.",
                "Check SpO2, HR and BP; look for fluid overload (swelling, weight gain, orthopnoea).",
                "Follow the hospital deterioration / emergency protocol if SpO2 is low or distress increases."),
    retest = "Re-check SpO2 and respiratory rate after 5 minutes.",
    notify = "Notify the medical officer immediately.",
    notify_level = "now"),
  bp_symptoms = list(
    actions = c("Stop activity. Sit or lie the patient down.",
                "Measure BP and HR (seated, then lying if low BP is suspected).",
                "Ask about headache, visual disturbance, light-headedness, chest pain and breathlessness."),
    retest = "After at least 5 minutes of rest.",
    notify = "Inform the medical officer (UKKA: symptomatic hyper- or hypotension is a reason not to exercise).",
    notify_level = "conditional"),
  dizziness = list(
    actions = c("Stop activity. Sit or lie the patient down.",
                "Check BP, HR and blood glucose.",
                "Ask about recent medication changes, fluid removal (dialysis) and food intake."),
    retest = "After resting; repeat BP, HR and blood glucose.",
    notify = "Inform the medical officer if dizziness persists, BP is abnormal, or other red flags are present.",
    notify_level = "conditional"),
  access_bleeding = list(
    actions = c("Apply firm direct pressure over the site (do not occlude the whole limb for long); keep the limb elevated and still.",
                "Call the dialysis nurse / medical officer immediately.",
                "Do not resume exercise until the access has been reviewed."),
    retest = "Not applicable - access review required.",
    notify = "Notify the dialysis nurse / medical officer immediately.",
    notify_level = "now")
)

# Map a STOP message to its guidance entry
step_code_for <- function(msg) {
  patterns <- c(
    sbp_high = "Severe Resting Hypertension", dbp_high = "Severe Diastolic Hypertension",
    hr_abn = "Abnormal Resting Heart Rate",
    spo2_low = "Hypoxemia", fever = "Fever / Active Infection", hypoglycemia = "Hypoglycemia (",
    hyperkalaemia = "Hyperkalaemia, moderate-severe", anaemia = "Severe Anaemia",
    hd_new = "Haemodialysis started", recent_mi = "Myocardial infarction within",
    unstable_condition = "Clinically unstable medical condition",
    chest_pain = "Acute Chest Pain", severe_sob = "Severe Dyspnea",
    bp_symptoms = "Symptomatic high / low blood pressure",
    dizziness = "Dizziness / Vertigo", access_bleeding = "AV Access / Exit-site Bleeding"
  )
  hit <- names(patterns)[vapply(patterns, function(p) grepl(p, msg, fixed = TRUE), logical(1))]
  if (length(hit) > 0) hit[1] else NULL
}

# --- 1. eGFR Calculation (CKD-EPI 2021 Equation - Race Free; scr in mg/dL) ---
calculate_egfr <- function(scr, age, gender) {
  if (!num_ok(scr) || !num_ok(age) || is.null(gender) || length(gender) != 1 ||
      is.na(gender) || scr <= 0 || age <= 0) {
    return(NULL)
  }
  
  gender_clean <- tolower(trimws(as.character(gender)))
  if (!gender_clean %in% c("male", "female")) return(NULL)
  kappa <- if (gender_clean == "female") 0.7 else 0.9
  alpha <- if (gender_clean == "female") -0.241 else -0.302
  gender_factor <- if (gender_clean == "female") 1.012 else 1.0
  
  scr_kappa <- scr / kappa
  min_val <- min(scr_kappa, 1)
  max_val <- max(scr_kappa, 1)
  
  egfr <- 142 * (min_val^alpha) * (max_val^-1.200) * (0.9938^age) * gender_factor
  
  stage <- if (egfr >= 90) "Stage 1 (Normal / High GFR)"
  else if (egfr >= 60) "Stage 2 (Mildly Decreased)"
  else if (egfr >= 45) "Stage 3a (Mild-Moderate Decrease)"
  else if (egfr >= 30) "Stage 3b (Moderate-Severe Decrease)"
  else if (egfr >= 15) "Stage 4 (Severely Decreased)"
  else "Stage 5 (Kidney Failure)"
  
  list(egfr = round(egfr, 1), stage = stage)
}

# --- 2. Functional norms (age/sex adjusted) ---
# 30s chair-stand "below average" cut-offs (CDC STEADI norms, ages 60-94).
# Ages <60 use the 60-64 row; ages >=90 use the 90-94 row.
sts_norm_cutoff <- function(age, gender) {
  brk  <- c(60, 65, 70, 75, 80, 85, 90)
  male <- c(14, 12, 12, 11, 10, 8, 7)
  fem  <- c(12, 11, 10, 10, 9, 8, 4)
  if (!num_ok(age)) return(10)
  idx <- max(1, findInterval(age, brk))
  is_male <- !is.null(gender) && length(gender) == 1 && !is.na(gender) &&
    tolower(trimws(as.character(gender))) == "male"
  if (is_male) male[idx] else fem[idx]
}

# Low handgrip strength cut-offs (EWGSOP2): men < 27 kg, women < 16 kg
grip_low_cutoff <- function(gender) {
  is_male <- !is.null(gender) && length(gender) == 1 && !is.na(gender) &&
    tolower(trimws(as.character(gender))) == "male"
  if (is_male) 27 else 16
}

# --- 3. Borg RPE classification (shared by UI badge, post-evaluation and report) ---
classify_rpe <- function(rpe, target = c(11, 13)) {
  if (!num_ok(rpe)) return(list(zone = "unknown", color = "#64748b", label = "RPE not recorded", icon = ""))
  low <- target[1]; high <- target[2]
  if (rpe < low) {
    list(zone = "under", color = "#0284c7", icon = "\U0001F535",
         label = sprintf("Borg RPE %.1f: Below prescribed target (%.0f-%.0f)", rpe, low, high))
  } else if (rpe <= high) {
    list(zone = "target", color = "#16a34a", icon = "\U0001F7E2",
         label = sprintf("Borg RPE %.1f: Within prescribed target zone (%.0f-%.0f)", rpe, low, high))
  } else if (rpe < 15) {
    list(zone = "upper", color = "#d97706", icon = "\U0001F7E0",
         label = sprintf("Borg RPE %.1f: Upper safety threshold (target %.0f-%.0f)", rpe, low, high))
  } else {
    list(zone = "over", color = "#dc2626", icon = "\U0001F534",
         label = sprintf("Borg RPE %.1f: Over-exertion warning (target %.0f-%.0f)", rpe, low, high))
  }
}

# --- 4. Report risk flags (derived from the same thresholds as the engine) ---
flag_bp <- function(sbp, dbp) {
  if (num_ok(sbp) && sbp > THR$sbp_stop) return("danger")
  if (num_ok(dbp) && dbp > THR$dbp_stop) return("danger")
  if (num_ok(sbp) && sbp < THR$sbp_low) return("warn")
  "safe"
}
flag_hr_spo2 <- function(hr, spo2, post = FALSE) {
  if (num_ok(spo2) && spo2 < THR$spo2_low) return("danger")
  if (post) {
    if (num_ok(hr) && hr > THR$post_hr_high) return("warn")
  } else {
    if (num_ok(hr) && hr > THR$hr_stop) return("danger")
    if (num_ok(hr) && hr > THR$hr_caution) return("warn")
  }
  "safe"
}
flag_labs <- function(k, hb, glucose, temp) {
  if (num_ok(k) && k >= THR$k_stop) return("danger")
  if (num_ok(hb) && hb < THR$hb_low) return("danger")
  if (num_ok(glucose) && glucose < THR$glu_low) return("danger")
  if (num_ok(temp) && temp >= THR$temp_fever) return("danger")
  if (num_ok(k) && (k >= THR$k_caution || k < THR$k_low)) return("warn")
  if (num_ok(glucose) && glucose > THR$glu_high) return("warn")
  "safe"
}
worst_flag <- function(...) {
  f <- c(...)
  if ("danger" %in% f) "danger" else if ("warn" %in% f) "warn" else "safe"
}

# --- 5. Pre-Test Evaluation & FITT Generation Engine ---
generate_cdss_recommendation <- function(sbp, dbp, hr, spo2, temp, glucose,
                                         scr, age, gender, dry_weight, pre_weight,
                                         k_level, hb_level, ckd_category, dial_time,
                                         fistula_arm, fistula_age,
                                         pd_full_abdomen, pd_hernia, pd_catheter_status,
                                         post_transplant_time,
                                         mwt6_dist, sts30_reps, handgrip_kg,
                                         diabetic_neuropathy, foot_ulcer,
                                         lifestyle_risks, symptoms, lab_age_days = NULL,
                                         hd_vintage = NULL, hypo_risk_meds = FALSE,
                                         start_level = "standard") {
  
  stop_alerts <- c()      # reasons NOT to exercise today (guideline contraindications)
  caution_alerts <- c()   # proceed only with the modifications listed
  next_steps <- list()    # guidance entries, one per STOP reason
  precautions <- c()
  lifestyle_advice <- c()
  
  add_stop    <- function(msg) {
    stop_alerts <<- c(stop_alerts, msg)
    code <- step_code_for(msg)
    if (!is.null(code)) next_steps[[length(next_steps) + 1]] <<- c(list(code = code, finding = msg), NEXT_STEPS[[code]])
  }
  add_caution <- function(msg) caution_alerts <<- c(caution_alerts, msg)
  
  # ----------------------------------------------------------------------------
  # 1. Resting vital signs (criteria: see THR_SOURCES)
  # ----------------------------------------------------------------------------
  if (num_ok(sbp) && sbp > THR$sbp_stop) {
    add_stop(sprintf("Severe Resting Hypertension (SBP %.0f mmHg > %.0f)", sbp, THR$sbp_stop))
  }
  if (num_ok(dbp) && dbp > THR$dbp_stop) {
    add_stop(sprintf("Severe Diastolic Hypertension (DBP %.0f mmHg > %.0f)", dbp, THR$dbp_stop))
  }
  if (num_ok(sbp) && sbp < THR$sbp_low) {
    add_caution(sprintf("Low Resting SBP (%.0f mmHg < %.0f)", sbp, THR$sbp_low))
    precautions <- c(precautions, "Low resting SBP: ask about dizziness or light-headedness. Symptomatic hypotension means do NOT exercise (UKKA). If asymptomatic, monitor BP during the session and stop if symptoms appear.")
  }
  if (num_ok(hr) && hr > THR$hr_stop) {
    add_stop(sprintf("Abnormal Resting Heart Rate (%.0f bpm > %.0f)", hr, THR$hr_stop))
  } else if (num_ok(hr) && hr > THR$hr_caution) {
    add_caution(sprintf("Elevated Resting Heart Rate (%.0f bpm, above %.0f)", hr, THR$hr_caution))
    precautions <- c(precautions, "Resting HR above 100 bpm (tachycardia): re-check after seated rest; consider pain, fever, anxiety, dehydration, caffeine and medication; monitor closely if proceeding.")
  }
  if (num_ok(spo2) && spo2 < THR$spo2_low) {
    add_stop(sprintf("Hypoxemia (SpO2 %.0f%% < %.0f%%)", spo2, THR$spo2_low))
  }
  if (num_ok(temp) && temp >= THR$temp_fever) {
    add_stop(sprintf("Fever / Active Infection (Temp %.1f \u00B0C)", temp))
  }
  if (num_ok(glucose) && glucose < THR$glu_low) {
    add_stop(sprintf("Hypoglycemia (Blood Glucose %.1f mmol/L < %.1f)", glucose, THR$glu_low))
  } else if (num_ok(glucose) && glucose < THR$glu_target_low && isTRUE(hypo_risk_meds)) {
    add_caution(sprintf("Glucose %.1f mmol/L is below the pre-exercise target of %.1f on insulin / sulfonylurea", glucose, THR$glu_target_low))
    precautions <- c(precautions, "Take 15-30 g fast-acting carbohydrate before exercising and re-check glucose in about 15 minutes (ADA / ACSM). Carry fast-acting carbohydrate during the session.")
  } else if (num_ok(glucose) && glucose > THR$glu_high) {
    add_caution(sprintf("Marked Hyperglycemia (Blood Glucose %.1f mmol/L > %.1f)", glucose, THR$glu_high))
    precautions <- c(precautions, if (glucose >= THR$glu_very_high)
      "Glucose >= 19.4 mmol/L (350 mg/dL): test ketones before any exercise; do NOT exercise if moderate / large ketones are present; delay intense exercise until glucose is below 13.9 mmol/L (ADA / ACSM)."
      else
        "Glucose > 13.9 mmol/L (250 mg/dL): check ketones if available - do NOT exercise if moderate / large ketones are present. With no ketones and feeling well, light-moderate activity may proceed (ADA / ACSM).")
  }
  
  # ----------------------------------------------------------------------------
  # 2. Blood results
  # ----------------------------------------------------------------------------
  if (num_ok(k_level) && k_level >= THR$k_stop) {
    add_stop(sprintf("Hyperkalaemia, moderate-severe (K+ %.1f mmol/L >= %.1f; UKKA classification)", k_level, THR$k_stop))
  } else if (num_ok(k_level) && k_level >= THR$k_caution) {
    add_caution(sprintf("Mild Hyperkalaemia (K+ %.1f mmol/L, 5.5-5.9; UKKA classification)", k_level))
    precautions <- c(precautions, "Mild hyperkalaemia: confirm the result is recent and not haemolysed; ask about weakness or palpitations; inform the nephrology team.")
  } else if (num_ok(k_level) && k_level < THR$k_low) {
    add_caution(sprintf("Hypokalaemia (K+ %.1f mmol/L < %.1f)", k_level, THR$k_low))
    precautions <- c(precautions, "Hypokalaemia: ask about cramps, weakness or palpitations and inform the medical team.")
  }
  if (num_ok(hb_level) && hb_level < THR$hb_low) {
    add_stop(sprintf("Severe Anaemia (Hb %.1f g/dL < %.1f; WHO definition)", hb_level, THR$hb_low))
  }
  if (num_ok(lab_age_days) && !is.na(THR$lab_stale_days_dialysis) && !is.na(THR$lab_stale_days_other)) {
    stale_limit <- if (identical(ckd_category, "dialysis")) THR$lab_stale_days_dialysis else THR$lab_stale_days_other
    if (lab_age_days > stale_limit) {
      add_caution(sprintf("Blood results may be out of date (%.0f days old; unit limit %.0f days)", lab_age_days, stale_limit))
      precautions <- c(precautions, "Blood results are older than your unit's review limit: obtain a repeat blood test or confirm with the medical team.")
    }
  }
  
  # ----------------------------------------------------------------------------
  # 3. Starting level (clinician decision) and functional tests (shown against published norms)
  # ----------------------------------------------------------------------------
  light <- identical(start_level, "light")
  start_label <- if (light) "Light start" else "Standard"
  func_notes <- c()
  if (num_ok(sts30_reps) && sts30_reps > 0) {
    cutoff <- sts_norm_cutoff(age, gender)
    func_notes <- c(func_notes, if (sts30_reps < cutoff)
      sprintf("30s-STS %.0f reps: below the CDC STEADI age/sex norm (%d) - consider a light start.", sts30_reps, cutoff)
      else sprintf("30s-STS %.0f reps: meets the CDC STEADI age/sex norm (%d).", sts30_reps, cutoff))
  }
  if (num_ok(handgrip_kg) && handgrip_kg > 0) {
    g_cut <- grip_low_cutoff(gender)
    func_notes <- c(func_notes, if (handgrip_kg < g_cut)
      sprintf("Handgrip %.1f kg: below the EWGSOP2 cut-off (%d kg) - low muscle strength; consider sarcopenia screening / dietitian review.", handgrip_kg, g_cut)
      else sprintf("Handgrip %.1f kg: adequate (EWGSOP2 cut-off %d kg).", handgrip_kg, g_cut))
  }
  
  # ----------------------------------------------------------------------------
  # 4. Haemodialysis (UKKA 2021 Rec 2.1-2.3)
  # ----------------------------------------------------------------------------
  if (ckd_category == "dialysis") {
    if (identical(hd_vintage, "under3m")) {
      add_stop("Haemodialysis started < 3 months ago (UKKA: avoid exercise)")
    }
    has_av  <- !is.null(fistula_arm) && length(fistula_arm) == 1 && fistula_arm %in% c("Left Arm", "Right Arm")
    has_cvc <- !is.null(fistula_arm) && length(fistula_arm) == 1 && fistula_arm == "CVC"
    
    if (has_av) {
      precautions <- c(precautions, sprintf("AV access (%s): do not place a BP cuff, IV line or tight band on this arm.", fistula_arm))
      precautions <- c(precautions, "Test handgrip / upper-limb strength on the NON-access arm only.")
      if (!is.null(fistula_age) && length(fistula_age) == 1 && fistula_age == "early") {
        add_caution(sprintf("AV access on %s not yet cleared for use by the access team", fistula_arm))
        precautions <- c(precautions, "Access not yet cleared: no resistance or heavy load on the access arm; gentle hand / finger exercises only unless the access team advises otherwise.")
      } else {
        precautions <- c(precautions, "Established access: avoid heavy local load directly compressing the access site.")
      }
    } else if (has_cvc) {
      precautions <- c(precautions, "Central venous catheter: keep the dressing clean, dry and intact; avoid pulling on the line and heavy upper-body resistance on the catheter side; no swimming.")
    } else {
      precautions <- c(precautions, "No vascular access recorded: confirm access type before exercise and avoid BP cuffs / IVs on any access limb.")
    }
    
    if (num_ok(dial_time) && dial_time < THR$dial_early_hr) {
      add_caution(sprintf("Early in the dialysis session (%.1f h elapsed)", dial_time))
      precautions <- c(precautions, "UKKA suggests avoiding exercise in the first 30 minutes of haemodialysis. There is no contraindication to exercising in the second half of the session.")
    }
  }
  
  # ----------------------------------------------------------------------------
  # 5. Peritoneal dialysis: ISPD / GREX 2022 practice points (expert consensus; not covered by UKKA 2021)
  # ----------------------------------------------------------------------------
  if (ckd_category == "peritoneal") {
    precautions <- c(precautions, "Avoid breath-holding (Valsalva) and heavy abdominal straining during resistance exercise (general safety advice for patients with a dialysis catheter in situ).")
    if (isTRUE(pd_full_abdomen)) {
      precautions <- c(precautions, "PD with a full abdomen: the ISPD / GREX practice points address exercising with a full vs empty abdomen. If it causes discomfort, consider draining before exercise (confirm local practice).")
    }
    if (isTRUE(pd_hernia)) {
      add_caution("Known abdominal / inguinal hernia in a PD patient")
      precautions <- c(precautions, "Avoid heavy resistance and high intra-abdominal pressure activities until the hernia has been surgically assessed.")
    }
    if (!is.null(pd_catheter_status) && length(pd_catheter_status) == 1 && pd_catheter_status == "healing") {
      add_caution("PD catheter exit site not yet healed (per the PD team)")
      precautions <- c(precautions, "Exit site not yet healed: no swimming or water immersion; avoid activities causing heavy sweating or friction at the exit site until the PD team confirms healing.")
    } else {
      precautions <- c(precautions, "Swimming: only as advised by the PD team (protected exit site, suitable water); avoid open water and hot tubs.")
    }
  }
  
  # ----------------------------------------------------------------------------
  # 6. Kidney transplant (UKKA 2021 Rec 3.1-3.16)
  # ----------------------------------------------------------------------------
  if (ckd_category == "transplant") {
    if (!is.null(post_transplant_time) && length(post_transplant_time) == 1 && post_transplant_time == "early") {
      add_caution("Transplant team restrictions still in force")
      precautions <- c(precautions, "Follow the transplant team's current restrictions (UKKA does not define a time limit). Avoid contact sports, heavy abdominal resistance and direct pressure over the graft (UKKA 3.15: avoid traumatic damage to the transplanted kidney).")
    } else {
      precautions <- c(precautions, "Avoid contact sports and consider prolonged extreme exercise carefully; avoid sport-enhancing supplements (UKKA 3.15-3.16).")
    }
  }
  if (ckd_category == "pre_transplant") {
    precautions <- c(precautions, "UKKA Rec 3.13 (Grade 2C): exercise before surgery (prehabilitation) may help increase pre-transplant physical activity and aid recovery.")
  }
  
  # ----------------------------------------------------------------------------
  # 7. Diabetic foot
  # ----------------------------------------------------------------------------
  foot_risk <- FALSE
  if (isTRUE(diabetic_neuropathy) || isTRUE(foot_ulcer)) {
    foot_risk <- TRUE
    if (isTRUE(foot_ulcer)) {
      add_caution("Active diabetic foot ulcer")
      precautions <- c(precautions, "Active foot ulcer: non-weight-bearing exercise only; refer to podiatry / wound care.")
    } else {
      precautions <- c(precautions, "Peripheral neuropathy: prefer low-impact activity with suitable footwear; inspect the feet before and after every session.")
    }
  }
  
  # ----------------------------------------------------------------------------
  # 8. Lifestyle (UKKA 2021)
  # ----------------------------------------------------------------------------
  if (!is.null(lifestyle_risks) && length(lifestyle_risks) > 0) {
    if ("smoking" %in% lifestyle_risks) {
      lifestyle_advice <- c(lifestyle_advice, "Smoking cessation (UKKA 1.8 / 2.10 / 3.24, Grade 1A): offer stop-smoking support, including behavioural support and pharmacotherapy as locally available.")
    }
    if ("alcohol" %in% lifestyle_risks) {
      lifestyle_advice <- c(lifestyle_advice, "Alcohol (UKKA 1.9 / 2.11, Grade 1B): keep within national guidelines (UK Chief Medical Officers: no more than 14 units a week, spread over 3 or more days).")
    }
    if ("nephrotoxic" %in% lifestyle_risks) {
      lifestyle_advice <- c(lifestyle_advice, "Nephrotoxin avoidance (general nephrology advice, not a UKKA exercise-guideline recommendation): counsel against non-prescribed NSAIDs, unverified herbal supplements and protein powders.")
    }
  }
  
  # ----------------------------------------------------------------------------
  # 9. Red flags (UKKA: unstable conditions, symptomatic BP, undiagnosed chest pain, recent MI)
  # ----------------------------------------------------------------------------
  if (!is.null(symptoms) && length(symptoms) > 0) {
    if ("chest_pain" %in% symptoms) add_stop("Red Flag Symptom: Acute Chest Pain / Angina")
    if ("severe_sob" %in% symptoms) add_stop("Red Flag Symptom: Severe Dyspnea")
    if ("bp_symptoms" %in% symptoms) add_stop("Red Flag Symptom: Symptomatic high / low blood pressure")
    if ("dizziness" %in% symptoms) add_stop("Red Flag Symptom: Dizziness / Vertigo")
    if ("fistula_bleeding" %in% symptoms) add_stop("Red Flag Symptom: AV Access / Exit-site Bleeding or Pain")
    if ("recent_mi" %in% symptoms) add_stop("Red Flag: Myocardial infarction within the last 2 weeks")
    if ("unstable_condition" %in% symptoms) add_stop("Red Flag: Clinically unstable medical condition")
  }
  
  status <- if (length(stop_alerts) > 0) "CONTRAINDICATED" else if (length(caution_alerts) > 0) "CAUTION" else "SAFE"
  alerts <- c(stop_alerts, caution_alerts)
  
  # ----------------------------------------------------------------------------
  # 10. FITT (UKKA 2021; ACSM Borg classification: light 9-11, moderate 12-13)
  # ----------------------------------------------------------------------------
  is_hd <- ckd_category == "dialysis"
  target_rpe <- if (light) c(9, 11) else if (is_hd) c(12, 14) else c(12, 13)
  intensity_txt <- if (light) {
    if (is_hd) "Light Intensity (Borg RPE 9-11); progress toward moderate (RPE 12-14) as tolerated; never above RPE 15 (hard)"
    else "Light Intensity (Borg RPE 9-11); progress toward moderate (RPE 12-13) as tolerated"
  } else if (is_hd) "Moderate Intensity (Borg RPE 12-14); never above RPE 15 (hard)"
  else "Moderate Intensity (Borg RPE 12-13)"
  time_txt <- if (light) "Short bouts (about 10 mins) building toward 30 mins/session"
  else "At least 30 mins/session on most days (bouts of 10 mins or more count toward the weekly total)"
  
  aerobic_type_standard <- "Aerobic exercise (Brisk walking, cycling)"
  aerobic_type_safe <- if (foot_risk) "NON-WEIGHT-BEARING Aerobic exercise (Recumbent stationary bike, arm ergometer, seated aerobics)" else aerobic_type_standard
  caution_note  <- if (status == "CAUTION") " CAUTION status: use the lower end of the intensity range and re-check symptoms and BP every 5-10 minutes." else ""
  
  fitt <- list()
  if (ckd_category == "non_dialysis") {
    fitt <- list(
      Category = sprintf("Non-Dialysis CKD (Starting level: %s)", start_label),
      UKKA_Grade = "UKKA 2021 Rec 1.1, 1.2, 1.5 (Grade 1B / 1B / 1C)",
      Frequency = "Daily activity; structured exercise 3-5 days/week (accumulating at least 150 mins/week moderate intensity)",
      Intensity = intensity_txt,
      Time = time_txt,
      Type = sprintf("%s + Progressive Resistance Training (2 or more days/week, major muscle groups) with balance and flexibility work. Avoid high-intensity interval training.", aerobic_type_safe),
      Monitoring = paste0("Monitor BP and HR before and after exercise; use Borg RPE to guide intensity; stop if symptoms develop.", caution_note)
    )
  } else if (is_hd) {
    has_early_av <- !is.null(fistula_arm) && length(fistula_arm) == 1 && fistula_arm %in% c("Left Arm", "Right Arm") &&
      !is.null(fistula_age) && length(fistula_age) == 1 && fistula_age == "early"
    fitt <- list(
      Category = sprintf("Haemodialysis Patient (Starting level: %s)", start_label),
      UKKA_Grade = "UKKA 2021 Rec 2.1-2.3 (Grade 1B-1C)",
      Frequency = "3 days/week (every haemodialysis session); add activity on non-dialysis days to work toward 150 mins/week",
      Intensity = intensity_txt,
      Time = if (light) "Short bouts building toward 30 mins/session (avoid the first 30 minutes of treatment)"
      else "At least 30 mins/session (avoid the first 30 minutes of treatment)",
      Type = if (has_early_av)
        "Intradialytic cycling (static cycle ergometer) and lower-limb resistance only. Access not yet cleared: no resistance on the access arm; gentle hand / finger exercises only unless the access team advises otherwise."
      else
        "Intradialytic cycling (static cycle ergometer) +/- light resistance (resistance bands / ankle weights). Avoid heavy load on the access arm / catheter side.",
      Monitoring = paste0("Before: ask how the patient feels; record the last intradialytic BP and HR. During: report pain, excessive fatigue, altered consciousness, overheating, anxiety, severe breathlessness, chest pain or dizziness; stop for symptomatic hypotension.", caution_note)
    )
  } else if (ckd_category == "peritoneal") {
    fitt <- list(
      Category = sprintf("Peritoneal Dialysis Patient (Starting level: %s)", start_label),
      UKKA_Grade = "Not covered by UKKA 2021 - ISPD / GREX 2022 practice points (expert consensus)",
      Frequency = "Daily activity; structured exercise 3-5 days/week (building toward 150 mins/week)",
      Intensity = intensity_txt,
      Time = time_txt,
      Type = sprintf("%s + Light Resistance (avoid breath-holding and heavy abdominal strain). Consider draining dialysate if a full abdomen causes discomfort. Swimming only as advised by the PD team.", aerobic_type_safe),
      Monitoring = paste0("Monitor abdominal pressure symptoms and the catheter exit site.", caution_note)
    )
  } else if (ckd_category == "pre_transplant") {
    fitt <- list(
      Category = sprintf("Pre-Transplant Candidate - prehabilitation (Starting level: %s)", start_label),
      UKKA_Grade = "UKKA 2021 Rec 3.13 (Grade 2C)",
      Frequency = "Structured prehabilitation as set by the supervising team (aerobic and resistance)",
      Intensity = intensity_txt,
      Time = time_txt,
      Type = sprintf("%s + Strengthening of core, quadriceps and gluteal muscles (sit-to-stand, step-ups).", aerobic_type_safe),
      Monitoring = paste0("Repeat the functional tests periodically; monitor BP and symptom response.", caution_note)
    )
  } else if (ckd_category == "transplant") {
    is_early_tx <- !is.null(post_transplant_time) && length(post_transplant_time) == 1 && post_transplant_time == "early"
    fitt <- list(
      Category = sprintf("Kidney Transplant Recipient (%s; Starting level: %s)",
                         if (is_early_tx) "transplant team restrictions in force" else "no restrictions", start_label),
      UKKA_Grade = "UKKA 2021 Rec 3.1, 3.3, 3.9-3.12, 3.15 (Grade 1B-2C)",
      Frequency = "At least 3 days/week structured exercise (aerobic and resistance); 150 mins/week moderate-vigorous activity",
      Intensity = intensity_txt,
      Time = time_txt,
      Type = if (is_early_tx)
        sprintf("%s + Light resistance within the transplant team's restrictions. No contact sports or heavy abdominal strain.", aerobic_type_safe)
      else
        sprintf("%s + Progressive Resistance Training. Avoid contact sports.", aerobic_type_safe),
      Monitoring = paste0("Monitor BP response and graft-site symptoms.", caution_note)
    )
  }
  
  next_appt <- if (status == "SAFE") {
    "Proceed with today's session. Follow-up: Next scheduled therapy session."
  } else if (status == "CAUTION") {
    "Proceed with a MODIFIED session as per the precautions below. Re-assess at the next session and escalate to the medical team if cautions persist."
  } else {
    "Session postponed due to contraindications. Re-evaluate clinical stability prior to next session."
  }
  
  list(
    status = status,
    alerts = alerts,
    stop_alerts = stop_alerts,
    caution_alerts = caution_alerts,
    next_steps = next_steps,
    precautions = unique(precautions),
    lifestyle_advice = lifestyle_advice,
    functional_tier = start_label,
    func_notes = func_notes,
    fitt = fitt,
    target_rpe = target_rpe,
    next_appointment = next_appt
  )
}

# --- 6. Post-Exercise Guidance Engine ---
generate_post_exercise_guidance <- function(post_sbp, post_dbp, post_hr, post_spo2, post_rpe,
                                            pre_sbp, post_symptoms, target_rpe = c(11, 13)) {
  
  lv <- c("CLEARED", "OBSERVATION_REQUIRED", "MEDICAL_REVIEW_REQUIRED")
  discharge_status <- "CLEARED"
  upgrade <- function(new) {
    if (match(new, lv) > match(discharge_status, lv)) discharge_status <<- new
  }
  
  issues <- c()
  rpe_adjust <- "Maintain current FITT protocol for the next session."
  bp_adjust <- NULL
  symptom_adjust <- NULL
  rpe_msg <- NULL
  
  # --- Borg RPE vs the PRESCRIBED target (no gaps between zones) ---
  if (num_ok(post_rpe)) {
    cls <- classify_rpe(post_rpe, target_rpe)
    if (cls$zone == "under") {
      rpe_msg <- sprintf("%s <strong>%s:</strong> Intensity was below the therapeutic target. Consider gradually increasing workload or duration if the patient remains asymptomatic.", cls$icon, cls$label)
      rpe_adjust <- "Intensity was low. Progress workload/duration by 10-15% next session if patient remains asymptomatic."
    } else if (cls$zone == "target") {
      rpe_msg <- sprintf("%s <strong>%s:</strong> Patient achieved the prescribed intensity. Ideal cardiovascular and renal rehab response.", cls$icon, cls$label)
      rpe_adjust <- "Maintain current FITT protocol for the next session."
    } else if (cls$zone == "upper") {
      rpe_msg <- sprintf("%s <strong>%s:</strong> Exertion is above target and close to the upper safety limit. Monitor post-session recovery closely.", cls$icon, cls$label)
      rpe_adjust <- "Slightly reduce exercise duration or increase rest intervals. Avoid further progression next session."
    } else if (cls$zone == "over") {
      rpe_msg <- sprintf("%s <strong>%s:</strong> Patient experienced high exertion exceeding safe thresholds!", cls$icon, cls$label)
      rpe_adjust <- "Step down exercise intensity by 30-50% for the next session to prevent cardiovascular strain and exhaustion."
    }
  }
  
  # --- Blood pressure ---
  if (num_ok(post_sbp)) {
    if (post_sbp > THR$sbp_stop || (num_ok(post_dbp) && post_dbp > THR$dbp_stop)) {
      upgrade("MEDICAL_REVIEW_REQUIRED")
      issues <- c(issues, sprintf("\U0001F6A8 <strong>CRITICAL:</strong> Post-exercise BP is %.0f/%s mmHg (above %.0f/%.0f). Do NOT discharge the patient until reviewed.",
                                  post_sbp, if (num_ok(post_dbp)) sprintf("%.0f", post_dbp) else "?", THR$sbp_stop, THR$dbp_stop))
      bp_adjust <- "Step down exercise intensity by 30-50% and consult the nephrologist regarding antihypertensive therapy."
    } else if (post_sbp < THR$sbp_low) {
      upgrade("MEDICAL_REVIEW_REQUIRED")
      issues <- c(issues, sprintf("\U0001F6A8 <strong>LOW POST-EXERCISE BP:</strong> Post-exercise SBP is %.0f mmHg (< %.0f). Place supine with legs elevated and notify the medical officer.", post_sbp, THR$sbp_low))
      bp_adjust <- "Hold progression. Review hydration, medication and (on dialysis) fluid removal with the medical team before the next session."
    }
  }
  
  # --- SpO2 / HR ---
  if (num_ok(post_spo2) && post_spo2 < THR$spo2_low) {
    upgrade("MEDICAL_REVIEW_REQUIRED")
    issues <- c(issues, sprintf("\U0001F6A8 <strong>POST-EXERCISE HYPOXEMIA:</strong> SpO2 %.0f%% (< %d%%). Sit upright, re-check SpO2 and notify medical officer.", post_spo2, THR$spo2_low))
  }
  if (num_ok(post_hr) && post_hr > THR$post_hr_high) {
    upgrade("OBSERVATION_REQUIRED")
    issues <- c(issues, sprintf("\u26A0\uFE0F <strong>HIGH POST-EXERCISE HR:</strong> %.0f bpm (above %.0f). Rest seated and re-check before discharge.", post_hr, THR$post_hr_high))
  }
  
  # --- Post-exercise symptoms: tiered ---
  if (!is.null(post_symptoms) && length(post_symptoms) > 0) {
    major <- intersect(post_symptoms, c("post_dizziness", "post_sob"))
    minor <- intersect(post_symptoms, c("post_fatigue", "post_cramps"))
    if (length(major) > 0) {
      upgrade("MEDICAL_REVIEW_REQUIRED")
      labels <- c(post_dizziness = "dizziness/lightheadedness", post_sob = "persistent shortness of breath")
      issues <- c(issues, sprintf("\U0001F6A8 <strong>ADVERSE SYMPTOMS REPORTED (%s):</strong> Do NOT discharge until cleared by attending physician.", paste(labels[major], collapse = ", ")))
      symptom_adjust <- "Do not progress. Reassess medical stability and symptoms before the next session."
    }
    if (length(minor) > 0) {
      upgrade("OBSERVATION_REQUIRED")
      labels <- c(post_fatigue = "severe fatigue/exhaustion", post_cramps = "muscle cramps")
      issues <- c(issues, sprintf("\u26A0\uFE0F <strong>SYMPTOMS REPORTED (%s):</strong> Observe until resolved; check hydration, electrolytes and intensity of the session.", paste(labels[minor], collapse = ", ")))
      if (is.null(symptom_adjust)) symptom_adjust <- "Reduce duration/intensity by ~20-30% next session and review symptoms (cramps: hydration/electrolytes, dry weight)."
    }
  }
  
  instructions <- c()
  if (discharge_status == "CLEARED") {
    instructions <- c("Patient tolerating post-exercise recovery well.", "Vital signs within safe post-exercise parameters.")
  }
  instructions <- c(instructions, issues, rpe_msg)
  
  next_adjustment <- if (!is.null(bp_adjust)) bp_adjust else if (!is.null(symptom_adjust)) symptom_adjust else rpe_adjust
  
  list(
    discharge_status = discharge_status,
    instructions = instructions,
    next_adjustment = next_adjustment
  )
}

# --- 7. Longitudinal Renal Trend Engine (sparse labs; dialysis-aware) ---
evaluate_longitudinal_trends <- function(df, ckd_category = NULL) {
  if (!is.null(ckd_category) && !egfr_applicable(ckd_category)) {
    return(list(
      trend_status = "NOT_APPLICABLE",
      message = "\u2139\uFE0F eGFR/creatinine trend alerts are not applicable for dialysis patients (creatinine reflects dialysis clearance, not native kidney function).",
      egfr_change = 0, scr_change = 0
    ))
  }
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      trend_status = "INSUFFICIENT_DATA",
      message = "No clinical session logs available for trend analysis.",
      egfr_change = 0, scr_change = 0
    ))
  }
  
  lab_df <- df[!is.na(df$sCr) & !is.na(df$eGFR), ]
  
  if (nrow(lab_df) < 2) {
    return(list(
      trend_status = "STABLE",
      message = "\u2139\uFE0F Fewer than two lab results recorded (sCr/eGFR are usually monthly/quarterly). Trend cannot be assessed yet.",
      egfr_change = 0, scr_change = 0
    ))
  }
  
  lab_df <- lab_df[order(lab_df$Date), ]
  n <- nrow(lab_df)
  first_rec <- lab_df[1, ]
  prev_rec  <- lab_df[n - 1, ]
  last_rec  <- lab_df[n, ]
  
  # Acute check: latest two results. Rounded to avoid floating-point artefacts.
  scr_step  <- round(last_rec$sCr - prev_rec$sCr, 2)
  egfr_step <- round(last_rec$eGFR - prev_rec$eGFR, 1)
  gap_days  <- as.numeric(difftime(as.Date(last_rec$Date), as.Date(prev_rec$Date), units = "days"))
  rel_drop  <- if (prev_rec$eGFR > 0) egfr_step / prev_rec$eGFR else 0
  
  # Overall check: first vs last result
  egfr_change <- round(last_rec$eGFR - first_rec$eGFR, 1)
  scr_change  <- round(last_rec$sCr - first_rec$sCr, 2)
  
  is_aki_risk <- (scr_step >= 0.3) || (rel_drop <= -0.25)
  
  if (is_aki_risk) {
    status <- "HIGH_RISK_AKI"
    msg <- sprintf("\U0001F6A8 POSSIBLE AKI / RAPID DECLINE: between the last two results (%d days apart) serum creatinine changed by %+.2f mg/dL and eGFR by %+.1f mL/min/1.73m\u00B2. Clinical review and repeat bloods recommended.",
                   as.integer(gap_days), scr_step, egfr_step)
  } else if (egfr_change < -5) {
    status <- "MODERATE_DECLINE"
    msg <- sprintf("\u26A0\uFE0F MODERATE DECLINE: eGFR has fallen by %.1f mL/min/1.73m\u00B2 over the recorded follow-up.", abs(egfr_change))
  } else {
    status <- "STABLE"
    msg <- sprintf("\u2705 RENAL TRAJECTORY STABLE: eGFR change over follow-up is %+.1f mL/min/1.73m\u00B2. No acute change between the latest two results.", egfr_change)
  }
  
  list(
    trend_status = status,
    message = msg,
    egfr_change = egfr_change,
    scr_change = scr_change,
    scr_step = scr_step, egfr_step = egfr_step, gap_days = gap_days,
    prev_date = as.Date(prev_rec$Date), last_date = as.Date(last_rec$Date),
    prev_scr = prev_rec$sCr, last_scr = last_rec$sCr,
    prev_egfr = prev_rec$eGFR, last_egfr = last_rec$eGFR
  )
}