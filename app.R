# ==============================================================================
# Shiny Application Entry Point (app.R)
# Aligned strictly with UKKA Exercise & Lifestyle in CKD Guidelines
# Optimized UI/UX & Dynamic Clinical Print Report System
# v2: 3-tier safety status, validated inputs, evaluation snapshots, session history (CSV)
# ==============================================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(shinyBS)

source("logic.R")

# ------------------------------------------------------------------------------
# 1. UI Design
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Evaluation log and handling record storage (local CSV; MRN only, no name / IC)
# ------------------------------------------------------------------------------
EVAL_FILE     <- file.path("data", "evaluations.csv")
HANDLING_FILE <- file.path("data", "handling.csv")
EVAL_COLS     <- c("Timestamp", "MRN", "Category", "SBP", "DBP", "HR", "SpO2", "Temp", "Glucose", "K", "Hb", "LabDate", "Status", "StopReasons")
HANDLING_COLS <- c("Timestamp", "MRN", "EvalTimestamp", "NotifiedWho", "NotifiedTime", "Decision", "Reschedule", "Notes")

HANDLING_DECISIONS <- c("Exercise withheld today",
                        "Re-test after rest / treatment",
                        "Medical review requested",
                        "Proceeded with modified session (medical approval)",
                        "Rescheduled",
                        "Other (see notes)")

# Borg RPE choices (explicit selection is required - no default)
RPE_CHOICES <- local({
  v <- seq(6, 20, by = 0.5)
  anchors <- c("6" = "No exertion", "7" = "Extremely light", "9" = "Very light", "11" = "Light",
               "13" = "Somewhat hard", "15" = "Hard", "17" = "Very hard", "19" = "Extremely hard", "20" = "Maximal")
  lab <- vapply(v, function(x) {
    k <- as.character(x)
    if (k %in% names(anchors)) sprintf("%s - %s", k, anchors[[k]]) else k
  }, character(1))
  c("Select..." = "", setNames(as.character(v), lab))
})

load_log <- function(path, cols) {
  empty <- as.data.frame(setNames(replicate(length(cols), character(), simplify = FALSE), cols), stringsAsFactors = FALSE)
  empty$Date <- as.Date(character())
  if (!file.exists(path)) return(empty)
  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE, colClasses = "character"), error = function(e) NULL)
  if (is.null(df) || !all(cols %in% names(df)) || nrow(df) == 0) return(empty)
  df$Date <- as.Date(substr(df$Timestamp, 1, 10))
  df
}

append_log <- function(path, row) {
  tryCatch({
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    exists <- file.exists(path)
    write.table(row, path, sep = ",", row.names = FALSE, col.names = !exists, append = exists, qmethod = "double")
    TRUE
  }, error = function(e) FALSE)
}

# Report letterhead: edit the name below; an optional logo can be placed at www/logo.png
REPORT_ORG_NAME <- "Rehabilitation Department"

ui <- dashboardPage(
  skin = "blue",
  
  title = "UKKA Renal PT CDSS",
  
  dashboardHeader(
    title = tags$span(icon("stethoscope"), " Renal PT CDSS"),
    tags$li(class = "dropdown", uiOutput("header_patient_ui", inline = TRUE)),
    tags$li(
      class = "dropdown",
      actionButton("show_ukka_modal", " Guidelines & thresholds", class = "btn-guide btn-sm", icon = icon("book-medical"))
    )
  ),
  
  dashboardSidebar(
    div(class = "patient-card",
        h5(icon("id-card"), strong(" Patient"), style = "margin-top:0; margin-bottom:10px; font-size: 12px;"),
        textInput("pt_name", "Patient Name:", value = "", placeholder = "Full name"),
        textInput("pt_mrn", "MRN (required to save history):", value = "", placeholder = "e.g. MRN-0000000"),
        textInput("pt_ic", "IC / Passport:", value = "", placeholder = "IC / Passport no."),
        actionButton("new_patient", "New patient / reset", icon = icon("user-plus"), class = "btn-default btn-block btn-sm")
    ),
    
    sidebarMenu(
      id = "tabs",
      menuItem("1  Pre-Test & FITT", tabName = "pre_eval", icon = icon("user-nurse")),
      menuItem("2  Post-Test Safety", tabName = "post_eval", icon = icon("heartbeat")),
      menuItem("3  Trends & History", tabName = "longitudinal", icon = icon("chart-line")),
      menuItem("4  Report & Export", tabName = "print_report", icon = icon("file-medical"))
    ),
    
    div(class = "progress-card",
        div(class = "pg-title", "Session progress"),
        uiOutput("sidebar_progress_ui"))
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Overall Page Styles */
        .content-wrapper { background-color: #f8fafc; }
        .main-header .logo { font-weight: 700; background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%) !important; }
        .main-header .navbar { background: linear-gradient(135deg, #2a5298 0%, #3a7bd5 100%) !important; }
        .box { border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); border-top-width: 3px !important; }
        .patient-card { padding: 14px 12px; color: #ecf0f1; background-color: #1a2226; }
        .patient-card input.form-control { background-color: #263238; border: 1px solid #3c4a50; color: #ecf0f1; font-size: 13px; }
        
        /* Clinical Alerts System */
        .clinical-alert { padding: 14px 18px; border-radius: 8px; font-weight: 600; font-size: 14px; border-left: 5px solid transparent; margin-bottom: 15px; }
        .clinical-alert-success { background-color: #f0fdf4; color: #166534; border-left-color: #22c55e; }
        .clinical-alert-warning { background-color: #fffbeb; color: #92400e; border-left-color: #f59e0b; }
        .clinical-alert-danger  { background-color: #fef2f2; color: #991b1b; border-left-color: #ef4444; }
        .clinical-alert-info    { background-color: #f0f9ff; color: #075985; border-left-color: #0284c7; }
        .cdss-placeholder { padding: 18px 14px; text-align: center; color: #64748b; background: #f8fafc; border: 1px dashed #cbd5e1; border-radius: 8px; font-size: 13px; }
        .value-badge { display: inline-block; color: #fff; padding: 4px 10px; border-radius: 20px; font-weight: 600; font-size: 12px; }
        
        /* Actionable Alert CTA Container */
        .alert-flex-container { display: flex; align-items: center; justify-content: space-between; gap: 15px; flex-wrap: wrap; }
        .alert-content-text { flex: 1; min-width: 280px; }
        .alert-cta-group { display: flex; gap: 8px; flex-shrink: 0; }
        
        /* CSS Grid Form Layout for Perfect Vertical Alignment */
        .form-grid {
          display: grid;
          grid-template-rows: auto 1fr;
          grid-auto-flow: column;
          gap: 8px 20px;
        }
        
        /* RPE Section Styling */
        .rpe-card-container {
          border: 1px solid #e2e8f0;
          padding: 18px;
          border-radius: 10px;
          background-color: #ffffff;
          margin-bottom: 18px;
          box-shadow: 0 2px 6px rgba(0,0,0,0.02);
        }
        .rpe-header-flex { display: flex; justify-content: space-between; align-items: center; gap: 12px; margin-bottom: 15px; flex-wrap: wrap; }
        .rpe-title-text { font-size: 14px; font-weight: 700; color: #1e293b; margin: 0; line-height: 1.4; flex: 1 1 200px; }
        .btn-rpe-guide { background-color: #f1f5f9; color: #0284c7; border: 1px solid #bae6fd; font-weight: 600; padding: 5px 12px; font-size: 12px; border-radius: 6px; transition: all 0.2s ease; white-space: nowrap; }
        .btn-rpe-guide:hover { background-color: #e0f2fe; color: #0369a1; border-color: #7dd3fc; }
        
        /* Rainbow Borg RPE Slider */
        .irs-line {
          background: linear-gradient(to right, 
            #38bdf8 0%, #38bdf8 28.57%, 
            #4ade80 28.57%, #4ade80 53.57%, 
            #fbbf24 53.57%, #fbbf24 60.71%, 
            #f87171 60.71%, #f87171 100%) !important;
          height: 10px !important;
          border-radius: 5px !important;
          border: none !important;
        }
        .irs-bar, .irs-bar-edge { background: transparent !important; border: none !important; }
        .irs-single { font-weight: bold !important; font-size: 13px !important; border-radius: 4px !important; }
        
        /* Modal Table & Accordion Styling */
        .modal-rpe-table { width: 100%; border-collapse: separate; border-spacing: 0; border-radius: 8px; overflow: hidden; border: 1px solid #e2e8f0; }
        .modal-rpe-table th { background: #0f172a; color: #f8fafc; font-weight: 600; font-size: 12px; padding: 10px 12px; text-transform: uppercase; letter-spacing: 0.5px; border: none; }
        .modal-rpe-table td { padding: 10px 12px; font-size: 13px; border-top: 1px solid #f1f5f9; vertical-align: middle; }
        .rpe-pill { display: inline-block; padding: 3px 8px; border-radius: 12px; font-size: 11px; font-weight: 700; text-align: center; }
        .rpe-pill-default { background: #e2e8f0; color: #475569; }
        .rpe-pill-info    { background: #e0f2fe; color: #0369a1; }
        .rpe-pill-success { background: #dcfce7; color: #15803d; }
        .rpe-pill-warning { background: #fef3c7; color: #b45309; }
        .rpe-pill-danger  { background: #fee2e2; color: #b91c1c; }
        
        /* Accordion Custom Styling */
        .panel-group .panel { border-radius: 6px; border: 1px solid #cbd5e1; margin-bottom: 10px; }
        .panel-default > .panel-heading { background-color: #f8fafc; color: #1e293b; font-weight: 600; }
        
        /* Table Headers Badges */
        th.lab-header { background-color: #eff6ff !important; color: #1e40af !important; border-bottom: 2px solid #3b82f6 !important; }
        th.vital-header { background-color: #f0fdf4 !important; color: #166534 !important; border-bottom: 2px solid #22c55e !important; }

        /* =================================================================== */
        /* RIGOROUS MEDICAL REPORT STYLING (PAGE 4 ENHANCEMENT)               */
        /* =================================================================== */
        .medical-report-paper {
          background: #ffffff;
          padding: 30px;
          border-radius: 4px;
          box-shadow: 0 4px 20px rgba(0,0,0,0.08);
          font-family: 'Segoe UI', -apple-system, BlinkMacSystemFont, Roboto, sans-serif;
          color: #1e293b;
          max-width: 960px;
          margin: 0 auto;
          border: 1px solid #cbd5e1;
        }

        .med-header-container {
          border-bottom: 2px solid #0f172a;
          padding-bottom: 15px;
          margin-bottom: 20px;
          display: flex;
          justify-content: space-between;
          align-items: flex-end;
        }

        .med-hospital-title {
          font-size: 20px;
          font-weight: 800;
          color: #0f172a;
          letter-spacing: -0.3px;
          margin: 0;
          text-transform: uppercase;
        }

        .med-sub-title {
          font-size: 12px;
          color: #475569;
          margin: 3px 0 0 0;
          font-weight: 600;
        }

        .med-meta-box {
          text-align: right;
          font-size: 11px;
          color: #64748b;
          line-height: 1.4;
        }

        .med-patient-grid {
          display: grid;
          grid-template-columns: repeat(3, 1fr);
          gap: 10px 15px;
          background-color: #f8fafc;
          border: 1px solid #e2e8f0;
          padding: 12px 16px;
          border-radius: 6px;
          margin-bottom: 20px;
          font-size: 12px;
        }

        .med-patient-item span {
          color: #64748b;
          display: block;
          font-size: 10px;
          text-transform: uppercase;
          font-weight: 700;
          letter-spacing: 0.5px;
        }

        .med-patient-item strong {
          color: #0f172a;
          font-size: 13px;
        }

        .med-section-title {
          font-size: 13px;
          font-weight: 700;
          color: #0f172a;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          border-left: 4px solid #1e3c72;
          padding-left: 8px;
          margin: 20px 0 10px 0;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }

        .med-table {
          width: 100%;
          border-collapse: collapse;
          margin-bottom: 15px;
          font-size: 12px;
        }

        .med-table th {
          background-color: #f1f5f9;
          color: #334155;
          font-weight: 700;
          text-align: left;
          padding: 8px 10px;
          border: 1px solid #cbd5e1;
          font-size: 11px;
          text-transform: uppercase;
        }

        .med-table td {
          padding: 8px 10px;
          border: 1px solid #cbd5e1;
          color: #1e293b;
        }

        .med-fitt-grid {
          display: grid;
          grid-template-columns: repeat(2, 1fr);
          gap: 10px;
          margin-bottom: 15px;
        }

        .med-fitt-box {
          border: 1px solid #cbd5e1;
          border-left: 4px solid #2563eb;
          padding: 10px;
          border-radius: 4px;
          background: #fafafa;
        }

        .med-fitt-box h5 {
          margin: 0 0 4px 0;
          font-size: 11px;
          font-weight: 700;
          color: #1e40af;
          text-transform: uppercase;
        }

        .med-fitt-box p {
          margin: 0;
          font-size: 12px;
          color: #334155;
          line-height: 1.4;
        }

        .med-status-badge {
          display: inline-block;
          padding: 3px 8px;
          border-radius: 4px;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.3px;
        }

        .med-badge-safe { background-color: #dcfce7; color: #15803d; border: 1px solid #86efac; }
        .med-badge-warn { background-color: #fef3c7; color: #b45309; border: 1px solid #fde047; }
        .med-badge-danger { background-color: #fee2e2; color: #b91c1c; border: 1px solid #fca5a5; }

        .med-signature-grid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 40px;
          margin-top: 40px;
          padding-top: 15px;
          border-top: 1px solid #e2e8f0;
          page-break-inside: avoid;
        }

        .med-signature-line {
          border-bottom: 1px solid #0f172a;
          margin-top: 40px;
          margin-bottom: 5px;
        }

        .med-signature-title {
          font-size: 11px;
          font-weight: 700;
          color: #0f172a;
          text-transform: uppercase;
        }

        .med-signature-sub {
          font-size: 10px;
          color: #64748b;
        }

        .med-footer-note {
          margin-top: 25px;
          text-align: center;
          font-size: 10px;
          color: #94a3b8;
          border-top: 1px dashed #cbd5e1;
          padding-top: 8px;
        }

        .med-alert-list { margin: 0 0 8px 0; padding-left: 20px; font-size: 12px; }
        .med-alert-list li { margin-bottom: 2px; }
        .med-subhead { font-size: 11px; font-weight: 700; text-transform: uppercase; color: #334155; margin: 8px 0 4px 0; }

        /* PRINT MEDIA STYLES (PDF Export) */
        @media print {
          body { background: white !important; font-size: 10pt; color: #000; }
          .main-header, .main-sidebar, .no-print, .btn, .alert-cta-group, .content-header { display: none !important; }
          .content-wrapper, .right-side, .main-footer { margin-left: 0 !important; background: white !important; padding: 0 !important; }
          .box { border: none !important; box-shadow: none !important; padding: 0 !important; margin: 0 !important; }
          .medical-report-paper { border: none !important; padding: 0 !important; box-shadow: none !important; width: 100% !important; max-width: 100% !important; }
          .med-patient-grid, .med-table th, .med-fitt-box { background-color: #f8fafc !important; -webkit-print-color-adjust: exact; }
          .med-section-title, .med-signature-grid { page-break-inside: avoid !important; }
        }
        /* =================================================================== */
        /* v6 CLINICAL THEME: flat, tabular, colour only for abnormal values   */
        /* =================================================================== */
        :root { --accent: #0f6a8b; --ink: #1f2937; --muted: #6b7280; --line: #dfe3e8; --bg: #f5f7f9; }
        body, .content-wrapper, .right-side { background: var(--bg) !important; font-family: 'Segoe UI', 'Microsoft YaHei', Arial, sans-serif; color: var(--ink); font-size: 13px; font-variant-numeric: tabular-nums; }
        .content { padding: 14px 16px; }
        h4, h5 { color: var(--ink); }
        .muted { color: var(--muted); font-weight: 400; }
        .ok-text { color: #6b7280; }

        /* Header */
        .skin-blue .main-header .logo { background: #fff !important; color: var(--accent) !important; font-weight: 700; font-size: 16px; border-bottom: 1px solid var(--line); border-right: 1px solid var(--line); }
        .skin-blue .main-header .logo:hover { background: #fff !important; }
        .skin-blue .main-header .navbar { background: #fff !important; border-bottom: 1px solid var(--line); }
        .skin-blue .main-header .navbar .sidebar-toggle { color: var(--muted); }
        .skin-blue .main-header .navbar .sidebar-toggle:hover { background: #f3f6f8 !important; color: var(--accent); }
        .hdr-chip { display: inline-block; margin: 15px 16px 0 0; font-size: 12.5px; font-weight: 600; color: #374151; }
        .hdr-chip .fa { display: none; }
        .btn-guide { background: #fff !important; border: 1px solid #c9d1d9 !important; color: #374151 !important; padding: 4px 12px; margin: 10px 14px 0 0; }
        .btn-guide:hover { background: #f3f6f8 !important; }

        /* Sidebar */
        .skin-blue .main-sidebar, .skin-blue .left-side { background: #fff; }
        .main-sidebar { border-right: 1px solid var(--line); }
        .skin-blue .sidebar a { color: #374151; }
        .skin-blue .sidebar-menu > li > a { padding: 10px 16px; margin: 0; border-radius: 0; border-left: 3px solid transparent; font-weight: 600; font-size: 13px; }
        .skin-blue .sidebar-menu > li:hover > a { background: #f3f6f8; color: var(--accent); border-left-color: transparent; }
        .skin-blue .sidebar-menu > li.active > a { background: #eaf3f7; color: var(--accent); border-left: 3px solid var(--accent); }
        .skin-blue .sidebar-menu > li > a > .fa { width: 22px; }
        .patient-card { margin: 0; padding: 14px; background: #fff; border: 0; border-bottom: 1px solid var(--line); border-radius: 0; color: var(--ink); }
        .patient-card h5 { color: var(--muted) !important; font-size: 11px !important; font-weight: 700; text-transform: uppercase; letter-spacing: .06em; }
        .patient-card h5 .fa { display: none; }
        .patient-card input.form-control { background: #fff; border: 1px solid #c9d1d9; color: var(--ink); font-size: 12.5px; }
        .patient-card .form-group { margin-bottom: 8px; }
        .progress-card { margin: 0; padding: 14px; border: 0; border-top: 1px solid var(--line); border-radius: 0; background: #fff; }
        .pg-title { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: .06em; color: var(--muted); margin-bottom: 6px; }
        .pg-row { display: flex; align-items: center; gap: 8px; font-size: 12px; padding: 3px 0; color: #6b7280; }
        .pg-dot { width: 16px; height: 16px; border-radius: 50%; display: inline-flex; align-items: center; justify-content: center; font-size: 9px; font-weight: 700; background: #e5e7eb; color: #6b7280; flex-shrink: 0; }
        .pg-done { color: #1f2937; } .pg-done .pg-dot { background: #2e7d32; color: #fff; }
        .pg-warn { color: #8a5a00; } .pg-warn .pg-dot { background: #e0a100; color: #fff; }
        .pg-bad { color: #a31515; } .pg-bad .pg-dot { background: #c62828; color: #fff; }

        /* Boxes: flat panels */
        .box { border-radius: 6px !important; border: 1px solid var(--line) !important; border-top: 1px solid var(--line) !important; box-shadow: none !important; background: #fff; margin-bottom: 14px; }
        .box.box-solid > .box-header { background: #fafbfc !important; color: var(--ink) !important; border-bottom: 1px solid var(--line); border-radius: 6px 6px 0 0; padding: 10px 16px; }
        .box.box-solid > .box-header .box-title { font-size: 12.5px; font-weight: 700; text-transform: uppercase; letter-spacing: .05em; color: #374151; }
        .box-body { padding: 14px 16px; border-radius: 0 0 6px 6px; }

        /* Inputs */
        .form-control, .selectize-input { border-radius: 4px !important; border: 1px solid #c9d1d9; box-shadow: none; font-size: 13px; }
        .form-control { height: 32px; padding: 4px 8px; }
        textarea.form-control { height: auto; }
        .selectize-input { min-height: 32px; padding: 5px 8px; }
        .form-control:focus, .selectize-input.focus { border-color: var(--accent); box-shadow: 0 0 0 2px rgba(15,106,139,.15); }
        .form-group { margin-bottom: 8px; }
        .control-label, .form-group > label { display: block; max-width: 100%; font-size: 11.5px; font-weight: 600; color: #4b5563; margin-bottom: 2px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
        .checkbox label, .radio label { font-size: 13px; font-weight: 400; color: #374151; white-space: normal; }
        .form-group .checkbox, .shiny-options-group .checkbox, .shiny-options-group .radio { margin: 0 0 6px 0; }
        .shiny-options-group { display: grid; grid-template-columns: 1fr 1fr; gap: 0 16px; align-items: start; }
        .form-grid { display: grid !important; grid-template-rows: none !important; grid-auto-flow: row !important; grid-template-columns: repeat(var(--cols, 2), minmax(0, 1fr)); gap: 0 10px; }
        .form-grid.cols-3 { --cols: 3; } .form-grid.cols-4 { --cols: 4; }
        .form-section-title { display: block; font-size: 11px; font-weight: 700; letter-spacing: .06em; text-transform: uppercase; color: #4b5563; background: none; border: 0; border-bottom: 1px solid var(--line); border-radius: 0; padding: 0 0 4px 0; margin: 16px 0 8px 0; }
        .form-section-title .fa { display: none; }
        .form-section-title.fs-danger { color: #a31515; border-bottom-color: #f1c0c0; }
        .is-invalid { border-color: #c62828 !important; background: #fff6f6 !important; box-shadow: 0 0 0 2px rgba(198,40,40,.15) !important; }
        .value-badge { border-radius: 3px; font-size: 11.5px; font-weight: 600; padding: 3px 8px; }

        /* Buttons */
        .btn { border-radius: 4px; font-weight: 600; box-shadow: none; }
        .btn-primary, .btn-primary:focus { background: var(--accent); border: 1px solid var(--accent); color: #fff; }
        .btn-primary:hover, .btn-primary:active { background: #0c5873; border-color: #0c5873; color: #fff; }
        .btn-default { background: #fff; border: 1px solid #c9d1d9; color: #374151; } .btn-default:hover { background: #f3f6f8; }
        .btn-success { background: #2e7d32; border: 1px solid #2e7d32; color: #fff; } .btn-success:hover { background: #256628; color: #fff; }
        .btn-warning { background: #e0a100; border: 1px solid #e0a100; color: #fff; }
        .btn-danger { background: #c62828; border: 1px solid #c62828; color: #fff; }
        .btn-block { padding: 9px 12px; font-size: 13.5px; }
        .sticky-action { position: sticky; bottom: 0; z-index: 6; background: #fff; border-top: 1px solid var(--line); padding: 10px 0 2px 0; margin-top: 10px; }
        .next-step { display: flex; align-items: center; justify-content: space-between; gap: 12px; margin-top: 14px; padding: 10px 14px; border: 1px solid var(--line); background: #fafbfc; border-radius: 4px; font-size: 12.5px; color: #4b5563; }

        /* Alerts & empty state */
        .clinical-alert { border-radius: 4px; font-weight: 500; font-size: 13px; padding: 10px 14px; margin-bottom: 12px; }
        .empty-state { padding: 34px 16px; text-align: center; color: var(--muted); border: 1px dashed #cfd6dd; border-radius: 6px; background: #fff; }
        .es-icon { display: none; }
        .es-title { font-size: 14px; font-weight: 700; color: #374151; }
        .es-text { font-size: 12.5px; margin-top: 4px; }

        /* Status banner */
        .st-banner { border: 1px solid var(--line); border-left: 5px solid #9aa5b1; border-radius: 4px; background: #fff; padding: 12px 16px; margin-bottom: 12px; }
        .st-ok { border-left-color: #2e7d32; background: #f6fbf6; }
        .st-warn { border-left-color: #e0a100; background: #fffaf0; }
        .st-danger { border-left-color: #c62828; background: #fff6f6; }
        .st-label { font-size: 10.5px; font-weight: 700; letter-spacing: .07em; text-transform: uppercase; color: var(--muted); }
        .st-head { font-size: 17px; font-weight: 700; margin: 2px 0; }
        .st-ok .st-head { color: #1b5e20; } .st-warn .st-head { color: #8a5a00; } .st-danger .st-head { color: #a31515; }
        .st-detail { font-size: 12.5px; color: #4b5563; }
        .st-list { margin: 8px 0 0 0; padding: 0; list-style: none; font-size: 12.5px; }
        .st-list li { padding: 4px 0; border-top: 1px solid rgba(0,0,0,.07); }

        /* Tags */
        .tg { display: inline-block; font-size: 10px; font-weight: 700; letter-spacing: .05em; text-transform: uppercase; padding: 1px 7px; border-radius: 3px; border: 1px solid transparent; margin-right: 8px; white-space: nowrap; }
        .tg-stop { background: #fde8e8; color: #a31515; border-color: #f1b5b5; }
        .tg-caution { background: #fff1d6; color: #8a5a00; border-color: #efc46b; }
        .tg-ok { background: #f1f8f1; color: #2e7d32; border-color: #b7dcb9; }

        /* Tables */
        .cl-table { width: 100%; border-collapse: collapse; font-size: 13px; margin-bottom: 8px; }
        .cl-table th { font-size: 10.5px; font-weight: 700; letter-spacing: .05em; text-transform: uppercase; color: var(--muted); text-align: left; padding: 6px 10px; border-bottom: 2px solid #cfd6dd; background: #fafbfc; }
        .cl-table td { padding: 7px 10px; border-bottom: 1px solid #edf0f3; vertical-align: top; }
        .cl-table td.num { font-weight: 600; white-space: nowrap; }
        .cl-table tr.row-warn td { background: #fffaf0; }
        .cl-table tr.row-danger td { background: #fff6f6; }
        .cl-kv td:first-child { width: 120px; color: var(--muted); font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: .05em; }
        .section-label { display: flex; align-items: center; gap: 8px; flex-wrap: wrap; font-size: 11px; font-weight: 700; letter-spacing: .06em; text-transform: uppercase; color: #4b5563; margin: 16px 0 6px 0; padding-bottom: 4px; border-bottom: 1px solid var(--line); }
        .section-label .fa { display: none; }
        .evidence-pill { margin-left: auto; font-size: 10.5px; font-weight: 600; color: var(--accent); background: none; border: 1px solid #b9d4df; border-radius: 3px; padding: 1px 7px; text-transform: none; letter-spacing: 0; }
        .sub-pop { font-size: 12.5px; color: #4b5563; margin-bottom: 6px; }
        .sum-strip { display: flex; flex-wrap: wrap; border: 1px solid var(--line); border-radius: 4px; background: #fff; margin-bottom: 12px; }
        .sum-item { flex: 1; min-width: 120px; padding: 8px 14px; border-right: 1px solid var(--line); }
        .sum-item:last-child { border-right: 0; }
        .sum-label { font-size: 10px; font-weight: 700; letter-spacing: .06em; text-transform: uppercase; color: var(--muted); }
        .sum-val { font-size: 15px; font-weight: 700; }
        .sum-sub { font-size: 11px; color: var(--muted); }
        .v-ok { color: #1b5e20; } .v-warn { color: #8a5a00; } .v-danger { color: #a31515; }
        .note-list { margin: 0 0 8px 0; padding: 0; list-style: none; }
        .note-list li { padding: 6px 0; border-bottom: 1px solid #edf0f3; font-size: 13px; }
        .action-block { border: 1px solid #efc46b; border-left: 4px solid #e0a100; background: #fffaf0; border-radius: 4px; padding: 8px 12px; margin-bottom: 8px; font-size: 12.5px; }
        .action-block ol { margin: 4px 0 0 0; padding-left: 18px; }
        details.cdss-details { border: 1px solid var(--line); border-radius: 4px; margin-bottom: 6px; background: #fff; }
        details.cdss-details summary { cursor: pointer; padding: 8px 12px; font-weight: 600; color: var(--ink); list-style: none; font-size: 12.5px; }
        details.cdss-details summary::-webkit-details-marker { display: none; }
        details.cdss-details summary:before { content: '\\25B8  '; color: var(--muted); }
        details.cdss-details[open] summary:before { content: '\\25BE  '; }
        details.cdss-details .details-body { padding: 0 14px 8px 14px; font-size: 12.5px; }
        details.cdss-details .details-body ul { padding-left: 18px; margin-bottom: 0; }
        details.cdss-details .details-body li { margin-bottom: 4px; }
        .count-pill { background: #eef1f4; border-radius: 3px; font-size: 11px; padding: 0 7px; margin-left: 6px; color: #4b5563; }
        .two-col-list { column-count: 2; column-gap: 28px; padding-left: 18px; margin: 0 0 8px 0; font-size: 12px; }
        .two-col-list li { margin-bottom: 4px; break-inside: avoid; }
        @media (max-width: 800px) { .two-col-list { column-count: 1; } }

        /* RPE slider: neutral track, status as text */
        .rpe-card-container { border: 1px solid var(--line); border-radius: 4px; box-shadow: none; padding: 12px 14px; }
        .btn-rpe-guide { background: #fff; color: var(--accent); border: 1px solid #b9d4df; border-radius: 4px; }
        .irs-line { background: #d6dde4 !important; height: 6px !important; border-radius: 3px !important; }
        .irs-single { background: var(--accent) !important; }
        .rpe-line { font-size: 12.5px; padding: 4px 0 0 0; color: #374151; }
        .rpe-line .dot { display: inline-block; width: 9px; height: 9px; border-radius: 50%; margin-right: 8px; vertical-align: -1px; }

        /* Data tables, modal, report */
        table.dataTable thead th { background: #fafbfc; color: #4b5563; font-size: 11px; text-transform: uppercase; letter-spacing: .04em; border-bottom: 2px solid #cfd6dd !important; }
        table.dataTable tbody tr:hover { background: #f3f8fa !important; }
        .dataTables_wrapper .dataTables_paginate .paginate_button.current { background: var(--accent) !important; color: #fff !important; border: 0 !important; border-radius: 4px; }
        .modal-content { border-radius: 6px; border: 1px solid var(--line); box-shadow: 0 10px 30px rgba(15,23,42,.2); }
        .medical-report-paper { border-radius: 4px; box-shadow: none; border: 1px solid var(--line); }
        .med-header-container { border-bottom: 2px solid #1f2937 !important; }
        .med-patient-grid { background: #fff !important; border: 1px solid var(--line); border-radius: 4px; }
        .med-section-title { border-left: 0 !important; background: none !important; border-bottom: 1px solid #9aa5b1; padding: 0 0 4px 0 !important; margin: 18px 0 8px 0 !important; font-size: 12px; letter-spacing: .06em; }
        @media print {
          .sticky-action, .next-step { display: none !important; }
          .st-banner, .tg, .cl-table tr, .sum-strip, .action-block { -webkit-print-color-adjust: exact !important; print-color-adjust: exact !important; }
          .cl-table tr, .st-banner, .action-block { break-inside: avoid; }
        }

        /* =================================================================== */
        /* v7 READABILITY: larger type, more whitespace, sentence case         */
        /* =================================================================== */
        body, .content-wrapper, .right-side { font-size: 14px; line-height: 1.5; }
        .content { padding: 18px 22px; }
        .box-body { padding: 18px 22px; }
        .box.box-solid > .box-header { padding: 14px 22px; }
        .box.box-solid > .box-header .box-title { font-size: 15px; font-weight: 700; text-transform: none; letter-spacing: 0; color: #111827; }
        .form-section-title { font-size: 13.5px; font-weight: 700; text-transform: none; letter-spacing: 0; color: #111827; margin: 26px 0 12px 0; padding-bottom: 6px; }
        .form-section-title.fs-danger { color: #b42318; }
        .control-label, .form-group > label { font-size: 12.5px; font-weight: 500; color: #4b5563; margin-bottom: 4px; }
        .form-group { margin-bottom: 12px; }
        .form-grid { gap: 0 14px; }
        .form-control { height: 36px; }
        .checkbox label, .radio label { font-size: 14px; line-height: 1.4; }
        .form-group .checkbox, .shiny-options-group .checkbox, .shiny-options-group .radio { margin: 0 0 10px 0; }
        .section-label { font-size: 14px; font-weight: 700; text-transform: none; letter-spacing: 0; color: #111827; margin: 28px 0 10px 0; padding-bottom: 6px; }
        .section-label:first-child { margin-top: 4px; }
        .cl-table th { font-size: 12px; font-weight: 600; text-transform: none; letter-spacing: 0; color: #6b7280; padding: 8px 12px; }
        .cl-table td { padding: 12px 12px; line-height: 1.45; }
        .cl-table td.nw, .cl-table td.num { white-space: nowrap; }
        .cl-table td.ref { color: #6b7280; font-size: 12.5px; }
        .cl-table .unit { color: #9ca3af; font-size: 12px; }
        .cl-kv td:first-child { width: 110px; font-size: 12.5px; font-weight: 600; text-transform: none; letter-spacing: 0; }
        .kv-detail { color: #6b7280; font-size: 12.5px; margin-top: 3px; line-height: 1.5; }
        .tg, .ok-text { white-space: nowrap; }
        .tg { font-size: 11.5px; font-weight: 600; text-transform: none; letter-spacing: 0; padding: 2px 8px; }
        .st-banner { padding: 16px 20px; margin-bottom: 20px; }
        .st-label { font-size: 12px; font-weight: 600; text-transform: none; letter-spacing: 0; }
        .st-head { font-size: 19px; margin: 3px 0 4px 0; }
        .st-detail { font-size: 13.5px; }
        .st-list { margin-top: 12px; }
        .st-list li { padding: 9px 0; }
        .sum-item { padding: 12px 18px; }
        .sum-label { font-size: 12px; font-weight: 600; text-transform: none; letter-spacing: 0; }
        .sum-val { font-size: 16px; }
        .evidence-pill { font-size: 11.5px; }
        .sub-pop { font-size: 13px; margin-bottom: 10px; }
        details.cdss-details { margin-top: 12px; }
        details.cdss-details summary { font-size: 13.5px; padding: 11px 14px; }
        .next-step { margin-top: 24px; padding: 14px 18px; font-size: 13.5px; }
        .note-list li { padding: 11px 0; line-height: 1.5; }
        .action-block { padding: 12px 16px; margin-bottom: 12px; font-size: 13.5px; }
        .med-section-title { font-size: 14px !important; text-transform: none !important; letter-spacing: 0 !important; margin: 30px 0 10px 0 !important; }
        .pg-title { text-transform: none; letter-spacing: 0; font-size: 12px; }
        .patient-card h5 { text-transform: none; letter-spacing: 0; font-size: 12.5px !important; }

        /* v9 additions */
        .ns-block { border: 1px solid var(--line); border-left: 4px solid #c62828; border-radius: 4px; padding: 12px 16px; margin-bottom: 12px; background: #fff; }
        .ns-block .cl-table { margin: 8px 0 0 0; }
        .shiny-date-input.is-invalid input { border-color: #c62828 !important; background: #fff6f6 !important; }
        select.is-invalid + .selectize-control .selectize-input { border-color: #c62828 !important; background: #fff6f6 !important; }
        .patient-card .btn { margin-top: 4px; }
        @media print { .ns-block { break-inside: avoid; page-break-inside: avoid; border: 1px solid #999; padding: 6px 10px; margin-bottom: 6px; } }

        /* =================================================================== */
        /* v8 PRINT LAYOUT: A4, compact, sections never split across pages     */
        /* =================================================================== */
        .unit-field .radio-inline { margin-top: 7px; }
        @page { size: A4; margin: 8mm 12mm 9mm 12mm; }
        @media print {
          html, body, .content-wrapper, .right-side, .content { background: #fff !important; padding: 0 !important; margin: 0 !important; }
          body { font-size: 11px; line-height: 1.4; }
          .medical-report-paper { border: 0 !important; box-shadow: none !important; padding: 0 !important; margin: 0 !important; max-width: none !important; width: 100% !important; border-radius: 0 !important; }
          .print-block { break-inside: avoid; page-break-inside: avoid; }
          .med-section-title { break-after: avoid; page-break-after: avoid; margin: 14px 0 6px 0 !important; font-size: 12px !important; }
          .med-header-container { margin-bottom: 8px !important; padding-bottom: 6px !important; }
          .med-patient-grid { padding: 8px 10px !important; gap: 6px 12px !important; margin-bottom: 8px !important; }
          .sum-strip { margin-bottom: 6px; }
          .sum-item { padding: 6px 10px; }
          .sum-label { font-size: 10px; }
          .sum-val { font-size: 13px; }
          .cl-table { font-size: 11px; margin-bottom: 4px; }
          .cl-table th { padding: 4px 8px; font-size: 10px; }
          .cl-table td { padding: 5px 8px; line-height: 1.3; }
          .cl-table tr { break-inside: avoid; page-break-inside: avoid; }
          .cl-kv td:first-child { width: 90px; font-size: 10.5px; }
          .kv-detail { font-size: 10px; margin-top: 1px; }
          .section-label { margin: 10px 0 4px 0; font-size: 11.5px; }
          .note-list li { padding: 5px 0; }
          .tg { font-size: 10px; padding: 1px 6px; }
          .two-col-list { font-size: 10.5px; }
          .action-block { padding: 6px 10px; margin-bottom: 6px; font-size: 11px; }
          .sub-pop { font-size: 11px; margin-bottom: 4px; }
          .med-signature-grid { margin-top: 18px !important; break-inside: avoid; page-break-inside: avoid; }
          .med-footer-note { margin-top: 10px !important; }
          .cl-table { font-size: 10.5px !important; }
          .cl-table td .kv-detail { display: inline; margin-left: 6px; }
          .cl-table td.ref { font-size: 10.5px !important; }
          .cl-kv td .kv-detail { display: block; margin-left: 0; }
          .sub-pop { display: none; }
          .st-list { margin-top: 2px !important; }
          .st-list li { padding: 3px 0 !important; font-size: 11px !important; }
          .cl-table td { padding: 2px 8px !important; }
          .med-signature-line { height: 16px !important; }
          .med-footer-note { margin-top: 6px !important; padding-top: 4px !important; }
          .section-label { margin: 6px 0 2px 0 !important; }
          .med-section-title { margin: 8px 0 3px 0 !important; }
          .sum-item { padding: 4px 10px !important; }
          .med-patient-grid { padding: 6px 10px !important; }
          .med-signature-grid { margin-top: 12px !important; }
          .med-hospital-title { font-size: 15px !important; line-height: 1.25 !important; margin: 0 !important; }
          .med-sub-title { font-size: 10.5px !important; margin: 2px 0 0 0 !important; }
          .med-meta-box { font-size: 10px !important; line-height: 1.4 !important; }
          .cl-table td { padding: 3px 8px !important; line-height: 1.25 !important; }
          .cl-table th { padding: 3px 8px !important; }
          .section-label { margin: 8px 0 3px 0 !important; }
          .med-section-title { margin: 10px 0 4px 0 !important; }
          .print-flow .section-label, .print-flow .med-section-title { break-after: avoid; page-break-after: avoid; }
          .two-col-list li, .action-block, .st-list li { break-inside: avoid; page-break-inside: avoid; }
        }

        /* =================================================================== */
        /* v10 PRINT POLISH: letterhead, running header / footer, quiet tables  */
        /* =================================================================== */
        @page { size: A4; margin: 16mm 14mm 15mm 14mm; }
        .rpt-brand { display: flex; align-items: center; gap: 12px; }
        .rpt-logo { height: 44px; width: auto; }
        .rpt-org { font-size: 10.5px; font-weight: 700; letter-spacing: .12em; text-transform: uppercase; color: var(--accent); margin-bottom: 2px; }
        @media print {
          .med-header-container { border-bottom: 2px solid #0f6a8b !important; }
          .rpt-logo { height: 38px; }
          .med-hospital-title { font-size: 13.5px !important; letter-spacing: 0 !important; }
          .med-meta-box { min-width: 150px; }
          .rpt-org { font-size: 9px; }
          .med-section-title { color: #0f6a8b !important; border-bottom: 1px solid #b8cad4 !important; font-weight: 700; }
          .section-label { color: #374151 !important; border-bottom: 1px solid #e1e8ec !important; }
          .cl-table th { background: none !important; border-bottom: 1px solid #8aa0ad !important; color: #51606b !important; }
          .cl-table td { border-bottom: 1px solid #e6ebee !important; }
          .cl-table tr.row-warn td { background: #fbf7ef !important; }
          .cl-table tr.row-danger td { background: #fcf3f3 !important; }
          .tg { background: #fff !important; }
          .tg-stop { border-color: #b42318 !important; color: #b42318 !important; }
          .tg-caution { border-color: #b7791f !important; color: #8a5a00 !important; }
          .tg-ok { border-color: #8fc59a !important; color: #2e7d32 !important; }
          .sum-strip { border: 0 !important; border-top: 2px solid #0f6a8b !important; border-bottom: 1px solid #d5dee3 !important; border-radius: 0 !important; background: #f7fafb !important; }
          .sum-item { border-right: 1px solid #e1e8ec !important; }
          .med-patient-grid { background: #f7fafb !important; border: 0 !important; border-bottom: 1px solid #d5dee3 !important; border-radius: 0 !important; }
          .ns-block { border: 1px solid #c9d1d6 !important; border-left: 3px solid #b42318 !important; border-radius: 2px !important; }
          .med-footer-note { border-top: 1px solid #d5dee3 !important; font-size: 8.5px !important; color: #6b7280 !important; }
        }

        /* =================================================================== */
        /* v11 PRINT TYPOGRAPHY: one type scale, clear section bands            */
        /*   title 16 | section band 12 | table text 10.5 | secondary 9.5      */
        /* =================================================================== */
        .rpt-logo { height: 50px; width: auto; flex: 0 0 auto; }
        .med-header-container { display: block !important; }
        .rpt-brand > div { min-width: 0; flex: 1 1 auto; }
        .med-meta-box { display: flex !important; flex-wrap: wrap !important; gap: 2px 22px !important; text-align: left !important; margin-top: 8px !important; line-height: 1.5 !important; }
        .med-meta-box > div { white-space: nowrap !important; }
        @media print {
          .medical-report-paper { font-size: 10.5px !important; line-height: 1.5 !important; color: #1f2937 !important; }
          .rpt-logo { height: 44px !important; }
          .rpt-org { font-size: 9px !important; letter-spacing: .14em !important; }
          .med-hospital-title { font-size: 16px !important; line-height: 1.25 !important; letter-spacing: 0 !important; }
          .med-sub-title { font-size: 10px !important; }
          .med-meta-box, .med-meta-box * { font-size: 9.5px !important; }
          .med-patient-item span { font-size: 9px !important; letter-spacing: .06em !important; }
          .med-patient-item strong { font-size: 11.5px !important; }
          .sum-label { font-size: 9px !important; }
          .sum-val { font-size: 12px !important; }
          .sum-sub { font-size: 9.5px !important; }
          .med-section-title { background: #eaf2f6 !important; border: 0 !important; border-left: 4px solid #0f6a8b !important; padding: 5px 10px !important; margin: 20px 0 9px 0 !important; font-size: 12px !important; font-weight: 700 !important; color: #0b4f68 !important; text-transform: uppercase !important; letter-spacing: .05em !important; }
          .section-label { font-size: 10.5px !important; font-weight: 700 !important; color: #374151 !important; margin: 12px 0 5px 0 !important; padding-bottom: 3px !important; text-transform: none !important; letter-spacing: 0 !important; }
          .cl-table th { font-size: 9.5px !important; font-weight: 700 !important; text-transform: uppercase !important; letter-spacing: .04em !important; padding: 4px 8px !important; }
          .cl-table td { font-size: 10.5px !important; padding: 4px 8px !important; line-height: 1.4 !important; }
          .cl-table td.ref, .cl-table .unit, .kv-detail, .cl-table td .kv-detail { font-size: 9.5px !important; }
          .cl-kv td:first-child { font-size: 9.5px !important; text-transform: uppercase !important; letter-spacing: .04em !important; }
          .tg { font-size: 9px !important; }
          .ok-text { font-size: 10px !important; }
          .ns-block { padding: 8px 10px !important; margin-bottom: 8px !important; }
          .ns-block strong, .ns-block div, .ns-block li { font-size: 10.5px !important; }
          .note-list li, .two-col-list, .two-col-list li, .st-list li { font-size: 10.5px !important; }
          .evidence-pill { font-size: 9px !important; }
          .med-footer-note { font-size: 8.5px !important; }
          .med-signature-title { font-size: 10px !important; }
          .med-signature-sub { font-size: 9px !important; }
        }

        /* v12 PRINT REFINEMENT: aligned left edge, quieter labels, more air */
        @media print {
          .cl-table td:first-child, .cl-table th:first-child { padding-left: 0 !important; }
          .cl-table td:last-child, .cl-table th:last-child { padding-right: 0 !important; }
          .cl-table td { padding-top: 5px !important; padding-bottom: 5px !important; }
          .cl-table th { text-transform: none !important; letter-spacing: 0 !important; font-size: 10px !important; font-weight: 600 !important; color: #6b7280 !important; }
          .cl-kv td:first-child { text-transform: none !important; letter-spacing: 0 !important; font-size: 10px !important; font-weight: 600 !important; color: #6b7280 !important; width: 96px !important; }
          .med-patient-item span { text-transform: none !important; letter-spacing: 0 !important; font-size: 9.5px !important; font-weight: 500 !important; color: #6b7280 !important; }
          .sum-label { text-transform: none !important; letter-spacing: 0 !important; font-size: 9.5px !important; font-weight: 500 !important; }
          .med-patient-grid { padding: 10px 14px !important; row-gap: 9px !important; }
          .sum-item { padding: 8px 14px !important; }
          .med-section-title { margin: 24px 0 10px 0 !important; }
          .section-label { margin: 14px 0 6px 0 !important; }
          .ns-block { padding: 10px 12px !important; margin-bottom: 10px !important; }
          .note-list li { padding: 6px 0 !important; }
          .med-signature-grid { margin-top: 30px !important; }
          .medical-report-paper p { margin: 0 0 6px 0; }
        }

      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('invalid', function(m) {
          document.querySelectorAll('.is-invalid').forEach(function(e) { e.classList.remove('is-invalid'); });
          var first = null;
          (m.ids || []).forEach(function(id) {
            var el = document.getElementById(id);
            if (el) { el.classList.add('is-invalid'); if (!first) first = el; }
          });
          if (first) {
            var tgt = (first.tagName === 'SELECT') ? first.parentElement : first;
            tgt.scrollIntoView({block: 'center', behavior: 'smooth'});
            if (first.tagName !== 'SELECT' && first.focus) first.focus();
          }
        });
        Shiny.addCustomMessageHandler('clear_date', function(id) {
          var inp = $('#' + id).find('input');
          try { inp.bsDatepicker('clearDates'); } catch (e) { inp.val(''); }
          inp.trigger('change');
        });
        document.addEventListener('input', function(e) { if (e.target && e.target.classList) e.target.classList.remove('is-invalid'); });
      "))
    ),
    
    tabItems(
      # TAB 1: PRE-TEST Assessment & FITT
      tabItem(
        tabName = "pre_eval",
        fluidRow(
          box(
            title = "Patient & baseline inputs", width = 5, status = "primary", solidHeader = TRUE,
            
            selectInput("ckd_category", "UKKA Clinical Sub-population:", 
                        choices = c("Select..." = "",
                                    "Non-Dialysis CKD (Stages 1-5)" = "non_dialysis",
                                    "Haemodialysis Patient (Intradialytic)" = "dialysis",
                                    "Peritoneal Dialysis Patient (PD)" = "peritoneal",
                                    "Pre-Transplant Candidate (Prehab)" = "pre_transplant",
                                    "Kidney Transplant Recipient" = "transplant")),
            
            conditionalPanel(
              condition = "input.ckd_category == 'dialysis'",
              selectInput("hd_vintage", "Time on haemodialysis:",
                          choices = c("Select..." = "",
                                      "Less than 3 months" = "under3m",
                                      "3 months or more" = "over3m")),
              numericInput("dial_time", "Time elapsed in today's session (hours):", value = NA, min = 0, max = 6.0, step = 0.5),
              selectInput("fistula_arm", "Vascular Access:",
                          choices = c("Select..." = "",
                                      "No AV access / not recorded" = "None",
                                      "AV Access - Left Arm" = "Left Arm",
                                      "AV Access - Right Arm" = "Right Arm",
                                      "Central Venous Catheter (CVC)" = "CVC")),
              conditionalPanel(
                condition = "input.fistula_arm == 'Left Arm' || input.fistula_arm == 'Right Arm'",
                selectInput("fistula_age", "AV Access Status:",
                            choices = c("Select..." = "",
                                        "New / maturing - not yet cleared for use" = "early",
                                        "Established - cleared for use" = "matured"))
              ),
              div(class = "form-grid",
                  numericInput("dry_weight", "Dry Weight (kg):", value = NA, min = 30.0, max = 150.0),
                  numericInput("pre_weight", "Pre-Weight (kg):", value = NA, min = 30.0, max = 150.0)
              ),
              uiOutput("idwg_display_ui")
            ),
            
            conditionalPanel(
              condition = "input.ckd_category == 'peritoneal'",
              checkboxInput("pd_full_abdomen", "Full Abdomen (Dialysate In Situ)", value = FALSE),
              checkboxInput("pd_hernia", "Known Abdominal / Inguinal Hernia", value = FALSE),
              selectInput("pd_catheter_status", "PD Catheter Exit-site Status:",
                          choices = c("Select..." = "",
                                      "Not yet healed (per the PD team)" = "healing",
                                      "Fully Healed & Intact" = "healed"))
            ),
            
            conditionalPanel(
              condition = "input.ckd_category == 'transplant'",
              selectInput("post_transplant_time", "Transplant team restrictions:",
                          choices = c("Select..." = "",
                                      "Restrictions still in force" = "early",
                                      "No restrictions from the transplant team" = "late"))
            ),
            
            div(class = "form-section-title", icon("user"), "Demographics"),
            div(class = "form-grid",
                numericInput("age", "Age (years):", value = NA, min = 18, max = 100),
                selectInput("gender", "Gender:", choices = c("Select..." = "", "Male" = "male", "Female" = "female"))
            ),
            
            div(class = "form-section-title", icon("vial"), "Blood results"),
            div(class = "form-grid cols-3",
                suppressWarnings(dateInput("lab_date", "Blood test date:", value = NA, max = Sys.Date(), format = "yyyy-mm-dd")),
                numericInput("k_level", "K+ mmol/L", value = NA, min = 2.0, max = 8.0, step = 0.1),
                numericInput("hb_level", "Hb g/dL", value = NA, min = 4.0, max = 18.0, step = 0.1)
            ),
            div(class = "form-grid",
                numericInput("scr", "Serum Creatinine (mg/dL):", value = NA, min = 0.2, max = 20.0, step = 0.1),
                div(class = "unit-field",
                    radioButtons("scr_unit", "Unit:", 
                                 choices = c("mg/dL" = "mgdl", "\u00B5mol/L" = "umol"), inline = TRUE))
            ),
            uiOutput("egfr_display_ui"),
            
            div(class = "form-section-title", icon("walking"), "Functional Capacity Baseline"),
            div(class = "form-grid cols-3",
                numericInput("mwt6_dist", "6MWT Distance (m):", value = NA, min = 0, max = 1000),
                numericInput("sts30_reps", "30s-STS (Reps):", value = NA, min = 0, max = 50),
                numericInput("handgrip_kg", "Handgrip (kg):", value = NA, min = 0, max = 100)
            ),
            p(style = "font-size: 11px; color: #64748b; margin-top: -4px;",
              "Leave blank if not done. Test handgrip on the non-access arm."),
            selectInput("start_level", "Starting level (clinician decision):",
                        choices = c("Standard (moderate intensity)" = "standard",
                                    "Light start (deconditioned / frail)" = "light")),
            uiOutput("start_level_hint_ui"),
            
            div(class = "form-section-title", icon("shoe-prints"), "Diabetic & Foot Risk"),
            checkboxInput("diabetic_neuropathy", "Peripheral Neuropathy Present", value = FALSE),
            checkboxInput("foot_ulcer", "Active Foot Ulcer / Wound Present", value = FALSE),
            checkboxInput("hypo_risk_meds", "On insulin or sulfonylurea (hypoglycaemia risk)", value = FALSE),
            
            div(class = "form-section-title", icon("heartbeat"), "Vital Signs (today)"),
            div(class = "form-grid cols-3",
                numericInput("sbp", "SBP (upper) mmHg", value = NA, min = 70, max = 220),
                numericInput("dbp", "DBP (lower) mmHg", value = NA, min = 40, max = 130),
                numericInput("hr", "HR bpm", value = NA, min = 40, max = 160),
                numericInput("spo2", "SpO2 %", value = NA, min = 70, max = 100),
                numericInput("temp", "Temp \u00B0C", value = NA, min = 35.0, max = 41.0, step = 0.1),
                numericInput("glucose", "Glucose mmol/L", value = NA, min = 2.0, max = 25.0, step = 0.5)
            ),
            uiOutput("bp_validation_ui"),
            
            div(class = "form-section-title", icon("smoking"), "Lifestyle Risk Factors"),
            checkboxGroupInput("lifestyle_risks", NULL,
                               choices = c("Smoking" = "smoking",
                                           "Alcohol >14 units/week" = "alcohol",
                                           "NSAIDs / herbal / nephrotoxins" = "nephrotoxic")),
            
            div(class = "form-section-title fs-danger", icon("exclamation-triangle"), "Red Flag Symptoms"),
            checkboxGroupInput("symptoms", NULL,
                               choices = c("Chest pain / angina" = "chest_pain",
                                           "Severe dyspnea" = "severe_sob",
                                           "Dizziness" = "dizziness",
                                           "Access bleeding / pain" = "fistula_bleeding",
                                           "Headache / visual disturbance (BP symptoms)" = "bp_symptoms",
                                           "Heart attack in the last 2 weeks" = "recent_mi",
                                           "Clinically unstable condition" = "unstable_condition")),
            div(class = "sticky-action",
                actionButton("run_cdss", "Evaluate PT Safety & Generate UKKA FITT", class = "btn-primary btn-block", icon = icon("stethoscope")))
          ),
          
          box(
            title = "Safety result & FITT protocol", width = 7, status = "info", solidHeader = TRUE,
            uiOutput("pre_stale_ui"),
            uiOutput("pause_banner_ui"),
            uiOutput("status_alert"),
            uiOutput("next_steps_output"),
            uiOutput("vitals_snapshot_ui"),
            uiOutput("fitt_output"),
            uiOutput("precautions_output"),
            uiOutput("lifestyle_output"),
            uiOutput("pre_next_ui"),
            
            conditionalPanel(
              condition = "output.handling_needed",
              div(class = "section-label", "Handling record"),
              p(class = "muted", style = "font-size: 12px; margin: 0 0 8px 0;",
                "Record who was informed and what was decided. Entries are kept in today's log and printed in the report."),
              div(class = "form-grid",
                  textInput("hd_who", "Informed (name / role):", placeholder = "e.g. Dr Lim, nephrology"),
                  div(textInput("hd_time", "Time informed (HH:MM):", placeholder = "HH:MM"),
                      actionLink("hd_now", "Use current time"))
              ),
              selectInput("hd_decision", "Decision / outcome:",
                          choices = c("Select..." = "", HANDLING_DECISIONS)),
              textInput("hd_reschedule", "Rescheduled to (date / session):", placeholder = "e.g. next HD session, 12 Nov"),
              textAreaInput("hd_notes", "Notes:", rows = 2, resize = "vertical"),
              actionButton("hd_save", "Save handling record", class = "btn-default"),
              uiOutput("handling_log_ui")
            ),
            
            uiOutput("eval_log_ui")
          )
        )
      ),
      
      # TAB 2: POST-TEST Discharge Safety
      tabItem(
        tabName = "post_eval",
        fluidRow(
          box(
            title = "Post-exercise vitals & symptoms", width = 5, status = "warning", solidHeader = TRUE,
            uiOutput("post_context_ui"),
            div(class = "form-section-title", icon("heartbeat"), "Post-Exercise Vital Signs"),
            div(class = "form-grid cols-4",
                numericInput("post_sbp", "SBP (upper) mmHg", value = NA, min = 60, max = 220),
                numericInput("post_dbp", "DBP (lower) mmHg", value = NA, min = 40, max = 130),
                numericInput("post_hr", "HR bpm", value = NA, min = 40, max = 180),
                numericInput("post_spo2", "SpO2 %", value = NA, min = 70, max = 100)
            ),
            
            # Borg RPE Panel
            div(class = "rpe-card-container",
                div(class = "rpe-header-flex",
                    h5("Post-Session Perceived Exertion (Borg RPE 6-20)", class = "rpe-title-text"),
                    actionButton("show_rpe_guide", " Borg Reference Table", class = "btn-rpe-guide", icon = icon("table"))
                ),
                selectInput("post_rpe_sel", NULL, choices = RPE_CHOICES, width = "100%"),
                uiOutput("rpe_status_badge_ui")
            ),
            
            div(class = "form-section-title fs-danger", icon("exclamation-triangle"), "Post-Exercise Adverse Symptoms"),
            checkboxGroupInput("post_symptoms", NULL,
                               choices = c("Dizziness" = "post_dizziness",
                                           "Severe fatigue" = "post_fatigue",
                                           "Muscle cramps" = "post_cramps",
                                           "Shortness of breath" = "post_sob")),
            div(class = "sticky-action",
                actionButton("run_post_cdss", "Evaluate Post-Exercise Discharge Safety", class = "btn-primary btn-block", icon = icon("file-medical-alt")))
          ),
          
          box(
            title = "Discharge guidance & next session", width = 7, status = "success", solidHeader = TRUE,
            uiOutput("post_stale_ui"),
            uiOutput("discharge_alert"),
            uiOutput("post_vitals_ui"),
            uiOutput("discharge_instructions_output"),
            uiOutput("next_adjustment_output"),
            uiOutput("post_next_ui"),
            div(class = "form-section-title", icon("save"), "Session record"),
            actionButton("save_session", "Save Session to History", class = "btn-success btn-block", icon = icon("save")),
            p(style = "font-size: 11px; color: #64748b; margin-top: 6px;",
              "Saved locally (data/sessions.csv): MRN, category, status, SBP, RPE. Creatinine / eGFR are added automatically when the value differs from the last saved result. Patient name and IC are not stored.")
          )
        )
      ),
      
      # TAB 3: Longitudinal Trends & Cohort
      tabItem(
        tabName = "longitudinal",
        
        fluidRow(
          column(12, uiOutput("data_source_banner_ui"))
        ),
        
        fluidRow(
          column(12, uiOutput("longitudinal_renal_alert_ui"))
        ),
        
        fluidRow(
          column(12, uiOutput("history_kpi_ui"))
        ),
        
        fluidRow(
          box(
            title = "Renal trajectory (lab follow-ups)", width = 6, status = "primary", solidHeader = TRUE,
            div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
                span(style = "font-size: 12px; color: #64748b; font-weight: 600;", "Metric Toggle:"),
                radioButtons("lab_metric_toggle", NULL, 
                             choices = c("eGFR Trend" = "egfr", "sCr Trend" = "scr"), 
                             selected = "egfr", inline = TRUE)
            ),
            plotlyOutput("egfr_trend_plot", height = "300px")
          ),
          
          box(
            title = "Pre vs post SBP per session", width = 6, status = "info", solidHeader = TRUE,
            p(class = "muted", style = "text-align: right; margin: 0 0 4px 0; font-size: 12px;", sprintf("Shaded bands: SBP > %.0f mmHg (upper limit) and < %.0f mmHg (low)", THR$sbp_stop, THR$sbp_low)),
            plotlyOutput("bp_trend_plot_enhanced", height = "300px")
          )
        ),
        
        fluidRow(
          box(
            title = "Session logs & lab records", width = 12, status = "success", solidHeader = TRUE,
            div(style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px; margin-bottom: 15px; background: #f8fafc; padding: 10px 14px; border-radius: 8px; border: 1px solid #e2e8f0;",
                div(style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                    dateRangeInput("history_date_range", "Filter Date Range:",
                                   start = Sys.Date() - 30, end = Sys.Date(),
                                   min = Sys.Date() - 3650, max = Sys.Date(),
                                   format = "yyyy-mm-dd", width = "260px"),
                    div(style = "margin-top: 10px;",
                        actionButton("btn_preset_30", "Last 30 Days", class = "btn-xs btn-default"),
                        actionButton("btn_preset_90", "Last 3 Months", class = "btn-xs btn-default"),
                        actionButton("btn_preset_all", "All Records", class = "btn-xs btn-default")
                    )
                ),
                div(
                  downloadButton("download_history", " Export Clinical History (.csv)", class = "btn-sm btn-primary", style = "font-weight: 600;")
                )
            ),
            DT::dataTableOutput("history_table")
          )
        )
      ),
      
      # TAB 4: Print Export (MEDICAL RIGOROUS CLINICAL REPORT)
      tabItem(
        tabName = "print_report",
        div(
          class = "no-print",
          style = "max-width: 960px; margin: 0 auto 15px auto; background: #fff; padding: 12px 20px; border-radius: 6px; border: 1px solid #cbd5e1; display: flex; justify-content: space-between; align-items: center; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
          div(
            h5(strong("Clinical Summary Report Generator"), style = "margin:0; color: #0f172a;"),
            p("Aligned with UKKA guidelines. Print tip: in the print dialog, turn off \"Headers and footers\" for a clean page.", style = "margin: 2px 0 0 0; color: #64748b; font-size: 12px;")
          ),
          tags$button("Print / Export PDF", onclick = "(function(){var t=document.title;var m=(document.getElementById('pt_mrn')||{}).value||'';document.title='RenalPT_Report'+(m?'_'+m:'')+'_'+new Date().toISOString().slice(0,10);window.print();document.title=t;})()", class = "btn btn-primary btn-sm", style = "font-weight: 700; padding: 6px 16px;")
        ),
        
        div(
          class = "medical-report-paper",
          
          # 1. Header Area
          div(
            class = "med-header-container",
            div(class = "rpt-brand",
                if (file.exists(file.path("www", "logo.png"))) img(src = "logo.png", class = "rpt-logo"),
                div(
                  div(class = "rpt-org", REPORT_ORG_NAME),
                  h1("Renal Physical Therapy Assessment Report", class = "med-hospital-title"),
                  p("Clinical Decision Support System \u2014 UKKA Guidelines Compliant", class = "med-sub-title")
                )
            ),
            uiOutput("report_meta_ui")
          ),
          
          # Dynamic Rendered Content
          uiOutput("dynamic_report_body")
        )
      )
    )
  )
)


# ------------------------------------------------------------------------------
# 2. Session storage helpers (local CSV; no patient name / IC is stored)
# ------------------------------------------------------------------------------
SESSION_FILE <- file.path("data", "sessions.csv")

empty_sessions <- function() {
  data.frame(Timestamp = character(), MRN = character(), Category = character(),
             Pre_Status = character(), Discharge_Status = character(),
             sCr = numeric(), eGFR = numeric(), Pre_SBP = numeric(), Post_SBP = numeric(),
             RPE = numeric(), Date = as.Date(character()), stringsAsFactors = FALSE)
}

load_sessions <- function() {
  if (!file.exists(SESSION_FILE)) return(empty_sessions())
  df <- tryCatch(read.csv(SESSION_FILE, stringsAsFactors = FALSE, colClasses = c(MRN = "character")),
                 error = function(e) NULL)
  need <- c("Timestamp", "MRN", "Category", "Pre_Status", "Discharge_Status", "sCr", "eGFR", "Pre_SBP", "Post_SBP", "RPE")
  if (is.null(df) || !all(need %in% names(df)) || nrow(df) == 0) return(empty_sessions())
  df$Date <- as.Date(substr(df$Timestamp, 1, 10))
  df[order(df$Date), ]
}

`%||%` <- function(a, b) if (is.null(a)) b else a


range_msg <- function(val, label, lo, hi, id = NULL) {
  m <- NULL
  if (is.null(val) || length(val) != 1 || is.na(val)) {
    m <- sprintf("%s is missing.", label)
  } else if (val < lo || val > hi) {
    m <- sprintf("%s is out of range (%s-%s).", label, lo, hi)
  }
  if (!is.null(m) && !is.null(id)) names(m) <- id
  m
}

# ------------------------------------------------------------------------------
# UI helper components (flat, table-based clinical layout)
# ------------------------------------------------------------------------------
strip_emoji <- function(x) gsub("^[^\\x20-\\x7E]+\\s*", "", x, perl = TRUE)

level_tag <- function(level, text = NULL) {
  switch(level,
         danger = span(class = "tg tg-stop", text %||% "Abnormal"),
         warn   = span(class = "tg tg-caution", text %||% "Caution"),
         safe   = span(class = "ok-text", text %||% "Normal"),
         span(class = "muted", text %||% "\u2014"))
}

st_banner <- function(level, label, headline, detail = NULL, items = NULL) {
  div(class = paste("st-banner", paste0("st-", level)),
      div(class = "st-label", label),
      div(class = "st-head", headline),
      if (!is.null(detail)) div(class = "st-detail", detail),
      items)
}

finding_list <- function(stop_items = NULL, caution_items = NULL) {
  rows <- c(lapply(stop_items, function(x) tags$li(span(class = "tg tg-stop", "Stop"), x)),
            lapply(caution_items, function(x) tags$li(span(class = "tg tg-caution", "Caution"), x)))
  if (length(rows) == 0) return(NULL)
  tags$ul(class = "st-list", rows)
}

sum_strip <- function(items) {
  div(class = "sum-strip", lapply(items, function(i)
    div(class = "sum-item",
        div(class = "sum-label", i$label),
        div(class = paste("sum-val", if (!is.null(i$level)) paste0("v-", i$level)), i$value),
        if (!is.null(i$sub)) div(class = "sum-sub", i$sub))))
}

# Split "Borg RPE 11-13 (Moderate)" into headline + detail
split_head <- function(x) {
  x <- as.character(x %||% "")
  head <- trimws(sub("\\s*[(+].*$", "", x))
  if (!nzchar(head)) return(list(head = x, detail = ""))
  detail <- trimws(substring(x, nchar(head) + 1))
  detail <- sub("^\\+\\s*", "", detail)
  detail <- sub("^\\(([^)]*)\\)", "\\1", detail)
  list(head = head, detail = detail)
}

fitt_value <- function(text) {
  s <- split_head(text)
  tagList(strong(s$head), if (nzchar(s$detail)) div(class = "kv-detail", s$detail))
}

kv_table <- function(pairs) {
  tags$table(class = "cl-table cl-kv",
             tags$tbody(lapply(names(pairs), function(k) tags$tr(tags$td(k), tags$td(pairs[[k]])))))
}

collapsible <- function(title, body, count = NULL, open = FALSE) {
  tags$details(class = "cdss-details", open = if (open) NA else NULL,
               tags$summary(title, if (!is.null(count)) span(class = "count-pill", count)),
               div(class = "details-body", body))
}

abnormal_only <- function(insts) {
  keep <- vapply(insts, function(i) startsWith(i, "\U0001F6A8") || startsWith(i, "\U0001F534") ||
                   startsWith(i, "\u26A0") || startsWith(i, "\U0001F7E0"), logical(1))
  insts[keep]
}

note_items <- function(insts) {
  tags$ul(class = "note-list", lapply(insts, function(inst) {
    lvl <- if (startsWith(inst, "\U0001F6A8") || startsWith(inst, "\U0001F534")) "danger"
    else if (startsWith(inst, "\u26A0") || startsWith(inst, "\U0001F7E0")) "warn" else "safe"
    tags$li(switch(lvl, danger = span(class = "tg tg-stop", "Alert"),
                   warn = span(class = "tg tg-caution", "Caution"),
                   span(class = "tg tg-ok", "OK")),
            span(HTML(strip_emoji(inst))))
  }))
}

# "What to do next" block for one STOP reason (used on screen and in the report)
next_step_block <- function(n) {
  lvl_txt <- c(now = "Notify now", conditional = "Notify if criteria are met", no = "No notification needed")
  lvl_cls <- c(now = "tg-stop", conditional = "tg-caution", no = "tg-ok")
  div(class = "ns-block",
      div(strong(n$finding)),
      kv_table(list(
        Action = tags$ul(style = "margin: 0; padding-left: 18px;", lapply(n$actions, function(a) tags$li(a))),
        Retest = n$retest,
        Notify = tagList(span(class = paste("tg", lvl_cls[[n$notify_level]]), lvl_txt[[n$notify_level]]), n$notify)
      )))
}

eval_log_table <- function(d) {
  tags$table(class = "cl-table",
             tags$thead(tags$tr(lapply(c("#", "Time", "BP", "HR", "SpO2", "Result", "Reason"), tags$th))),
             tags$tbody(lapply(seq_len(nrow(d)), function(i) {
               r <- d[i, ]
               status <- switch(r$Status,
                                SAFE = span(class = "ok-text", "Safe"),
                                CAUTION = span(class = "tg tg-caution", "Caution"),
                                span(class = "tg tg-stop", "Stop"))
               reason <- if (!is.na(r$StopReasons) && nzchar(r$StopReasons)) r$StopReasons else "\u2014"
               tags$tr(tags$td(i), tags$td(class = "nw", substr(r$Timestamp, 12, 16)),
                       tags$td(class = "nw", sprintf("%s / %s", r$SBP, r$DBP)), tags$td(r$HR), tags$td(r$SpO2),
                       tags$td(class = "nw", status), tags$td(class = "ref", reason))
             })))
}

handling_table <- function(d) {
  cell <- function(x) if (is.na(x) || !nzchar(x)) "\u2014" else x
  tags$table(class = "cl-table",
             tags$thead(tags$tr(lapply(c("Time", "Informed", "Informed at", "Decision / outcome", "Rescheduled", "Notes"), tags$th))),
             tags$tbody(lapply(seq_len(nrow(d)), function(i) {
               r <- d[i, ]
               tags$tr(tags$td(class = "nw", substr(r$Timestamp, 12, 16)), tags$td(cell(r$NotifiedWho)), tags$td(class = "nw", cell(r$NotifiedTime)),
                       tags$td(cell(r$Decision)), tags$td(cell(r$Reschedule)), tags$td(class = "ref", cell(r$Notes)))
             })))
}

# Functional capacity (same cut-offs as logic.R)
func_items <- function(inp) {
  items <- list()
  m <- inp[["mwt6_dist"]]; s <- inp[["sts30_reps"]]; g <- inp[["handgrip_kg"]]
  if (num_ok(m) && m > 0) {
    items[[length(items) + 1]] <- list(name = "6-Minute Walk Test", value = sprintf("%.0f m", m), ref = "no UKKA cut-off",
                                       verdict = "Recorded", level = "safe")
  }
  if (num_ok(s) && s > 0) {
    cut <- sts_norm_cutoff(inp[["age"]], inp[["gender"]])
    items[[length(items) + 1]] <- list(name = "30s Sit-to-Stand", value = sprintf("%.0f reps", s), ref = sprintf("\u2265 %.0f reps (age/sex)", cut),
                                       verdict = if (s < cut) "Below norm" else "Meets norm", level = if (s < cut) "warn" else "safe")
  }
  if (num_ok(g) && g > 0) {
    cut <- grip_low_cutoff(inp[["gender"]])
    items[[length(items) + 1]] <- list(name = "Handgrip strength", value = sprintf("%.1f kg", g), ref = sprintf("\u2265 %.0f kg", cut),
                                       verdict = if (g < cut) "Low strength" else "Adequate", level = if (g < cut) "warn" else "safe")
  }
  items
}

func_table <- function(items) {
  tags$table(class = "cl-table",
             tags$thead(tags$tr(lapply(c("Test", "Result", "Interpretation"), tags$th))),
             tags$tbody(lapply(items, function(i)
               tags$tr(class = if (i$level == "warn") "row-warn" else NULL,
                       tags$td(i$name),
                       tags$td(strong(i$value), div(class = "kv-detail", paste("Reference", i$ref))),
                       tags$td(level_tag(i$level, i$verdict))))))
}

# Measurement rows shared by the Pre tab, Post tab and the report
vital_rows <- function(inp, pinp = NULL, target = NULL, only = NULL, full = FALSE) {
  has_post <- !is.null(pinp)
  want <- function(k) is.null(only) || k %in% only
  f0 <- function(x) if (num_ok(x)) sprintf("%.0f", x) else "-"
  f1 <- function(x) if (num_ok(x)) sprintf("%.1f", x) else "-"
  dash <- "\u2014"
  lab_note <- function(v) {
    ld <- inp[["lab_date"]]
    if (is.null(ld) || length(ld) != 1 || is.na(ld)) return(v)
    days <- as.numeric(Sys.Date() - as.Date(ld))
    tagList(v, div(class = "kv-detail", sprintf("Blood test %s (%.0f d ago)", format(as.Date(ld), "%d %b"), days)))
  }
  rows <- list()
  merged_scr <- FALSE
  add <- function(key, name, pre, post, ref, level) {
    rows[[length(rows) + 1]] <<- list(key = key, name = name, pre = pre, post = post, ref = ref, level = level)
  }
  
  if (want("bp")) {
    lv <- worst_flag(flag_bp(inp[["sbp"]], inp[["dbp"]]), if (has_post) flag_bp(pinp[["post_sbp"]], pinp[["post_dbp"]]))
    post <- dash
    if (has_post) {
      d <- if (num_ok(inp[["sbp"]]) && num_ok(pinp[["post_sbp"]])) pinp[["post_sbp"]] - inp[["sbp"]] else NA
      post <- tagList(sprintf("%s / %s", f0(pinp[["post_sbp"]]), f0(pinp[["post_dbp"]])),
                      if (num_ok(d)) div(class = "kv-detail", sprintf("SBP %+.0f vs pre", d)))
    }
    add("bp", "Blood pressure (mmHg)", sprintf("%s / %s", f0(inp[["sbp"]]), f0(inp[["dbp"]])), post,
        sprintf("SBP %.0f-%.0f / DBP \u2264 %.0f", THR$sbp_low, THR$sbp_stop, THR$dbp_stop), lv)
  }
  if (want("hr")) {
    hr <- inp[["hr"]]
    pre_lv <- flag_hr_spo2(hr, NA)
    lv <- worst_flag(pre_lv, if (has_post) flag_hr_spo2(pinp[["post_hr"]], NA, post = TRUE))
    add("hr", "Heart rate (bpm)", f0(hr), if (has_post) f0(pinp[["post_hr"]]) else dash,
        if (has_post) sprintf("\u2264 %.0f", THR$post_hr_high) else sprintf("\u2264 %.0f (stop > %.0f)", THR$hr_caution, THR$hr_stop), lv)
  }
  if (want("spo2")) {
    bad <- function(x) num_ok(x) && x < THR$spo2_low
    lv <- if (bad(inp[["spo2"]]) || (has_post && bad(pinp[["post_spo2"]]))) "danger" else "safe"
    add("spo2", "SpO2 (%)", f0(inp[["spo2"]]), if (has_post) f0(pinp[["post_spo2"]]) else dash, sprintf("\u2265 %.0f", THR$spo2_low), lv)
  }
  if (want("rpe") && has_post && !is.null(target)) {
    cls <- classify_rpe(pinp[["post_rpe"]], target)
    add("rpe", "Borg RPE (6-20)", dash, f1(pinp[["post_rpe"]]), sprintf("Target %.0f-%.0f", target[1], target[2]),
        switch(cls$zone, over = "danger", upper = "warn", "safe"))
  }
  if (want("temp")) add("temp", "Temperature (\u00B0C)", f1(inp[["temp"]]), dash, sprintf("< %.1f", THR$temp_fever), flag_labs(NA, NA, NA, inp[["temp"]]))
  if (want("glu"))  add("glu", "Glucose (mmol/L)", f1(inp[["glucose"]]), dash, sprintf("%.1f-%.1f", THR$glu_low, THR$glu_high), flag_labs(NA, NA, inp[["glucose"]], NA))
  if (want("k"))    add("k", "Potassium (mmol/L)", lab_note(f1(inp[["k_level"]])), dash, sprintf("%.1f-%.1f", THR$k_low, THR$k_caution - 0.1), flag_labs(inp[["k_level"]], NA, NA, NA))
  if (want("hb"))   add("hb", "Haemoglobin (g/dL)", lab_note(f1(inp[["hb_level"]])), dash, sprintf("\u2265 %.1f", THR$hb_low), flag_labs(NA, inp[["hb_level"]], NA, NA))
  if (want("renal")) {
    if (egfr_applicable(inp[["ckd_category"]])) {
      e <- calculate_egfr(inp[["scr"]], inp[["age"]], inp[["gender"]])
      if (!is.null(e)) {
        egfr_txt <- sprintf("%.0f (%s)", e$egfr, sub("\\s*\\(.*$", "", e$stage))
        if (full && num_ok(inp[["scr"]])) {
          egfr_txt <- tagList(egfr_txt, div(class = "kv-detail", sprintf("sCr %.2f mg/dL (%.0f \u00B5mol/L)", inp[["scr"]], inp[["scr"]] * 88.4)))
          merged_scr <- TRUE
        }
        add("renal", "eGFR (mL/min/1.73m\u00B2)", egfr_txt, dash, dash, "neutral")
      }
    } else if (identical(inp[["ckd_category"]], "dialysis") && num_ok(inp[["dry_weight"]]) && num_ok(inp[["pre_weight"]])) {
      idwg <- ((inp[["pre_weight"]] - inp[["dry_weight"]]) / inp[["dry_weight"]]) * 100
      add("renal", "Fluid gain, IDWG (%)", sprintf("%.1f (%+.1f kg)", idwg, inp[["pre_weight"]] - inp[["dry_weight"]]), dash,
          "No exercise cut-off", "neutral")
    }
  }
  if (full && want("scr") && num_ok(inp[["scr"]]) && !merged_scr) {
    add("scr", "Serum creatinine", sprintf("%.2f mg/dL (%.0f \u00B5mol/L)", inp[["scr"]], inp[["scr"]] * 88.4), dash, dash, "neutral")
  }
  rows
}

vitals_table <- function(rows, mode = "pre") {
  heads <- if (mode == "pre") c("Parameter", "Result", "Reference", "Status") else c("Parameter", "Pre", "Post", "Reference", "Status")
  param_cell <- function(nm) {
    m <- regmatches(nm, regexec("^(.*?)\\s*\\(([^)]*)\\)$", nm, perl = TRUE))[[1]]
    if (length(m) == 3) tagList(m[2], span(class = "unit", paste0("  ", m[3]))) else nm
  }
  tags$table(class = "cl-table",
             tags$thead(tags$tr(lapply(heads, tags$th))),
             tags$tbody(lapply(rows, function(r) {
               cls <- switch(r$level, warn = "row-warn", danger = "row-danger", NULL)
               cells <- if (mode == "pre") {
                 list(tags$td(param_cell(r$name)), tags$td(class = "num", r$pre), tags$td(class = "ref", r$ref), tags$td(class = "nw", level_tag(r$level)))
               } else {
                 list(tags$td(param_cell(r$name)), tags$td(class = "num", r$pre), tags$td(r$post), tags$td(class = "ref", r$ref), tags$td(class = "nw", level_tag(r$level)))
               }
               do.call(tags$tr, c(list(class = cls), cells))
             })))
}

# Date axis that also works when only ONE record is in range
date_axis <- function(dates, title) {
  ax <- list(type = "date", tickformat = "%d %b", title = title, tickangle = 0, nticks = 6)
  d <- unique(as.Date(dates))
  if (length(d) == 1) ax$range <- as.character(c(d - 7, d + 7))
  ax
}

# ------------------------------------------------------------------------------
# 3. SERVER Logic
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Helper Navigation Handlers ---
  observeEvent(input$go_tab1, {
    updateTabItems(session, "tabs", "pre_eval")
  })
  observeEvent(input$go_tab2, {
    updateTabItems(session, "tabs", "post_eval")
  })
  
  # --- Modal Event: UKKA Guidelines Global Quick Reference ---
  observeEvent(input$show_ukka_modal, {
    showModal(modalDialog(
      title = div(style = "font-weight: 700; color: #0f172a; font-size: 16px;",
                  "UKKA / Renal Association Guideline: Exercise and Lifestyle in CKD"),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      div(
        style = "max-height: 560px; overflow-y: auto; padding: 5px;",
        div(class = "clinical-alert clinical-alert-info",
            strong("Source: "),
            "Baker LA, March DS, Wilkinson TJ, et al. Clinical practice guideline exercise and lifestyle in chronic kidney disease. BMC Nephrol 2022;23:75. ",
            a(href = "https://doi.org/10.1186/s12882-021-02618-1", target = "_blank", "doi:10.1186/s12882-021-02618-1"),
            " | ",
            a(href = "https://www.ukkidney.org/", target = "_blank", "ukkidney.org"),
            div(style = "margin-top: 6px; font-size: 12px;",
                "Final version April 2021; the guideline lists a review date of April 2026 - check for a newer version before relying on this summary. Grades use the modified GRADE system (1 = recommend, 2 = suggest; A-D = evidence quality).")
        ),
        bsCollapse(
          id = "ukka_accordion", open = "panel_non_dialysis",
          bsCollapsePanel(
            title = HTML("<strong>1. Non-dialysis CKD (stages 1-5)</strong>"),
            value = "panel_non_dialysis", style = "primary",
            tags$ul(
              tags$li(strong("1.1 (1B):"), " Encourage physical activity in non-dialysis CKD without contraindications and with stable, controlled comorbidities."),
              tags$li(strong("1.2 (1B):"), " Follow the UK Chief Medical Officers' guidelines (2019), adapted: daily activity; muscle strength, balance and flexibility on at least 2 days/week; build up to 150 mins/week of moderate-intensity aerobic activity (or 75 mins vigorous); break up sedentary time."),
              tags$li(strong("1.5 (1C):"), " Use a prescribed combination of aerobic and muscle-strengthening exercise to improve muscle function."),
              tags$li("High-intensity interval training is not recommended (benefit-to-risk ratio not established in this population).")
            )
          ),
          bsCollapsePanel(
            title = HTML("<strong>2. Haemodialysis</strong>"),
            value = "panel_dialysis", style = "info",
            tags$ul(
              tags$li(strong("2.1 (1C):"), " Encourage physical activity and exercise in haemodialysis patients where there are no contraindications."),
              tags$li(strong("2.2 (1B):"), " Aim for 150 mins/week moderate-intensity activity (or 75 mins vigorous), combining intradialytic and non-dialysis-day exercise."),
              tags$li(strong("2.3:"), " Intradialytic exercise is safe with no contraindications and should be available in all units."),
              tags$li(strong("Implementation:"), " supervised intradialytic cycling (static ergometer), at least 30 mins at each session (3x/week), avoiding the first 30 minutes of treatment; moderate intensity Borg RPE 12-14 and not above RPE 15; light resistance can be added; there is no contraindication to exercising in the second half of treatment."),
              tags$li(strong("Avoid exercise:"), " within 3 months of starting haemodialysis; any uncontrolled / clinically unstable condition (including infection or fever, MI within 2 weeks, undiagnosed chest pain); resting SBP > 200 or DBP > 110 mmHg; symptomatic hyper- or hypotension; unstable cardiac conditions (AHA / ACSM class D).")
            )
          ),
          bsCollapsePanel(
            title = HTML("<strong>3. Kidney transplantation and prehabilitation</strong>"),
            value = "panel_transplant", style = "success",
            tags$ul(
              tags$li(strong("3.1 (1B):"), " Encourage general physical activity in transplant recipients without contraindications."),
              tags$li(strong("3.3 (1C):"), " Aim for 150 mins/week moderate-to-vigorous activity (or 75 mins vigorous)."),
              tags$li(strong("3.9-3.12 (1B-2C):"), " Structured exercise at least 3x/week, combining aerobic and resistance training, supervised by trained staff and individualised to graft status."),
              tags$li(strong("3.13 (2C):"), " Exercise before surgery (prehabilitation) may increase pre-transplant activity and aid recovery."),
              tags$li(strong("3.15-3.16 (2C):"), " Avoid traumatic damage to the graft and contact sports; consider prolonged extreme exercise carefully; avoid sport-enhancing supplements.")
            )
          ),
          bsCollapsePanel(
            title = HTML("<strong>4. Lifestyle</strong>"),
            value = "panel_lifestyle", style = "warning",
            tags$ul(
              tags$li(strong("1.8 / 2.10 / 3.24 (1A):"), " Stop smoking (strongly discouraged in transplant recipients)."),
              tags$li(strong("1.9 / 2.11 (1B), 3.25 (2B):"), " Keep alcohol within national guidelines."),
              tags$li(strong("1.10 / 2.12 (1B), 3.26 (2B):"), " Avoid recreational drug use.")
            )
          ),
          bsCollapsePanel(
            title = HTML("<strong>5. Peritoneal dialysis (not covered by the UKKA guideline)</strong>"),
            value = "panel_pd", style = "default",
            tags$ul(
              tags$li("The UKKA 2021 guideline has no peritoneal dialysis section."),
              tags$li("Use the ISPD / Global Renal Exercise Network practice recommendations (Bennett PN et al., Perit Dial Int 2022;42:8-24). These are expert-consensus practice points covering exit-site care, exercising with a full or empty abdomen, intra-abdominal pressure, swimming and activity after catheter insertion."),
              tags$li("This app's PD advice is therefore a draft that must be confirmed against the ISPD / GREX document and local practice.")
            )
          ),
          bsCollapsePanel(
            title = HTML("<strong>6. Thresholds used by this app and where they come from</strong>"),
            value = "panel_thresholds", style = "default",
            p(class = "muted", style = "font-size: 12px;",
              sprintf("Rule set %s. Only rules backed by an international guideline or a standard clinical definition are enforced. Anything without an international standard is shown for information or left to the clinician.", THRESHOLDS_VERSION)),
            tags$table(class = "cl-table",
                       tags$thead(tags$tr(lapply(c("Item", "Rule", "Basis", "Source"), tags$th))),
                       tags$tbody(lapply(THR_SOURCES, function(r)
                         tags$tr(tags$td(strong(r$item)), tags$td(r$rule), tags$td(class = "nw", r$basis), tags$td(class = "ref", r$source)))))
          )
        )
      )
    ))
  })
  
  # --- Modal Event: Borg RPE Reference Table ---
  observeEvent(input$show_rpe_guide, {
    showModal(modalDialog(
      title = div(
        style = "display: flex; align-items: center; gap: 10px; font-weight: 700; color: #0f172a; font-size: 16px;",
        icon("book-medical", style = "color: #0284c7;"),
        span("Borg RPE (6–20) Perceived Exertion & Talk Test Reference Chart")
      ),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      
      div(
        style = "max-height: 520px; overflow-y: auto; padding: 2px;",
        tags$table(
          class = "modal-rpe-table",
          tags$thead(
            tags$tr(
              tags$th("RPE Rating", style = "width: 12%; text-align: center;"),
              tags$th("Perceived Exertion Description", style = "width: 28%;"),
              tags$th("Breathing & Speech Status (Talk Test)", style = "width: 42%;"),
              tags$th("Classification", style = "width: 18%; text-align: center;")
            )
          ),
          tags$tbody(
            tags$tr(style = "background-color: #ffffff;",
                    tags$td(strong("6"), style = "text-align: center; color: #64748b;"),
                    tags$td("No exertion at all", style = "color: #475569;"),
                    tags$td("At rest; breathing is completely smooth and normal.", style = "color: #475569;"),
                    tags$td(style = "text-align: center;", span(class="rpe-pill rpe-pill-default", "Rest / Very Light"))
            ),
            tags$tr(style = "background-color: #f8fafc;",
                    tags$td(strong("7 – 8"), style = "text-align: center; color: #64748b;"),
                    tags$td("Extremely light", style = "color: #475569;"),
                    tags$td("Feels extremely easy with no noticeable effort.", style = "color: #475569;"),
                    tags$td(style = "text-align: center;", span(class="rpe-pill rpe-pill-default", "Extremely Light"))
            ),
            tags$tr(style = "background-color: #f0f9ff; border-left: 4px solid #38bdf8;",
                    tags$td(strong("9 – 10"), style = "text-align: center; color: #0369a1;"),
                    tags$td("Very light", style = "color: #0369a1;"),
                    tags$td("Feels like a casual walk; can talk easily and sing.", style = "color: #0369a1;"),
                    tags$td(style = "text-align: center;", span(class="rpe-pill rpe-pill-info", "Light Exercise"))
            ),
            tags$tr(style = "background-color: #f0fdf4; border-left: 4px solid #4ade80;",
                    tags$td(strong("11"), style = "text-align: center; color: #15803d;"),
                    tags$td("Light", style = "color: #15803d;"),
                    tags$td("Slight effort required, but comfortable; conversation is easy.", style = "color: #15803d;"),
                    tags$td(style = "text-align: center;", span(class="rpe-pill rpe-pill-success", "Low-Mod Entry"))
            ),
            tags$tr(style = "background-color: #dcfce7; border-left: 4px solid #22c55e; font-weight: bold;",
                    tags$td(strong("12 – 13"), style = "text-align: center; color: #166534;"),
                    tags$td("Somewhat hard", style = "color: #166534;"),
                    tags$td("Breathing quickens, but can maintain full sentences (cannot sing).", style = "color: #166534;"),
                    tags$td(style = "text-align: center;", span(class="rpe-pill rpe-pill-success", "CKD Target Zone"))
            ),
            tags$tr(style = "background-color: #fffbeb; border-left: 4px solid #fbbf24;",
                    tags$td(strong("14"), style = "text-align: center; color: #b45309;"),
                    tags$td("Between somewhat hard & hard", style = "color: #b45309;"),
                    tags$td("Breathing is noticeably faster; speech starts to falter slightly.", style = "color: #b45309;"),
                    tags$td(style = "text-align: center;", span(class="rpe-pill rpe-pill-warning", "Upper Threshold"))
            ),
            tags$tr(style = "background-color: #fef2f2; border-left: 4px solid #f87171;",
                    tags$td(strong("15 – 16"), style = "text-align: center; color: #b91c1c;"),
                    tags$td("Hard / Heavy", style = "color: #b91c1c;"),
                    tags$td("Heavy breathing; can only speak in short phrases.", style = "color: #b91c1c;"),
                    tags$td(style = "text-align: center;", span(class="rpe-pill rpe-pill-danger", "Vigorous Intensity"))
            ),
            tags$tr(style = "background-color: #fef2f2; border-left: 4px solid #ef4444;",
                    tags$td(strong("17 – 18"), style = "text-align: center; color: #b91c1c;"),
                    tags$td("Very hard", style = "color: #b91c1c;"),
                    tags$td("Gasping for breath; can only manage single words.", style = "color: #b91c1c;"),
                    tags$td(style = "text-align: center;", span(class="rpe-pill rpe-pill-danger", "Very High"))
            ),
            tags$tr(style = "background-color: #fee2e2; border-left: 4px solid #dc2626;",
                    tags$td(strong("19 – 20"), style = "text-align: center; color: #991b1b;"),
                    tags$td("Extremely hard / Maximal effort", style = "color: #991b1b;"),
                    tags$td("Near total exhaustion; impossible to speak or continue.", style = "color: #991b1b;"),
                    tags$td(style = "text-align: center;", span(class="rpe-pill rpe-pill-danger", "Maximal Limit"))
            )
          )
        )
      )
    ))
  })
  
  # --------------------------------------------------------------------------
  # Reactive state
  # --------------------------------------------------------------------------
  pre_state      <- reactiveVal(NULL)   # snapshot of the last Pre-Test evaluation
  post_state     <- reactiveVal(NULL)   # snapshot of the last Post-Test evaluation
  fitt_paused    <- reactiveVal(FALSE)
  sessions_store <- reactiveVal(load_sessions())
  session_saved  <- reactiveVal(FALSE)
  eval_store     <- reactiveVal(load_log(EVAL_FILE, EVAL_COLS))
  handling_store <- reactiveVal(load_log(HANDLING_FILE, HANDLING_COLS))
  
  scr_mgdl <- reactive(scr_to_mgdl(input$scr, input$scr_unit))
  
  # Creatinine unit toggle (converts the value already typed)
  observeEvent(input$scr_unit, {
    cur <- isolate(input$scr)
    if (identical(input$scr_unit, "umol")) {
      newv <- if (num_ok(cur)) round(cur * 88.4, 0) else NA
      updateNumericInput(session, "scr", label = "Serum Creatinine (\u00B5mol/L):", value = newv, min = 18, max = 1768, step = 5)
    } else {
      newv <- if (num_ok(cur)) round(cur / 88.4, 2) else NA
      updateNumericInput(session, "scr", label = "Serum Creatinine (mg/dL):", value = newv, min = 0.2, max = 20, step = 0.1)
    }
  }, ignoreInit = TRUE)
  
  # --------------------------------------------------------------------------
  # Input collection & validation
  # --------------------------------------------------------------------------
  collect_pre_inputs <- function() {
    cat <- input$ckd_category %||% ""
    inp <- list(
      mrn = trimws(input$pt_mrn %||% ""),
      ckd_category = cat, age = input$age, gender = input$gender %||% "", scr = scr_mgdl(),
      lab_date = input$lab_date,
      mwt6_dist = input$mwt6_dist, sts30_reps = input$sts30_reps, handgrip_kg = input$handgrip_kg,
      diabetic_neuropathy = input$diabetic_neuropathy, foot_ulcer = input$foot_ulcer,
      hypo_risk_meds = isTRUE(input$hypo_risk_meds),
      start_level = input$start_level %||% "standard",
      sbp = input$sbp, dbp = input$dbp, hr = input$hr, spo2 = input$spo2,
      temp = input$temp, glucose = input$glucose, k_level = input$k_level, hb_level = input$hb_level,
      lifestyle_risks = input$lifestyle_risks, symptoms = input$symptoms
    )
    if (identical(cat, "dialysis")) {
      inp$dial_time <- input$dial_time
      inp$hd_vintage <- input$hd_vintage %||% ""
      inp$fistula_arm <- input$fistula_arm %||% ""
      if (inp$fistula_arm %in% c("Left Arm", "Right Arm")) inp$fistula_age <- input$fistula_age %||% ""
      inp$dry_weight <- input$dry_weight
      inp$pre_weight <- input$pre_weight
    } else if (identical(cat, "peritoneal")) {
      inp$pd_full_abdomen <- input$pd_full_abdomen
      inp$pd_hernia <- input$pd_hernia
      inp$pd_catheter_status <- input$pd_catheter_status %||% ""
    } else if (identical(cat, "transplant")) {
      inp$post_transplant_time <- input$post_transplant_time %||% ""
    }
    inp
  }
  
  validate_pre <- function(inp) {
    cat <- inp[["ckd_category"]] %||% ""
    msgs <- c(
      if (!nzchar(inp[["mrn"]])) c(pt_mrn = "MRN is required (left sidebar)."),
      if (!nzchar(cat)) c(ckd_category = "Select the clinical sub-population."),
      range_msg(inp$age, "Age", 18, 100, "age"),
      if (!nzchar(inp[["gender"]])) c(gender = "Select the gender."),
      range_msg(inp$scr, "Serum creatinine", 0.2, 20.05, "scr"),
      range_msg(inp$sbp, "Pre SBP", 70, 220, "sbp"),
      range_msg(inp$dbp, "Pre DBP", 40, 130, "dbp"),
      range_msg(inp$hr, "HR", 40, 160, "hr"),
      range_msg(inp$spo2, "SpO2", 70, 100, "spo2"),
      range_msg(inp$temp, "Temperature", 35, 41, "temp"),
      range_msg(inp$glucose, "Glucose", 2, 25, "glucose"),
      range_msg(inp$k_level, "Potassium", 2, 8, "k_level"),
      range_msg(inp$hb_level, "Haemoglobin", 4, 18, "hb_level")
    )
    ld <- inp[["lab_date"]]
    if (is.null(ld) || length(ld) != 1 || is.na(ld)) {
      msgs <- c(msgs, lab_date = "Blood test date is required.")
    } else if (as.Date(ld) > Sys.Date()) {
      msgs <- c(msgs, lab_date = "Blood test date cannot be in the future.")
    }
    if (num_ok(inp$sbp) && num_ok(inp$dbp) && inp$sbp <= inp$dbp) msgs <- c(msgs, sbp = "Pre SBP must be higher than DBP.")
    if (identical(cat, "dialysis")) {
      arm <- inp[["fistula_arm"]] %||% ""
      msgs <- c(msgs,
                if (!nzchar(inp[["hd_vintage"]] %||% "")) c(hd_vintage = "Select how long the patient has been on haemodialysis."),
                if (!nzchar(arm)) c(fistula_arm = "Select the vascular access."),
                if (arm %in% c("Left Arm", "Right Arm") && !nzchar(inp[["fistula_age"]] %||% "")) c(fistula_age = "Select the AV access maturation phase."),
                range_msg(inp$dial_time, "Time elapsed in the dialysis session", 0, 6, "dial_time"),
                range_msg(inp$dry_weight, "Dry weight", 30, 150, "dry_weight"),
                range_msg(inp$pre_weight, "Pre-weight", 30, 150, "pre_weight"))
    } else if (identical(cat, "peritoneal")) {
      if (!nzchar(inp[["pd_catheter_status"]] %||% "")) msgs <- c(msgs, pd_catheter_status = "Select the PD catheter exit-site status.")
    } else if (identical(cat, "transplant")) {
      if (!nzchar(inp[["post_transplant_time"]] %||% "")) msgs <- c(msgs, post_transplant_time = "Select the post-transplant time elapsed.")
    }
    msgs
  }
  
  collect_post_inputs <- function() {
    list(mrn = trimws(input$pt_mrn %||% ""),
         post_sbp = input$post_sbp, post_dbp = input$post_dbp, post_hr = input$post_hr,
         post_spo2 = input$post_spo2,
         post_rpe = suppressWarnings(as.numeric(input$post_rpe_sel %||% "")),
         post_symptoms = input$post_symptoms)
  }
  
  validate_post <- function(inp) {
    msgs <- c(
      if (!nzchar(inp[["mrn"]])) c(pt_mrn = "MRN is required (left sidebar)."),
      range_msg(inp$post_sbp, "Post SBP", 60, 220, "post_sbp"),
      range_msg(inp$post_dbp, "Post DBP", 40, 130, "post_dbp"),
      range_msg(inp$post_hr, "Post HR", 40, 180, "post_hr"),
      range_msg(inp$post_spo2, "Post SpO2", 70, 100, "post_spo2"),
      range_msg(inp$post_rpe, "Borg RPE", 6, 20, "post_rpe_sel")
    )
    if (num_ok(inp$post_sbp) && num_ok(inp$post_dbp) && inp$post_sbp <= inp$post_dbp) msgs <- c(msgs, post_sbp = "Post SBP must be higher than DBP.")
    msgs
  }
  
  pre_stale  <- reactive({ st <- pre_state();  if (is.null(st))  FALSE else !identical(st$inp, collect_pre_inputs()) })
  post_stale <- reactive({ ps <- post_state(); if (is.null(ps)) FALSE else !identical(ps$inp, collect_post_inputs()) })
  
  # TRUE when the MRN was changed after the last evaluation (result belongs to another patient)
  patient_changed <- reactive({
    st <- pre_state()
    !is.null(st) && !identical(st$inp[["mrn"]], trimws(input$pt_mrn %||% ""))
  })
  post_patient_changed <- reactive({
    ps <- post_state()
    !is.null(ps) && !identical(ps$inp[["mrn"]], trimws(input$pt_mrn %||% ""))
  })
  
  pre_res_safe  <- reactive({ st <- pre_state();  if (!is.null(st) && isTRUE(st$ok) && !patient_changed()) st$res else NULL })
  post_res_safe <- reactive({ ps <- post_state(); if (!is.null(ps) && isTRUE(ps$ok) && !post_patient_changed()) ps$res else NULL })
  
  current_target <- reactive({
    r <- pre_res_safe()
    if (!is.null(r)) r$target_rpe else c(11, 13)
  })
  
  # --------------------------------------------------------------------------
  # Small live badges
  # --------------------------------------------------------------------------
  output$rpe_status_badge_ui <- renderUI({
    v <- suppressWarnings(as.numeric(input$post_rpe_sel %||% ""))
    if (!num_ok(v)) return(div(class = "rpe-line muted", "Select the patient's Borg rating."))
    cls <- classify_rpe(v, current_target())
    div(class = "rpe-line", span(class = "dot", style = sprintf("background: %s;", cls$color)), cls$label)
  })
  
  output$egfr_display_ui <- renderUI({
    if (!egfr_applicable(input$ckd_category)) {
      return(div(style = "margin-top: -5px; margin-bottom: 10px;",
                 span(class = "value-badge", style = "background-color: #64748b;",
                      "eGFR not valid for dialysis patients (creatinine reflects dialysis clearance)")))
    }
    egfr_res <- calculate_egfr(scr_mgdl(), input$age, input$gender)
    if (!is.null(egfr_res)) {
      badge_color <- if (egfr_res$egfr < 15) "#dc3545" else if (egfr_res$egfr < 60) "#ffc107" else "#28a745"
      div(style = "margin-top: -5px; margin-bottom: 10px;",
          span(class = "value-badge", style = sprintf("background-color: %s;", badge_color),
               sprintf("eGFR: %.1f mL/min/1.73m\u00B2 | %s", egfr_res$egfr, egfr_res$stage))
      )
    }
  })
  
  output$idwg_display_ui <- renderUI({
    if (num_ok(input$dry_weight) && num_ok(input$pre_weight) && input$dry_weight > 0) {
      idwg_pct <- ((input$pre_weight - input$dry_weight) / input$dry_weight) * 100
      badge_color <- "#64748b"   # information only: no exercise guideline defines an IDWG cut-off
      div(style = "margin-top: -5px; margin-bottom: 10px;",
          span(class = "value-badge", style = sprintf("background-color: %s;", badge_color),
               sprintf("Calculated IDWG: %.2f%% (%+.1f kg)", idwg_pct, input$pre_weight - input$dry_weight))
      )
    }
  })
  
  output$start_level_hint_ui <- renderUI({
    s <- input$sts30_reps
    if (num_ok(s) && s > 0 && num_ok(input$age) && nzchar(input$gender %||% "")) {
      cut <- sts_norm_cutoff(input$age, input$gender)
      if (s < cut) {
        p(style = "color: #8a5a00; font-size: 12.5px; margin: -2px 0 8px 0;",
          sprintf("30s sit-to-stand %.0f is below the CDC STEADI age/sex norm (%.0f): consider a light start.", s, cut))
      }
    }
  })
  
  output$bp_validation_ui <- renderUI({
    sbp <- input$sbp; dbp <- input$dbp
    msg <- NULL
    if (num_ok(sbp) && num_ok(dbp) && sbp <= dbp) {
      msg <- "SBP (upper number) must be higher than DBP (lower number)."
    } else if (num_ok(dbp) && dbp >= 100) {
      msg <- sprintf("DBP %.0f is very high. Please confirm it is the LOWER number (e.g. 120/80 -> SBP 120, DBP 80).", dbp)
    } else if (num_ok(sbp) && num_ok(dbp) && (sbp - dbp) < 20) {
      msg <- "SBP and DBP are very close together. Please re-check the entry."
    }
    if (!is.null(msg)) {
      p(style = "color: #b42318; font-size: 12.5px; font-weight: 600; margin: -2px 0 8px 0;", msg)
    }
  })
  
  # --------------------------------------------------------------------------
  # UX: header chip, sidebar progress, next-step navigation
  # --------------------------------------------------------------------------
  output$header_patient_ui <- renderUI({
    nm  <- trimws(input$pt_name %||% "")
    mrn <- trimws(input$pt_mrn %||% "")
    if (!nzchar(nm) && !nzchar(mrn)) {
      return(span(class = "hdr-chip", icon("user"), span(style = "color:#64748b; font-weight:500;", "No patient entered")))
    }
    span(class = "hdr-chip", icon("user"), if (nzchar(nm)) nm else "Unnamed patient",
         if (nzchar(mrn)) span(style = "color:#64748b; font-weight:500;", paste0("\u00B7 ", mrn)))
  })
  
  output$sidebar_progress_ui <- renderUI({
    st <- pre_state(); ps <- post_state(); sv <- session_saved()
    row <- function(label, state, sym = NULL) {
      cls <- switch(state, done = "pg-done", warn = "pg-warn", bad = "pg-bad", "")
      dot <- if (!is.null(sym)) sym else switch(state, done = "\u2713", warn = "!", bad = "\u2715", "")
      div(class = paste("pg-row", cls), span(class = "pg-dot", dot), label)
    }
    contra <- !is.null(st) && isTRUE(st$ok) && st$res$status == "CONTRAINDICATED"
    
    pre_row <- if (is.null(st)) row("Pre-test", "todo")
    else if (patient_changed()) row("Pre-test: different patient", "bad")
    else if (!isTRUE(st$ok)) row("Pre-test: fix inputs", "bad")
    else if (isTRUE(pre_stale())) row("Pre-test: re-evaluate", "warn")
    else row(paste0("Pre-test: ", switch(st$res$status, SAFE = "safe", CAUTION = "caution", "stop")),
             switch(st$res$status, SAFE = "done", CAUTION = "warn", "bad"))
    
    post_row <- if (contra) row("Post-test: not needed", "todo", "\u2013")
    else if (is.null(ps)) row("Post-test", "todo")
    else if (!isTRUE(ps$ok)) row("Post-test: fix inputs", "bad")
    else if (isTRUE(post_stale())) row("Post-test: re-evaluate", "warn")
    else row(paste0("Post-test: ", switch(ps$res$discharge_status, CLEARED = "cleared", OBSERVATION_REQUIRED = "observe", "review")),
             switch(ps$res$discharge_status, CLEARED = "done", OBSERVATION_REQUIRED = "warn", "bad"))
    
    save_row <- if (isTRUE(sv)) row("Session saved", "done") else row("Session not saved", "todo")
    tagList(pre_row, post_row, save_row)
  })
  
  observeEvent(input$goto_post, { updateTabItems(session, "tabs", "post_eval") })
  observeEvent(input$goto_report_a, { updateTabItems(session, "tabs", "print_report") })
  observeEvent(input$goto_report_b, { updateTabItems(session, "tabs", "print_report") })
  
  output$pre_next_ui <- renderUI({
    r <- pre_res_safe()
    if (is.null(r)) return(NULL)
    if (r$status == "CONTRAINDICATED") {
      div(class = "next-step", span("Re-testing after rest or treatment?"),
          div(actionButton("retest_clear", "Clear vitals for re-test", icon = icon("redo"), class = "btn-primary btn-sm"),
              actionButton("goto_report_a", "Open report", icon = icon("file-medical"), class = "btn-default btn-sm")))
    } else {
      div(class = "next-step", span("Exercise session finished?"),
          actionButton("goto_post", "Continue to Post-Test", icon = icon("arrow-right"), class = "btn-primary btn-sm"))
    }
  })
  
  output$post_next_ui <- renderUI({
    r <- post_res_safe()
    if (is.null(r)) return(NULL)
    div(class = "next-step", span("Save the session below, then open the clinical report."),
        actionButton("goto_report_b", "Open report", icon = icon("file-medical"), class = "btn-default btn-sm"))
  })
  
  # --------------------------------------------------------------------------
  # Pre-Test evaluation
  # --------------------------------------------------------------------------
  observeEvent(input$run_cdss, {
    inp <- collect_pre_inputs()
    msgs <- validate_pre(inp)
    if (length(msgs) > 0) {
      session$sendCustomMessage("invalid", list(ids = as.list(unname(names(msgs)))))
      pre_state(list(ok = FALSE, msgs = unname(msgs), inp = inp, time = Sys.time()))
      showNotification("Please correct the input problems before evaluating.", type = "error", duration = 6)
      return()
    }
    session$sendCustomMessage("invalid", list(ids = list()))
    lab_age_days <- as.numeric(Sys.Date() - as.Date(inp[["lab_date"]]))
    res <- tryCatch(
      generate_cdss_recommendation(
        sbp = inp[["sbp"]], dbp = inp[["dbp"]], hr = inp[["hr"]], spo2 = inp[["spo2"]],
        temp = inp[["temp"]], glucose = inp[["glucose"]],
        scr = inp[["scr"]], age = inp[["age"]], gender = inp[["gender"]],
        dry_weight = inp[["dry_weight"]], pre_weight = inp[["pre_weight"]],
        k_level = inp[["k_level"]], hb_level = inp[["hb_level"]],
        ckd_category = inp[["ckd_category"]], dial_time = inp[["dial_time"]],
        fistula_arm = inp[["fistula_arm"]], fistula_age = inp[["fistula_age"]],
        pd_full_abdomen = inp[["pd_full_abdomen"]], pd_hernia = inp[["pd_hernia"]],
        pd_catheter_status = inp[["pd_catheter_status"]],
        post_transplant_time = inp[["post_transplant_time"]],
        mwt6_dist = inp[["mwt6_dist"]], sts30_reps = inp[["sts30_reps"]], handgrip_kg = inp[["handgrip_kg"]],
        diabetic_neuropathy = inp[["diabetic_neuropathy"]], foot_ulcer = inp[["foot_ulcer"]],
        lifestyle_risks = inp[["lifestyle_risks"]], symptoms = inp[["symptoms"]],
        lab_age_days = lab_age_days,
        hd_vintage = inp[["hd_vintage"]], hypo_risk_meds = isTRUE(inp[["hypo_risk_meds"]]),
        start_level = inp[["start_level"]]
      ),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      pre_state(list(ok = FALSE, msgs = paste("Calculation error:", conditionMessage(res)), inp = inp, time = Sys.time()))
      showNotification("Calculation error - see the message on screen.", type = "error")
      return()
    }
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    pre_state(list(ok = TRUE, res = res, inp = inp, time = Sys.time(), ts = ts,
                   patient = list(name = trimws(input$pt_name %||% ""), mrn = inp[["mrn"]], ic = trimws(input$pt_ic %||% ""))))
    fitt_paused(FALSE)   # a fresh evaluation lifts any earlier pause
    session_saved(FALSE)
    
    # every successful evaluation (including re-tests) is kept in the evaluation log
    log_row <- data.frame(
      Timestamp = ts, MRN = inp[["mrn"]], Category = inp[["ckd_category"]],
      SBP = inp[["sbp"]], DBP = inp[["dbp"]], HR = inp[["hr"]], SpO2 = inp[["spo2"]], Temp = inp[["temp"]],
      Glucose = inp[["glucose"]], K = inp[["k_level"]], Hb = inp[["hb_level"]],
      LabDate = format(as.Date(inp[["lab_date"]])), Status = res$status,
      StopReasons = paste(res$stop_alerts, collapse = "; "), stringsAsFactors = FALSE)
    if (append_log(EVAL_FILE, log_row)) eval_store(load_log(EVAL_FILE, EVAL_COLS))
    else showNotification("Could not write data/evaluations.csv (check folder permissions).", type = "warning")
  })
  
  idle_placeholder <- function(msg = "Enter the patient data on the left, then press \"Evaluate PT Safety\". Results appear here.") {
    div(class = "empty-state",
        div(class = "es-title", "Ready when you are"),
        div(class = "es-text", msg))
  }
  
  output$pre_stale_ui <- renderUI({
    if (isTRUE(pre_stale())) {
      div(class = "clinical-alert clinical-alert-warning no-print",
          "Inputs have changed since the last evaluation. Click \"Evaluate PT Safety\" again to refresh the results below.")
    }
  })
  
  output$pause_banner_ui <- renderUI({
    if (isTRUE(fitt_paused())) {
      div(class = "clinical-alert clinical-alert-danger",
          "FITT PLAN PAUSED (renal alert). Exercise prescription is withheld until the patient is re-evaluated (click Evaluate again after medical review).")
    }
  })
  
  output$status_alert <- renderUI({
    st <- pre_state()
    if (is.null(st)) return(idle_placeholder())
    if (patient_changed()) {
      return(st_banner("danger", "Patient changed", "The last evaluation belongs to a different patient",
                       sprintf("It was done for MRN %s. Enter this patient's data and press Evaluate, or use \"New patient / reset\".",
                               if (nzchar(st$inp[["mrn"]])) st$inp[["mrn"]] else "(none)")))
    }
    if (!isTRUE(st$ok)) {
      return(st_banner("danger", "Input check", "Cannot evaluate - correct the fields marked in red", NULL,
                       tags$ul(class = "st-list", lapply(st$msgs, function(m) tags$li(m)))))
    }
    res <- st$res
    if (res$status == "CONTRAINDICATED") {
      st_banner("danger", "Pre-session clearance", "Do not exercise today", "Re-evaluate clinical stability before the next session.",
                finding_list(res$stop_alerts, res$caution_alerts))
    } else if (res$status == "CAUTION") {
      st_banner("warn", "Pre-session clearance", "Proceed with caution", "Modified session - follow the precautions below.",
                finding_list(NULL, res$caution_alerts))
    } else {
      st_banner("ok", "Pre-session clearance", "Safe to exercise", "Baseline safety checks passed.")
    }
  })
  
  output$vitals_snapshot_ui <- renderUI({
    st <- pre_state()
    if (is.null(st) || !isTRUE(st$ok) || patient_changed()) return(NULL)
    tagList(div(class = "section-label", "Baseline measurements"),
            vitals_table(vital_rows(st$inp), "pre"))
  })
  
  output$next_steps_output <- renderUI({
    res <- pre_res_safe()
    if (is.null(res) || length(res$next_steps) == 0) return(NULL)
    tagList(
      div(class = "section-label", "What to do next"),
      p(class = "muted", style = "font-size: 12px; margin: 0 0 8px 0;", NEXT_STEPS_STATUS),
      lapply(res$next_steps, next_step_block)
    )
  })
  
  output$fitt_output <- renderUI({
    res <- pre_res_safe()
    if (is.null(res)) return(NULL)
    if (res$status == "CONTRAINDICATED") {
      return(st_banner("danger", "FITT prescription", "Withheld", "A contraindication is present - no exercise prescription is issued for today."))
    }
    if (isTRUE(fitt_paused())) {
      return(st_banner("danger", "FITT prescription", "Paused", "Pending medical review of the renal alert."))
    }
    f <- res$fitt
    inp <- pre_state()$inp
    fi <- func_items(inp)
    tagList(
      div(class = "section-label", "FITT prescription", span(class = "evidence-pill", f$UKKA_Grade)),
      div(class = "sub-pop", f$Category),
      if (res$status == "CAUTION") div(class = "clinical-alert clinical-alert-warning",
                                       "Modified prescription: use the lower end of the intensity range and follow the precautions."),
      kv_table(list(Frequency = fitt_value(f$Frequency), Intensity = fitt_value(f$Intensity),
                    Time = fitt_value(f$Time), Type = fitt_value(f$Type), Monitoring = f$Monitoring)),
      if (length(fi) > 0) tagList(
        div(class = "section-label", sprintf("Functional capacity - starting level: %s", res$functional_tier)),
        func_table(fi)
      )
    )
  })
  
  output$precautions_output <- renderUI({
    res <- pre_res_safe()
    if (is.null(res) || length(res$precautions) == 0) return(NULL)
    collapsible("Clinical precautions", tags$ul(lapply(res$precautions, function(p) tags$li(p))),
                count = length(res$precautions), open = FALSE)
  })
  
  output$lifestyle_output <- renderUI({
    res <- pre_res_safe()
    if (is.null(res) || length(res$lifestyle_advice) == 0) return(NULL)
    collapsible("Lifestyle counselling (UKKA Chapter 4)", tags$ul(lapply(res$lifestyle_advice, function(la) tags$li(la))),
                count = length(res$lifestyle_advice), open = FALSE)
  })
  
  # --------------------------------------------------------------------------
  # Post-Test evaluation
  # --------------------------------------------------------------------------
  output$post_context_ui <- renderUI({
    r <- pre_res_safe()
    if (is.null(r)) {
      div(class = "clinical-alert clinical-alert-warning", style = "font-size: 12px; padding: 8px 12px;",
          "Pre-Test not evaluated: using the default RPE target (11-13) and the current Pre SBP.")
    } else {
      div(class = "clinical-alert clinical-alert-info", style = "font-size: 12px; padding: 8px 12px;",
          sprintf("Prescribed RPE target from Pre-Test: %.0f-%.0f (starting level: %s).", r$target_rpe[1], r$target_rpe[2], r$functional_tier))
    }
  })
  
  observeEvent(input$run_post_cdss, {
    inp <- collect_post_inputs()
    msgs <- validate_post(inp)
    if (length(msgs) > 0) {
      session$sendCustomMessage("invalid", list(ids = as.list(unname(names(msgs)))))
      post_state(list(ok = FALSE, msgs = unname(msgs), inp = inp, time = Sys.time()))
      showNotification("Please correct the input problems before evaluating.", type = "error", duration = 6)
      return()
    }
    session$sendCustomMessage("invalid", list(ids = list()))
    pst <- pre_state()
    have_pre <- !is.null(pst) && isTRUE(pst$ok)
    pre_sbp <- if (have_pre) pst$inp[["sbp"]] else if (num_ok(input$sbp)) input$sbp else NULL
    target  <- if (have_pre) pst$res$target_rpe else c(11, 13)
    res <- tryCatch(
      generate_post_exercise_guidance(
        post_sbp = inp[["post_sbp"]], post_dbp = inp[["post_dbp"]], post_hr = inp[["post_hr"]],
        post_spo2 = inp[["post_spo2"]], post_rpe = inp[["post_rpe"]], pre_sbp = pre_sbp,
        post_symptoms = inp[["post_symptoms"]], target_rpe = target
      ),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      post_state(list(ok = FALSE, msgs = paste("Calculation error:", conditionMessage(res)), inp = inp, time = Sys.time()))
      return()
    }
    post_state(list(ok = TRUE, res = res, inp = inp, pre_sbp = pre_sbp, target = target, time = Sys.time()))
    session_saved(FALSE)
  })
  
  output$post_stale_ui <- renderUI({
    if (isTRUE(post_stale())) {
      div(class = "clinical-alert clinical-alert-warning no-print",
          "Post-exercise inputs have changed since the last evaluation. Click \"Evaluate Post-Exercise Discharge Safety\" again.")
    }
  })
  
  output$discharge_alert <- renderUI({
    ps <- post_state()
    if (!is.null(ps) && post_patient_changed()) {
      return(st_banner("danger", "Patient changed", "The last post-test belongs to a different patient",
                       "Enter this patient's post-exercise data and press Evaluate."))
    }
    if (is.null(ps)) return(idle_placeholder("Enter the post-exercise vitals, RPE and symptoms, then press \"Evaluate Post-Exercise Discharge Safety\"."))
    if (!isTRUE(ps$ok)) {
      return(st_banner("danger", "Input check", "Cannot evaluate - correct the fields marked in red", NULL,
                       tags$ul(class = "st-list", lapply(ps$msgs, function(m) tags$li(m)))))
    }
    status <- ps$res$discharge_status
    if (status == "OBSERVATION_REQUIRED") {
      st_banner("warn", "Discharge clearance", "Hold discharge - observe", "Re-check vitals after 15 minutes of rest.")
    } else if (status == "MEDICAL_REVIEW_REQUIRED") {
      st_banner("danger", "Discharge clearance", "Do not discharge - urgent medical review", "Critical vital sign or red-flag symptom after exercise.")
    } else {
      st_banner("ok", "Discharge clearance", "Cleared for discharge", "Vital signs and clinical indicators are within safe limits.")
    }
  })
  
  output$post_vitals_ui <- renderUI({
    ps <- post_state()
    if (is.null(ps) || !isTRUE(ps$ok)) return(NULL)
    st <- pre_state()
    pre_inp <- if (!is.null(st) && isTRUE(st$ok)) st$inp else list(sbp = ps$pre_sbp)
    rows <- vital_rows(pre_inp, pinp = ps$inp, target = ps$target, only = c("bp", "hr", "spo2", "rpe"))
    tagList(div(class = "section-label", "Post-exercise measurements"), vitals_table(rows, "post"))
  })
  
  output$discharge_instructions_output <- renderUI({
    r <- post_res_safe()
    if (is.null(r)) return(NULL)
    ab <- abnormal_only(r$instructions)
    tagList(div(class = "section-label", "Findings"),
            if (length(ab) > 0) note_items(ab) else p(class = "muted", "No abnormal findings."))
  })
  
  output$next_adjustment_output <- renderUI({
    r <- post_res_safe()
    if (is.null(r)) return(NULL)
    tagList(div(class = "section-label", "Next-session adjustment"),
            kv_table(list("Next session" = r$next_adjustment)))
  })
  
  # --------------------------------------------------------------------------
  # Evaluation log, handling record, re-test and new-patient reset
  # --------------------------------------------------------------------------
  eval_today <- reactive({
    s <- eval_store()
    mrn <- trimws(input$pt_mrn %||% "")
    if (!nzchar(mrn)) return(s[0, ])
    s[s$MRN == mrn & s$Date == Sys.Date(), ]
  })
  
  handling_today <- reactive({
    s <- handling_store()
    mrn <- trimws(input$pt_mrn %||% "")
    if (!nzchar(mrn)) return(s[0, ])
    s[s$MRN == mrn & s$Date == Sys.Date(), ]
  })
  
  output$eval_log_ui <- renderUI({
    d <- eval_today()
    if (nrow(d) == 0) return(NULL)
    tagList(div(class = "section-label", sprintf("Evaluation log - today (%d)", nrow(d))), eval_log_table(d))
  })
  
  output$handling_log_ui <- renderUI({
    d <- handling_today()
    if (nrow(d) == 0) return(NULL)
    tagList(div(class = "section-label", "Handling records - today"), handling_table(d))
  })
  
  handling_needed_r <- reactive({
    r <- pre_res_safe()
    !is.null(r) && r$status != "SAFE"
  })
  output$handling_needed <- handling_needed_r   # read by conditionalPanel in the UI
  outputOptions(output, "handling_needed", suspendWhenHidden = FALSE)
  
  observeEvent(input$hd_now, {
    updateTextInput(session, "hd_time", value = format(Sys.time(), "%H:%M"))
  })
  
  observeEvent(input$hd_save, {
    st <- pre_state()
    mrn <- trimws(input$pt_mrn %||% "")
    if (is.null(st) || !isTRUE(st$ok) || patient_changed() || !nzchar(mrn)) {
      showNotification("Run a valid evaluation for this patient first.", type = "error"); return()
    }
    if (!nzchar(input$hd_decision %||% "")) {
      session$sendCustomMessage("invalid", list(ids = list("hd_decision")))
      showNotification("Select the decision / outcome.", type = "error"); return()
    }
    t_in <- trimws(input$hd_time %||% "")
    if (nzchar(t_in) && !grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", t_in)) {
      session$sendCustomMessage("invalid", list(ids = list("hd_time")))
      showNotification("Time must be in HH:MM format.", type = "error"); return()
    }
    session$sendCustomMessage("invalid", list(ids = list()))
    row <- data.frame(
      Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), MRN = mrn, EvalTimestamp = st$ts,
      NotifiedWho = trimws(input$hd_who %||% ""), NotifiedTime = t_in,
      Decision = input$hd_decision, Reschedule = trimws(input$hd_reschedule %||% ""),
      Notes = trimws(input$hd_notes %||% ""), stringsAsFactors = FALSE)
    if (!append_log(HANDLING_FILE, row)) {
      showNotification("Could not write data/handling.csv (check folder permissions).", type = "error"); return()
    }
    handling_store(load_log(HANDLING_FILE, HANDLING_COLS))
    updateTextInput(session, "hd_who", value = "")
    updateTextInput(session, "hd_time", value = "")
    updateTextInput(session, "hd_reschedule", value = "")
    updateTextAreaInput(session, "hd_notes", value = "")
    updateSelectInput(session, "hd_decision", selected = "")
    showNotification("Handling record saved.", type = "message", duration = 4)
  })
  
  # Re-test: clear only today's vital signs so old values cannot be re-used by accident
  observeEvent(input$retest_clear, {
    for (id in c("sbp", "dbp", "hr", "spo2", "temp", "glucose")) updateNumericInput(session, id, value = "")
    showNotification("Vital signs cleared. Enter the re-test values, then press Evaluate.", type = "message", duration = 6)
  })
  
  # New patient: clear every input, result and note on screen (saved history is kept)
  reset_all <- function() {
    for (id in c("pt_name", "pt_mrn", "pt_ic", "hd_who", "hd_time", "hd_reschedule")) updateTextInput(session, id, value = "")
    updateTextAreaInput(session, "hd_notes", value = "")
    for (id in c("age", "scr", "mwt6_dist", "sts30_reps", "handgrip_kg", "sbp", "dbp", "hr", "spo2", "temp", "glucose",
                 "k_level", "hb_level", "dial_time", "dry_weight", "pre_weight",
                 "post_sbp", "post_dbp", "post_hr", "post_spo2")) updateNumericInput(session, id, value = "")
    for (id in c("ckd_category", "gender", "fistula_arm", "fistula_age", "pd_catheter_status",
                 "post_transplant_time", "post_rpe_sel", "hd_decision", "hd_vintage")) updateSelectInput(session, id, selected = "")
    for (id in c("diabetic_neuropathy", "foot_ulcer", "hypo_risk_meds", "pd_full_abdomen", "pd_hernia")) updateCheckboxInput(session, id, value = FALSE)
    for (id in c("lifestyle_risks", "symptoms", "post_symptoms")) updateCheckboxGroupInput(session, id, selected = character(0))
    updateRadioButtons(session, "scr_unit", selected = "mgdl")
    updateSelectInput(session, "start_level", selected = "standard")
    session$sendCustomMessage("clear_date", "lab_date")
    session$sendCustomMessage("invalid", list(ids = list()))
    pre_state(NULL); post_state(NULL); fitt_paused(FALSE); session_saved(FALSE)
    updateTabItems(session, "tabs", "pre_eval")
  }
  
  observeEvent(input$new_patient, {
    showModal(modalDialog(
      title = "Start a new patient?",
      "This clears all entered values, results and handling notes on screen. Saved history is not deleted.",
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_reset", "Clear and start new patient", class = "btn-primary")),
      easyClose = TRUE))
  })
  observeEvent(input$confirm_reset, {
    reset_all()
    removeModal()
    showNotification("Ready for a new patient.", type = "message", duration = 4)
  })
  
  # --------------------------------------------------------------------------
  # Save session to history (local CSV)
  # --------------------------------------------------------------------------
  observeEvent(input$save_session, {
    mrn <- trimws(input$pt_mrn %||% "")
    if (!nzchar(mrn)) {
      showNotification("Enter the patient's MRN (left sidebar) before saving.", type = "error"); return()
    }
    pst <- pre_state()
    if (is.null(pst) || !isTRUE(pst$ok)) {
      showNotification("Run a valid Pre-Test evaluation first.", type = "error"); return()
    }
    if (isTRUE(pre_stale())) {
      showNotification("Pre-Test inputs changed since evaluation - re-run Tab 1 before saving.", type = "warning"); return()
    }
    contra <- pst$res$status == "CONTRAINDICATED"
    ps <- post_state()
    post_ok <- !is.null(ps) && isTRUE(ps$ok)
    if (!contra) {
      if (!post_ok) { showNotification("Run the Post-Test evaluation before saving.", type = "error"); return() }
      if (isTRUE(post_stale())) { showNotification("Post-Test inputs changed since evaluation - re-run before saving.", type = "warning"); return() }
    }
    
    # Record creatinine / eGFR only when it is a NEW lab value for this MRN
    prev_labs <- sessions_store()
    prev_labs <- prev_labs[prev_labs$MRN == mrn & !is.na(prev_labs$sCr), ]
    cur_scr   <- round(pst$inp[["scr"]], 2)
    inc_labs  <- nrow(prev_labs) == 0 || abs(cur_scr - round(tail(prev_labs$sCr, 1), 2)) >= 0.01
    scr_v  <- if (inc_labs) pst$inp[["scr"]] else NA_real_
    egfr_v <- NA_real_
    if (inc_labs && egfr_applicable(pst$inp[["ckd_category"]])) {
      e <- calculate_egfr(pst$inp[["scr"]], pst$inp[["age"]], pst$inp[["gender"]])
      if (!is.null(e)) egfr_v <- e$egfr
    }
    
    row <- data.frame(
      Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      MRN = mrn,
      Category = pst$inp[["ckd_category"]],
      Pre_Status = pst$res$status,
      Discharge_Status = if (contra) "NOT_PERFORMED" else ps$res$discharge_status,
      sCr = scr_v, eGFR = egfr_v,
      Pre_SBP = pst$inp[["sbp"]],
      Post_SBP = if (contra) NA_real_ else ps$inp[["post_sbp"]],
      RPE = if (contra) NA_real_ else ps$inp[["post_rpe"]],
      stringsAsFactors = FALSE
    )
    
    ok <- tryCatch({
      dir.create(dirname(SESSION_FILE), showWarnings = FALSE, recursive = TRUE)
      exists <- file.exists(SESSION_FILE)
      write.table(row, SESSION_FILE, sep = ",", row.names = FALSE, col.names = !exists,
                  append = exists, qmethod = "double")
      TRUE
    }, error = function(e) FALSE)
    
    if (!ok) {
      showNotification("Could not write data/sessions.csv (check folder permissions).", type = "error"); return()
    }
    sessions_store(load_sessions())
    session_saved(TRUE)
    h <- history_all()
    if (nrow(h) > 0) updateDateRangeInput(session, "history_date_range", start = min(h$Date), end = Sys.Date())
    showNotification(sprintf("Session saved for MRN %s.%s", mrn, if (inc_labs) " New creatinine value recorded." else ""), type = "message", duration = 4)
  })
  
  # --------------------------------------------------------------------------
  # Longitudinal data (real saved sessions if any, otherwise labelled DEMO data)
  # --------------------------------------------------------------------------
  mock_history_data <- reactive({
    dates <- seq.Date(from = Sys.Date() - 60, to = Sys.Date(), by = "3 days")
    n_records <- length(dates)
    
    pre_sbp_vals <- c(135, 138, 142, 138, 150, 130, 128, 134, 132, 136, 130, 145, 138, 140, 135, 132, 130, 128, 134, 132, 130)
    post_sbp_vals <- c(125, 128, 130, 126, 160, 122, 120, 128, 124, 128, 122, 155, 130, 132, 128, 124, 122, 120, 125, 124, 122)
    rpe_vals <- c(11, 12, 12, 13, 15, 12, 11, 12, 13, 12, 11, 14, 12, 13, 12, 11, 12, 11, 12, 11, 11)
    
    scr_vals <- rep(NA_real_, n_records)
    egfr_vals <- rep(NA_real_, n_records)
    
    scr_vals[1] <- 3.2
    scr_vals[ceiling(n_records/2)] <- 3.3
    scr_vals[n_records] <- 3.5
    
    for (i in seq_len(n_records)) {
      if (!is.na(scr_vals[i])) {
        res <- calculate_egfr(scr_vals[i], input$age, input$gender)
        if (!is.null(res)) egfr_vals[i] <- res$egfr
      }
    }
    
    data.frame(
      Date = dates,
      sCr = scr_vals,
      eGFR = egfr_vals,
      Pre_SBP = pre_sbp_vals[1:n_records],
      Post_SBP = post_sbp_vals[1:n_records],
      RPE = rpe_vals[1:n_records]
    )
  })
  
  real_history <- reactive({
    s <- sessions_store()
    mrn <- trimws(input$pt_mrn %||% "")
    if (!nzchar(mrn)) return(s[0, ])
    s[s$MRN == mrn, ]
  })
  
  using_demo <- reactive(nrow(real_history()) == 0)
  
  history_all <- reactive({
    if (using_demo()) mock_history_data()
    else {
      r <- real_history()
      r[order(r$Date), c("Date", "sCr", "eGFR", "Pre_SBP", "Post_SBP", "RPE")]
    }
  })
  
  filtered_history_data <- reactive({
    df <- history_all()
    req(input$history_date_range)
    df[df$Date >= input$history_date_range[1] & df$Date <= input$history_date_range[2], ]
  })
  
  observeEvent(input$btn_preset_30, {
    updateDateRangeInput(session, "history_date_range", start = Sys.Date() - 30, end = Sys.Date())
  })
  observeEvent(input$btn_preset_90, {
    updateDateRangeInput(session, "history_date_range", start = Sys.Date() - 90, end = Sys.Date())
  })
  observeEvent(input$btn_preset_all, {
    df <- history_all()
    if (nrow(df) > 0) updateDateRangeInput(session, "history_date_range", start = min(df$Date), end = max(df$Date))
  })
  
  output$data_source_banner_ui <- renderUI({
    if (using_demo()) {
      div(class = "clinical-alert clinical-alert-warning",
          strong("DEMO DATA: "),
          "No saved sessions for this MRN. The charts, table, alert and CSV export below use SIMULATED data for illustration only - do not use for clinical decisions. Save sessions from Tab 2 to build a real history.")
    } else {
      div(class = "clinical-alert clinical-alert-info",
          sprintf("Showing %d saved session(s) for MRN %s.", nrow(real_history()), trimws(input$pt_mrn)))
    }
  })
  
  output$history_kpi_ui <- renderUI({
    df <- filtered_history_data()
    if (nrow(df) == 0) return(NULL)
    egfr_vals <- df$eGFR[!is.na(df$eGFR)]
    last_egfr <- if (length(egfr_vals) > 0 && egfr_applicable(input$ckd_category)) sprintf("%.0f", tail(egfr_vals, 1)) else "\u2014"
    avg_post <- mean(df$Post_SBP, na.rm = TRUE)
    avg_rpe  <- mean(df$RPE, na.rm = TRUE)
    high_n   <- sum(df$Post_SBP > THR$sbp_stop, na.rm = TRUE)
    sum_strip(list(
      list(label = "Sessions", value = nrow(df), sub = "in selected range"),
      list(label = "Latest eGFR", value = last_egfr, sub = "mL/min/1.73m\u00B2"),
      list(label = "Avg post SBP", value = if (is.nan(avg_post)) "\u2014" else sprintf("%.0f", avg_post), sub = "mmHg"),
      list(label = sprintf("Post SBP > %.0f", THR$sbp_stop), value = high_n, sub = "sessions", level = if (high_n > 0) "warn"),
      list(label = "Avg Borg RPE", value = if (is.nan(avg_rpe)) "\u2014" else sprintf("%.1f", avg_rpe))
    ))
  })
  
  trend_eval <- reactive({
    ev <- evaluate_longitudinal_trends(history_all(), input$ckd_category)
    ev$message <- strip_emoji(ev$message)
    ev
  })
  
  output$longitudinal_renal_alert_ui <- renderUI({
    ev <- trend_eval()
    
    if (ev$trend_status == "HIGH_RISK_AKI") {
      div(class = "clinical-alert clinical-alert-danger alert-flex-container",
          div(class = "alert-content-text",
              strong("RENAL ALERT: "), ev$message,
              if (isTRUE(fitt_paused())) div(style = "margin-top: 6px;", "FITT plan is currently PAUSED.")
          ),
          div(class = "alert-cta-group",
              actionButton("btn_refer_nephro", "Refer to Nephrologist", class = "btn-danger btn-sm", style = "font-weight:700;"),
              actionButton("btn_pause_fitt", "Pause FITT Plan", class = "btn-warning btn-sm", style = "font-weight:700;")
          )
      )
    } else if (ev$trend_status == "MODERATE_DECLINE") {
      div(class = "clinical-alert clinical-alert-warning", ev$message)
    } else if (ev$trend_status %in% c("NOT_APPLICABLE", "INSUFFICIENT_DATA")) {
      div(class = "clinical-alert clinical-alert-info", ev$message)
    } else {
      div(class = "clinical-alert clinical-alert-success", ev$message)
    }
  })
  
  observeEvent(input$btn_refer_nephro, {
    ev <- trend_eval()
    if (ev$trend_status != "HIGH_RISK_AKI") return()
    letter <- paste(c(
      "TO: Attending Nephrologist / Renal Clinic",
      sprintf("PATIENT: %s (MRN: %s)", input$pt_name, input$pt_mrn),
      sprintf("DATE: %s", as.character(Sys.Date())),
      "",
      "Dear Doctor,",
      "",
      "During physical therapy longitudinal follow-up, a possible acute change in renal function was flagged:",
      sprintf("- Serum creatinine: %.2f mg/dL (%s) -> %.2f mg/dL (%s), change %+.2f mg/dL",
              ev$prev_scr, ev$prev_date, ev$last_scr, ev$last_date, ev$scr_step),
      sprintf("- eGFR: %.1f -> %.1f mL/min/1.73m2, change %+.1f", ev$prev_egfr, ev$last_egfr, ev$egfr_step),
      sprintf("- Interval between results: %d days", as.integer(ev$gap_days)),
      if (isTRUE(fitt_paused())) "- Physical therapy exercise prescription is currently PAUSED for safety."
      else "- Please advise whether physical therapy may continue.",
      if (using_demo()) "- NOTE: generated from DEMO data (no saved sessions for this MRN).",
      "",
      "Please evaluate for possible Acute Kidney Injury (AKI) or nephrotoxic triggers.",
      "",
      "Sincerely,",
      "Renal Physical Therapy Team"
    ), collapse = "\n")
    
    showModal(modalDialog(
      title = div(style = "color: #b91c1c; font-weight: bold;", icon("file-medical"), " Urgent Nephrology Referral Letter"),
      size = "m", easyClose = TRUE, footer = modalButton("Close"),
      tags$textarea(id = "referral_text", readonly = "readonly", rows = 16,
                    style = "width: 100%; font-family: monospace; font-size: 13px; padding: 10px;", letter),
      tags$button("Copy letter", class = "btn btn-primary btn-sm", style = "margin-top: 8px;",
                  onclick = "navigator.clipboard.writeText(document.getElementById('referral_text').value)")
    ))
  })
  
  observeEvent(input$btn_pause_fitt, {
    fitt_paused(TRUE)
    showNotification("FITT Exercise Protocol paused. Re-evaluation (Tab 1) is required before the next session.", type = "warning", duration = 6)
  })
  
  output$egfr_trend_plot <- renderPlotly({
    df <- filtered_history_data()
    lab_df <- df[!is.na(df$sCr), ]
    
    if (nrow(lab_df) == 0) {
      return(plot_ly(type = "scatter", mode = "markers") %>% layout(title = "No Lab Records in Selected Range") %>% config(displayModeBar = FALSE))
    }
    
    if (input$lab_metric_toggle == "egfr") {
      lab_df <- lab_df[!is.na(lab_df$eGFR), ]
      if (!egfr_applicable(input$ckd_category) || nrow(lab_df) == 0) {
        return(plot_ly(type = "scatter", mode = "markers") %>% layout(title = "eGFR trend not applicable / not available (see sCr trend)") %>% config(displayModeBar = FALSE))
      }
      plot_ly(lab_df, x = ~Date, y = ~eGFR, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#0f6a8b', width = 2),
              marker = list(color = '#0f6a8b', size = 7),
              name = 'eGFR') %>%
        layout(
          yaxis = list(title = 'eGFR (mL/min/1.73m\u00B2)'),
          xaxis = date_axis(lab_df$Date, ''),
          margin = list(t = 15, b = 40)
        ) %>% config(displayModeBar = FALSE)
    } else {
      plot_ly(lab_df, x = ~Date, y = ~sCr, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#374151', width = 2),
              marker = list(color = '#374151', size = 7),
              name = 'sCr') %>%
        layout(
          yaxis = list(title = 'Serum creatinine (mg/dL)'),
          xaxis = date_axis(lab_df$Date, ''),
          margin = list(t = 15, b = 40)
        ) %>% config(displayModeBar = FALSE)
    }
  })
  
  output$bp_trend_plot_enhanced <- renderPlotly({
    df <- filtered_history_data()
    req(nrow(df) > 0)
    
    x_min <- min(df$Date)
    x_max <- max(df$Date)
    if (x_min == x_max) { x_min <- x_min - 7; x_max <- x_max + 7 }
    
    plot_ly(df, x = ~Date) %>%
      add_segments(x = x_min, xend = x_max, y = THR$sbp_stop, yend = THR$sbp_stop,
                   line = list(color = '#c62828', dash = 'dot', width = 1.5),
                   showlegend = FALSE, hoverinfo = "none") %>%
      add_segments(x = x_min, xend = x_max, y = THR$sbp_low, yend = THR$sbp_low,
                   line = list(color = '#b7791f', dash = 'dot', width = 1.5),
                   showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(y = ~Pre_SBP, name = 'Pre-exercise SBP', type = 'scatter', mode = 'lines+markers',
                line = list(color = '#0f6a8b', width = 2), marker = list(size = 6, color = '#0f6a8b')) %>%
      add_trace(y = ~Post_SBP, name = 'Post-exercise SBP', type = 'scatter', mode = 'lines+markers',
                line = list(color = '#b45309', width = 2), marker = list(size = 6, color = '#b45309')) %>%
      layout(
        yaxis = list(title = 'Systolic BP (mmHg)', range = c(70, 220)),
        xaxis = c(date_axis(df$Date, ''), list(range = as.character(c(x_min, x_max)))),
        legend = list(orientation = 'h', x = 0, y = 1.13),
        margin = list(t = 35, b = 40),
        shapes = list(
          list(type = "rect", fillcolor = "rgba(198, 40, 40, 0.07)", line = list(color = "transparent"),
               xref = "x", x0 = x_min, x1 = x_max, y0 = THR$sbp_stop, y1 = 220, layer = "below"),
          list(type = "rect", fillcolor = "rgba(183, 121, 31, 0.08)", line = list(color = "transparent"),
               xref = "x", x0 = x_min, x1 = x_max, y0 = 70, y1 = THR$sbp_low, layer = "below")
        )
      ) %>% config(displayModeBar = FALSE)
  })
  
  output$history_table <- DT::renderDataTable({
    df <- filtered_history_data()
    
    df_display <- df
    df_display$sCr <- ifelse(is.na(df_display$sCr), "N/A", sprintf("%.2f", df_display$sCr))
    df_display$eGFR <- ifelse(is.na(df_display$eGFR), "N/A", sprintf("%.1f", df_display$eGFR))
    df_display$Post_SBP <- ifelse(is.na(df_display$Post_SBP), "N/A", sprintf("%.0f", df_display$Post_SBP))
    df_display$RPE <- ifelse(is.na(df_display$RPE), "N/A", sprintf("%.1f", df_display$RPE))
    
    colnames(df_display) <- c("Session Date", 
                              "sCr (mg/dL)", "eGFR (mL/min)", 
                              "Pre-SBP (mmHg)", "Post-SBP (mmHg)", "Borg RPE")
    
    DT::datatable(
      df_display,
      rownames = FALSE,
      options = list(
        pageLength = 6,
        dom = 'tip',
        autoWidth = TRUE,
        headerCallback = JS(
          "function(thead, data, start, end, display) {",
          "  $(thead).find('th').eq(1).addClass('lab-header');",
          "  $(thead).find('th').eq(2).addClass('lab-header');",
          "  $(thead).find('th').eq(3).addClass('vital-header');",
          "  $(thead).find('th').eq(4).addClass('vital-header');",
          "  $(thead).find('th').eq(5).addClass('vital-header');",
          "}"
        )
      )
    )
  })
  
  output$download_history <- downloadHandler(
    filename = function() {
      mrn <- gsub("[^A-Za-z0-9_-]", "_", trimws(input$pt_mrn %||% ""))
      if (!nzchar(mrn)) mrn <- "NoMRN"
      sprintf("Renal_PT_Clinical_History_%s_%s%s.csv", mrn, Sys.Date(), if (using_demo()) "_DEMO" else "")
    },
    content = function(file) {
      write.csv(filtered_history_data(), file, row.names = FALSE)
    }
  )
  
  # --------------------------------------------------------------------------
  # Clinical report (Tab 4) - built ONLY from the evaluated snapshots
  # --------------------------------------------------------------------------
  output$report_meta_ui <- renderUI({
    input$tabs; pre_state(); post_state()   # refresh when the tab is opened / results change
    div(
      class = "med-meta-box",
      div(strong("DOC REF: "), sprintf("RPT-%s", format(Sys.time(), "%Y%m%d%H%M"))),
      div(strong("DATE: "), format(Sys.time(), "%Y-%m-%d %H:%M")),
      div(strong("STATUS: "), "CONFIDENTIAL / MEDICAL RECORD")
    )
  })
  
  output$dynamic_report_body <- renderUI({
    input$tabs
    st <- pre_state()
    ps <- post_state()
    
    incomplete <- function(msg, ...) {
      st_banner("warn", "Clinical report", "Report incomplete", msg,
                div(style = "margin-top: 10px;", ...))
    }
    
    if (is.null(st) || !isTRUE(st$ok)) {
      return(incomplete("Complete a valid Pre-Exercise Assessment (Tab 1) to generate this record.",
                        actionButton("go_tab1", "Go to Tab 1", class = "btn btn-primary btn-sm")))
    }
    
    pre <- st$res
    inp <- st$inp
    contra <- pre$status == "CONTRAINDICATED"
    post_ok <- !is.null(ps) && isTRUE(ps$ok)
    
    if (!contra && !post_ok) {
      return(incomplete("Complete the Post-Exercise Discharge evaluation (Tab 2) to finalize this record.",
                        actionButton("go_tab2", "Go to Tab 2", class = "btn btn-primary btn-sm")))
    }
    show_post <- post_ok && !contra
    post <- if (show_post) ps$res else NULL
    pinp <- if (show_post) ps$inp else NULL
    
    nz <- function(x) if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) "\u2014" else trimws(x)
    
    subpop_map <- c(
      "non_dialysis" = "Non-Dialysis CKD (Stages 1-5)",
      "dialysis" = "Haemodialysis Patient (Intradialytic)",
      "peritoneal" = "Peritoneal Dialysis Patient (PD)",
      "pre_transplant" = "Pre-Transplant Candidate (Prehab)",
      "transplant" = "Kidney Transplant Recipient"
    )
    subpop_label <- unname(subpop_map[inp[["ckd_category"]]])
    if (is.na(subpop_label)) subpop_label <- inp[["ckd_category"]]
    gender_str <- if (identical(inp[["gender"]], "male")) "Male" else "Female"
    
    egfr_res <- if (egfr_applicable(inp[["ckd_category"]])) calculate_egfr(inp[["scr"]], inp[["age"]], inp[["gender"]]) else NULL
    egfr_str <- if (!egfr_applicable(inp[["ckd_category"]])) "Not applicable (on dialysis)"
    else if (!is.null(egfr_res)) sprintf("%.1f mL/min/1.73m\u00B2 (%s)", egfr_res$egfr, sub("\\s*\\(.*$", "", egfr_res$stage))
    else "N/A"
    
    pre_level <- switch(pre$status, SAFE = "ok", CAUTION = "warn", "danger")
    dis_txt <- if (!show_post) "Not applicable" else switch(post$discharge_status, CLEARED = "Cleared", OBSERVATION_REQUIRED = "Hold - observe", "Medical review")
    dis_level <- if (!show_post) NULL else switch(post$discharge_status, CLEARED = "ok", OBSERVATION_REQUIRED = "warn", "danger")
    
    stale_note <- if (isTRUE(patient_changed())) {
      div(class = "clinical-alert clinical-alert-danger no-print",
          "The MRN was changed after this evaluation. This report belongs to the previously evaluated patient shown below.")
    } else if (isTRUE(pre_stale()) || (show_post && isTRUE(post_stale()))) {
      div(class = "clinical-alert clinical-alert-warning no-print",
          "Inputs have changed since the last evaluation. This report shows the LAST EVALUATED values; re-run the evaluation(s) to update it.")
    }
    
    rep_mrn <- st$patient$mrn
    ev_today <- eval_store(); ev_today <- ev_today[ev_today$MRN == rep_mrn & ev_today$Date == Sys.Date(), ]
    hd_today <- handling_store(); hd_today <- hd_today[hd_today$MRN == rep_mrn & hd_today$Date == Sys.Date(), ]
    
    fi <- func_items(inp)
    no_flags <- length(pre$stop_alerts) == 0 && length(pre$caution_alerts) == 0
    
    clean_css <- function(x) gsub("[\"'\\\\<>{}\n\r]", "", x)
    ident <- paste(c(if (nzchar(st$patient$name)) st$patient$name, if (nzchar(st$patient$mrn)) paste0("MRN ", st$patient$mrn)), collapse = "   |   ")
    page_style <- tags$style(HTML(sprintf(
      "@page { @top-right { content: \"%s\"; font-size: 8.5pt; color: #4b5563; } @bottom-left { content: \"Confidential medical record - %s\"; font-size: 8pt; color: #6b7280; } @bottom-right { content: \"Page \" counter(page) \" of \" counter(pages); font-size: 8pt; color: #6b7280; } }",
      clean_css(ident), clean_css(REPORT_ORG_NAME))))
    
    tagList(
      page_style,
      stale_note,
      
      div(class = "print-block", div(
        class = "med-patient-grid",
        div(class = "med-patient-item", span("Patient Full Name"), strong(nz(st$patient$name))),
        div(class = "med-patient-item", span("Medical Record No. (MRN)"), strong(nz(st$patient$mrn))),
        div(class = "med-patient-item", span("Identification / Passport"), strong(nz(st$patient$ic))),
        div(class = "med-patient-item", span("Age / Gender"), strong(sprintf("%.0f Yrs / %s", inp[["age"]], gender_str))),
        div(class = "med-patient-item", span("UKKA Clinical Sub-group"), strong(subpop_label)),
        div(class = "med-patient-item", span("Renal Function Status"), strong(egfr_str))
      ),
      
      sum_strip(list(
        list(label = "Pre-session clearance", value = switch(pre$status, SAFE = "Cleared", CAUTION = "Cleared with precautions", "Contraindicated"), level = pre_level),
        list(label = "Discharge clearance", value = dis_txt, level = dis_level, sub = if (!show_post) "Session not performed"),
        list(label = "Starting level", value = pre$functional_tier),
        list(label = "RPE target (Borg 6-20)", value = sprintf("%.0f - %.0f", pre$target_rpe[1], pre$target_rpe[2]))
      ))),
      
      div(class = "print-block",
          div(class = "med-section-title", span("1. Vital Signs & Laboratory Markers")),
          vitals_table(vital_rows(inp, pinp, if (show_post) ps$target, full = TRUE), "report")),
      
      div(class = "print-flow", div(class = "med-section-title", span("2. Safety Screening")),
          if (no_flags) p("No contraindications or cautions identified.", style = "margin: 0 0 8px 0;"),
          if (!no_flags) finding_list(pre$stop_alerts, pre$caution_alerts),
          if (length(pre$next_steps) > 0) tagList(
            div(class = "section-label", "Next steps for stop criteria"),
            p(class = "muted", style = "font-size: 11px; margin: 0 0 6px 0;", NEXT_STEPS_STATUS),
            lapply(pre$next_steps, next_step_block)
          ),
          if (length(pre$precautions) > 0) tagList(div(class = "section-label", "Clinical precautions"), tags$ul(class = "two-col-list", lapply(pre$precautions, function(p) tags$li(p)))),
          if (length(pre$lifestyle_advice) > 0) tagList(div(class = "section-label", "Lifestyle counselling"), tags$ul(class = "two-col-list", lapply(pre$lifestyle_advice, function(p) tags$li(p)))),
          if (length(fi) > 0) tagList(div(class = "section-label", sprintf("Functional capacity - starting level: %s", pre$functional_tier)), func_table(fi))),
      
      div(class = "print-block",
          div(class = "med-section-title", span("3. Evaluation Log & Handling Record")),
          if (nrow(ev_today) > 0) eval_log_table(ev_today) else p("No evaluation log entries found.", style = "margin: 0 0 6px 0;"),
          if (nrow(hd_today) > 0) tagList(div(class = "section-label", "Handling records"), handling_table(hd_today))
          else if (pre$status != "SAFE") p(class = "muted", "No handling record entered (who was informed, decision, reschedule).", style = "margin: 6px 0;")
      ),
      
      div(class = "print-block", div(
        class = "med-section-title", style = "display: flex; justify-content: space-between; flex-wrap: wrap; gap: 6px;",
        span("4. Physical Therapy Prescription (UKKA FITT)"),
        if (!contra && !isTRUE(fitt_paused())) span(class = "evidence-pill", pre$fitt$UKKA_Grade)
      ),
      if (contra) {
        p(strong("Withheld"), " - exercise prescription not issued due to a baseline clinical contraindication.", style = "margin: 0 0 8px 0;")
      } else if (isTRUE(fitt_paused())) {
        p(strong("Paused"), " - exercise prescription on hold pending medical review (renal alert).", style = "margin: 0 0 8px 0;")
      } else {
        tagList(
          div(class = "sub-pop", pre$fitt$Category),
          if (pre$status == "CAUTION") p(span(class = "tg tg-caution", "Modified"), "Apply the precautions listed above.", style = "margin: 0 0 6px 0;"),
          kv_table(list(Frequency = fitt_value(pre$fitt$Frequency), Intensity = fitt_value(pre$fitt$Intensity),
                        Time = fitt_value(pre$fitt$Time), Type = fitt_value(pre$fitt$Type), Monitoring = pre$fitt$Monitoring))
        )
      }),
      
      div(class = "print-block", style = "break-inside: avoid;", div(class = "med-section-title", span("5. Discharge Clearance & Follow-up")),
          if (!show_post) {
            kv_table(list("Discharge" = "Not applicable - session not performed.", "Guidance" = pre$next_appointment))
          } else {
            {
              ab <- abnormal_only(post$instructions)
              tagList(if (length(ab) > 0) note_items(ab) else p(class = "muted", "No abnormal post-exercise findings."),
                      kv_table(list("Next session" = post$next_adjustment)))
            }
          },
          
          div(
            class = "med-signature-grid",
            div(
              div(class = "med-signature-line"),
              div(class = "med-signature-title", "Assessing Renal Physiotherapist"),
              div(class = "med-signature-sub", "Signature, Name & Official Stamp / Date")
            ),
            div(
              div(class = "med-signature-line"),
              div(class = "med-signature-title", "Attending Nephrologist / Medical Officer"),
              div(class = "med-signature-sub", "Review Signature & Clinical Stamp / Date")
            )
          )
      )
    )
  })
}

shinyApp(ui = ui, server = server)