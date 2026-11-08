## ---- PRP-QUANT Template, version 4, as a {preregr} form ---------------------
#
# The items, their labels, descriptions, answer options, required flags and
# conditions follow the PreReg web form of the PRP-QUANT Template, version 4
# (Preregistration Task Force, 2026, https://doi.org/10.23668/psycharchives.22316;
# web form at https://apps.leibniz-psychology.org/prp-quant/, read on
# 7 November 2026), licensed CC BY 4.0.
# 
# Changes made for {preregr} (compared to the implement version 1):
#   - item identifiers are the input identifiers of the web form (e.g.
#     `PD1_start`), so that a filled form can be exported as a state file that
#     the web form directly imports; the template's numbering (e.g. "PD1.1") is
#     kept in the column `item_number`;
#   - the descriptions of item groups (PD1, PD6, M3, M4) are moved into their
#     sub-items, and the answer options are appended to the descriptions of the
#     choice items, which are validated against these descriptions;
#   - required items carry a star in their label, as on the web form;
#   - an item `contributors` displays the contributor fields of the title page;
#   - the web form's closing notes (R2 What's next?, R3 License and
#     development) are not implemented as items.


#' One item of the PRP-QUANT V4 form
#'
#' @param section Character(1). The section identifier.
#' @param id Character(1). The input identifier of the web form.
#' @param number Character(1). The item number of the template, e.g. `"PD1.1"`.
#' @param label Character(1). The item label.
#' @param description Character(1). The item description.
#' @param type Character(1). The input type of the web form: `"text"`,
#'   `"textarea"`, `"radio"` (one option), `"checkboxgroup"` (several options)
#'   or `"checkbox"` (yes or no).
#' @param choices Character or `NULL`. The options of `"radio"` and
#'   `"checkboxgroup"` items.
#' @param required Logical(1). Is the item required (starred) on the web form?
#' @param na Logical(1). Does the web form offer to mark the item as not
#'   applicable?
#' @param shown_if Character(2) or `NULL`. The item is shown on the web form
#'   only if the item named first holds (or, for `"checkboxgroup"`, includes)
#'   the option given second.
#'
#' @returns A tibble with one row.
v4_item <- function(
    section,
    id,
    number,
    label,
    description,
    type = "textarea",
    choices = NULL,
    required = FALSE,
    na = FALSE,
    shown_if = NULL
) {
  tibble::tibble(
    section     = section,
    id          = id,
    number      = number,
    label       = label,
    description = description,
    type        = type,
    choices     = list(choices),
    required    = required,
    na          = na,
    shown_if_id    = if (is.null(shown_if)) NA_character_ else shown_if[[1L]],
    shown_if_value = if (is.null(shown_if)) NA_character_ else shown_if[[2L]]
  )
}


#' Items of the PRP-QUANT V4 form
#'
#' @returns A tibble with one row per item in the order of the web form, with
#'   the columns of [v4_item()].
v4_items <- function() {
  sharing <- c(
    "as public use file",
    "as scientific use file",
    "with restricted access",
    "via secure data center",
    "upon request",
    "none",
    "not applicable"
  )
  
  sharing_levels <- paste(
    "The sharing levels are: public use file (access via download; usage for",
    "all purposes); scientific use file (access via download; usage restricted",
    "to scientific purposes); restricted access (access via download; access",
    "has to be actively granted by contributor; usage restricted to scientific",
    "purposes); secure data center (no download; usage/analysis only via",
    "remote access solutions or on-site); upon request (available upon email",
    "request by members of the scientific community); none (you do not plan to",
    "share this research product at all)."
  )
  
  sharing_intro <- paste(
    "Indicate whether/how data and code will be made available after the study",
    "(but note that not all repositories offer all options). If your study",
    "does not include the materials listed, select \"not applicable\". If you",
    "plan to share your data, remember to include a section about data sharing",
    "in the informed consent provided to the participants."
  )
  
  sharing_item <- function(id, number, product) {
    dplyr::bind_rows(
      v4_item(
        "PD",
        id,
        number,
        paste("Sharing level:", product),
        paste(sharing_intro, sharing_levels),
        type = "radio",
        choices = sharing
      ),
      v4_item(
        "PD",
        paste0(id, "_reason"),
        paste0(number, ".1"),
        paste("Reason for not sharing:", product),
        "Please state why this research product will not be shared.",
        type = "text",
        shown_if = c(id, "none")
      )
    )
  }
  
  duration <- paste(
    "Estimated duration of the project (if applicable): include the best",
    "estimate for how long the project will take from the start of data",
    "collection to project completion."
  )
  
  m4 <- "Indicate:"

  dplyr::bind_rows(

    # ── Title page ───────────────────────────────────────────────────────────
    v4_item(
      "T",
      "contributors",
      "",
      "Contributors",
      paste(
        "For each contributor: full name, affiliation, department, ORCID, and",
        "contributor role (Author(s) / Creator(s), Advisor(s), or Other",
        "kind(s) of contributor). Entered as contributor fields on the web",
        "form."
      )
    ),
    v4_item(
      "T",
      "T1",
      "T1",
      "Title",
      paste(
        "Give your research project a title. Please note that this is the",
        "title for the preregistration, not the resulting publication."
      ),
      type = "text",
      required = TRUE
    ),
    v4_item(
      "T",
      "T2",
      "T2",
      "Alternative title",
      paste(
        "Is there an alternative title (e.g., abbreviation or translation)? If",
        "your title is not in English, please provide the English translation",
        "here."
      ),
      type = "text",
      na = TRUE
    ),
    v4_item(
      "T",
      "T4",
      "T4",
      "Principal investigator and corresponding author",
      paste(
        "Who is the principal investigator for this project? As they will",
        "serve as the corresponding author to this preregistration, please",
        "provide their e-mail address."
      ),
      type = "text",
      required = TRUE
    ),
    v4_item(
      "T",
      "T5",
      "T5",
      "Versioning information", paste(
        "Indicate whether this is the first preregistration of your study or",
        "an update to an existing one (if you have not yet collected or",
        "analyzed your data; see the help section for more information). If",
        "this is an update, provide the DOI of the previous preregistration",
        "version."
      ),
      type = "radio",
      required = TRUE,
      choices = c(
        "This is the first preregistration of this study.",
        "This is an updated version.",
        paste(
          "This is a preregistration of a sub-project within a bigger project",
          "that is preregistered elsewhere."
        )
      )
    ),
    v4_item(
      "T",
      "T5_1",
      "T5.1",
      "Persistent identifier of the previous version", paste(
        "Insert the link to the previous version below; this should be a",
        "persistent identifier such as a DOI."
      ),
      type = "text",
      shown_if = c("T5", "This is an updated version.")
    ),
    v4_item(
      "T",
      "T5_2",
      "T5.2",
      "Persistent identifier of the overarching project",
      paste(
        "Insert the link to the preregistration of the overarching project",
        "below; this should be a persistent identifier such as a DOI."
      ),
      type = "text",
      shown_if = c(
        "T5",
        paste(
          "This is a preregistration of a sub-project within a bigger project",
          "that is preregistered elsewhere."
        )
      )
    ),

    # ── Project documentation ────────────────────────────────────────────────
    v4_item(
      "PD",
      "PD1_start",
      "PD1.1",
      "Planned beginning of data collection",
      duration,
      type = "text",
      required = TRUE
    ),
    v4_item(
      "PD",
      "PD1_end",
      "PD1.2",
      "Planned end of the project",
      paste(
        duration,
        "This does not refer to the end of data collection, but to finalizing",
        "data analysis and dissemination."
      ),
      type = "text",
      required = TRUE
    ),
    v4_item(
      "PD",
      "PD2",
      "PD2",
      paste(
        "Ethical considerations: Has appropriate institutional approval been",
        "obtained?"
      ),
      "Please select an option.",
      type = "radio",
      required = TRUE,
      choices = c(
        "Yes",
        "Not yet but is planned",
        "Study is/will be exempt from ethical board review"
      )
    ),
    v4_item(
      "PD",
      "PD2_1",
      "PD2.1",
      "ID of the ethics proposal",
      "Please provide the ID that has been assigned to your ethics proposal.",
      shown_if = c("PD2", "Yes")
    ),
    v4_item(
      "PD",
      "PD2_2",
      "PD2.2",
      "Reason for ethical exemption",
      "Please provide your reasoning.",
      shown_if = c("PD2", "Study is/will be exempt from ethical board review")
    ),
    v4_item(
      "PD",
      "PD3",
      "PD3",
      "Conflict of Interest Statement",
      paste(
        "Identify any real or perceived conflicts of interest with this",
        "study's execution (that are known at the time of preregistration).",
        "For example, any interests or activities that might be seen as",
        "influencing the research (e.g., financial interests in a test or",
        "procedure, funding by pharmaceutical companies for research)."
      ),
      type = "radio",
      required = TRUE,
      choices = c(
        paste(
          "All authors declare that they have no conflicts of interest in the",
          "conduct of this study."
        ),
        "There is a potential conflict of interest."
      )
    ),
    v4_item(
      "PD",
      "PD3_1",
      "PD3.1",
      "Please elaborate",
      paste(
        "Identify any real or perceived conflicts of interest with this",
        "study's execution (that are known at the time of preregistration).",
        "For example, any interests or activities that might be seen as",
        "influencing the research (e.g., financial interests in a test or",
        "procedure, funding by pharmaceutical companies for research)."
      ),
      shown_if = c("PD3", "There is a potential conflict of interest.")
    ),
    v4_item(
      "PD",
      "PD4",
      "PD4",
      "Funding",
      "Provide information on any funding you will receive for the study.",
      type = "radio",
      required = TRUE,
      choices = c(
        "No separate funding",
        "University/in-house budget",
        paste(
          "Third-party funding by public or private research funding",
          "organisation/foundation"
        ),
        "Industrial third-party funding/sponsor",
        "Other source(s)"
      )
    ),
    v4_item(
      "PD",
      "PD4_1",
      "PD4.1",
      "Faculty/department/research group(s)",
      "Please name the faculty/department/research group(s).",
      type = "text",
      shown_if = c("PD4", "University/in-house budget")
    ),
    v4_item(
      "PD",
      "PD4_2",
      "PD4.2",
      "Organisation(s)/foundation(s)",
      paste(
        "Please name organisation(s)/foundation(s), e.g., DFG, BMFTR, EU, ERC,",
        "VolkswagenStiftung."
      ),
      type = "text",
      shown_if = c(
        "PD4",
        paste(
          "Third-party funding by public or private research funding",
          "organisation/foundation"
        )
      )
    ),
    v4_item(
      "PD",
      "PD4_3",
      "PD4.3",
      "Funding source(s)",
      "Please name source(s).",
      type = "text",
      shown_if = c("PD4", "Industrial third-party funding/sponsor")
    ),
    v4_item(
      "PD",
      "PD4_4",
      "PD4.4",
      "Please specify the funding source",
      "Please specify.",
      type = "text",
      shown_if = c("PD4", "Other source(s)")
    ),
    v4_item(
      "PD",
      "PD4_5",
      "PD4.5",
      "Funding ID/reference number",
      "Indicate the funding ID/reference number, if available.",
      type = "text"
    ),
    v4_item(
      "PD",
      "PD5",
      "PD5",
      "Software",
      paste(
        "Indicate which software will be used for data collection and data",
        "analysis (e.g., OpenSesame will be used to implement the experiment,",
        "R will be used to analyze the data)."
      ),
      na = TRUE
    ),
    sharing_item(
      "PD6_experimental_code",
      "PD6.1",
      "Experimental code"
    ),
    sharing_item(
      "PD6_analysis_code",
      "PD6.2",
      "Analysis code"
    ),
    sharing_item(
      "PD6_raw_data",
      "PD6.3",
      "Raw data"
    ),
    sharing_item(
      "PD6_processed_data",
      "PD6.4",
      "Processed data"
    ),
    v4_item(
      "PD",
      "PD7",
      "PD7",
      "Planned repositories",
      paste(
        "Specify the planned repository(s) for archiving your materials or, if",
        "you have already published them, provide their persistent",
        "identifier(s)."
      ),
      na = TRUE
    ),
    v4_item(
      "PD",
      "PD8",
      "PD8",
      "Other information",
      paste(
        "If there is anything else you want to add regarding your project",
        "documentation, you can do so here."
      )
    ),

    # ── Abstract ─────────────────────────────────────────────────────────────
    v4_item(
      "A",
      "A1",
      "A1",
      "Project description",
      "Write a short description of the study you want to preregister.",
      required = TRUE
    ),
    v4_item(
      "A",
      "A2",
      "A2",
      "Keywords",
      paste(
        "Include terms specific to your topic, methodology, and the",
        "population, in case it is special. If you need help with keywords,",
        "try a keyword search using your proposed keywords in a search engine",
        "to check the results. We recommend using between 3 and 7 keywords",
        "(e.g., \"evaluative learning, subliminal influence, implicit",
        "learning, replication\")."
      ),
      type = "text",
      required = TRUE
    ),
    v4_item(
      "A",
      "A3",
      "A3",
      "Research area",
      "Indicate the research area of your study.",
      type = "radio",
      choices = c(
        "Clinical psychology",
        "Cognitive psychology",
        "Developmental psychology",
        "Educational psychology",
        "Personality psychology",
        "Social psychology",
        "Work, organizational, and business psychology",
        "Other"
      )
    ),
    v4_item(
      "A",
      "A3_1",
      "A3.1",
      "Please specify the research area",
      "Indicate the research area of your study.",
      type = "text",
      shown_if = c("A3", "Other")
    ),

    # ── Introduction ─────────────────────────────────────────────────────────
    v4_item(
      "I",
      "I1",
      "I1",
      "Theoretical background",
      paste(
        "Provide a concise overview that justifies the research",
        "questions/hypotheses (you can use bullet points). This should contain",
        "any relevant theoretical and/or practical/methodological",
        "justification. If this is a replication or follow-up study of",
        "previous research, describe it and its relationship to the current",
        "study. Please include references here and list them at the end of",
        "this document in the reference section (see R1)."
      )
    ),
    v4_item(
      "I",
      "I2",
      "I2",
      "Objectives and research question(s)",
      paste(
        "Outline the a) objectives and b) research questions that inform the",
        "methodology and analyses described below."
      )
    ),
    v4_item(
      "I",
      "I3",
      "I3",
      "Hypotheses (H1, H2, \u2026)",
      paste(
        "If you test hypotheses in your study: Provide hypotheses for",
        "predicted results in terms of the specific constructs that will",
        "inform the independent and dependent variables and predicted patterns",
        "(e.g., main effects, interactions). If multiple hypotheses, uniquely",
        "number them (e.g., H1, H2a, H2b) and refer to them the same way at",
        "other points in this registration document and the manuscript."
      ),
      required = TRUE
    ),
    v4_item(
      "I",
      "I3_1",
      "I3.1",
      "Purely exploratory study",
      paste(
        "This study is purely exploratory (please add research questions under",
        "I4 below)."
      ),
      type = "checkbox"
    ),
    v4_item(
      "I",
      "I4",
      "I4",
      "Exploratory research questions (E1, E2, \u2026 - optional)",
      paste(
        "If you already know that you want to examine exploratory research",
        "questions: Provide a rationale for your exploratory questions here.",
        "If multiple exploratory research questions, uniquely number them (E1,",
        "E2a, E2b, ...) and refer to them in the same way in the registration",
        "document and future publications."
      )
    ),

    # ── Method: data collection ──────────────────────────────────────────────
    v4_item(
      "MD",
      "M1",
      "M1",
      "Time point of registration",
      paste(
        "Indicate at which point in the project the study is preregistered.",
        "Select one of the options and add an explanatory text if you select",
        "\"Other\"."
      ),
      type = "radio",
      required = TRUE,
      choices = c(
        "Registration before creation/collection of data",
        paste(
          "Registration before any human observation of the data (i.e., prior",
          "to anyone looking into the data)"
        ),
        paste(
          "Registration before accessing the data (e.g., when conducting",
          "secondary data analysis)"
        ),
        "Registration before analysis of the data",
        "Other"
      )
    ),
    v4_item(
      "MD",
      "M1_other",
      "M1",
      "Please specify the time point of registration",
      "Please specify.",
      shown_if = c("M1", "Other")
    ),
    v4_item(
      "MD",
      "M1_1",
      "M1.1",
      "Pre-existing data: Publication status, source, and knowledge of",
      paste(
        "If data already exists: Indicate whether the data were previously",
        "published and specify the source of the data (e.g., DOI or APA style",
        "reference of original publication). You can specify your level of",
        "knowledge of the data in more detail below (e.g., descriptive",
        "statistics from previous publications) and whether or not this is",
        "relevant for the hypotheses of the present study."
      ),
      required = TRUE,
      na = TRUE
    ),
    v4_item(
      "MD",
      "M2",
      "M2",
      "Piloting",
      paste(
        "Describe all relevant pilot study/studies. If already conducted,",
        "describe what you piloted and what you learned. If planned, describe",
        "what you intend to pilot. You can also add any results, materials, or",
        "other documentation of the pilot(s) as appendices to your",
        "preregistration."
      ),
      na = TRUE
    ),
    v4_item(
      "MD",
      "M3_sample_size",
      "M3.1",
      "Planned sample size",
      "Report relevant planned sample sizes.",
      required = TRUE
    ),
    v4_item(
      "MD",
      "M3_justification",
      "M3.2",
      "Sample size justification",
      paste(
        "For designs with fixed sample sizes, provide a rationale (e.g., power",
        "analysis/curves, practical reasons). Give references, if appropriate."
      ),
      required = TRUE
    ),
    v4_item(
      "MD",
      "M3_stopping_rule",
      "M3.3",
      "Stopping rule",
      paste(
        "If data collection is not terminated by achieving full sample size,",
        "indicate your 'stopping rule' (e.g., for sequential designs, indicate",
        "the points at which you intend to be viewing your data and in any way",
        "analyzing them)."
      ),
      required = TRUE
    ),
    v4_item(
      "MD",
      "M4_population",
      "M4.1",
      "Population of interest",
      paste(
        m4,
        "Which population you are generally interested in (i.e., the group of",
        "persons or other elements that are the object of research and to",
        "which any analytic results refer)."
      )
    ),
    v4_item(
      "MD",
      "M4_recruitment",
      "M4.2",
      "Recruitment method",
      paste(
        m4,
        "Recruitment method (e.g., subject pool advertisement, community",
        "events, crowdsourcing platforms, snowball sampling)."
      )
    ),
    v4_item(
      "MD",
      "M4_characteristics",
      "M4.3",
      "Planned participant characteristics",
      paste(
        m4,
        "Planned participant characteristics (e.g., gender, race/ethnicity,",
        "sexual orientation, and gender identity, socioeconomic status,",
        "education level, age, disability or health status, geographic",
        "location)."
      )
    ),
    v4_item(
      "MD",
      "M4_quotas",
      "M4.4",
      "Quota or stratification sampling",
      paste(
        m4,
        "Details of any quota sampling or stratification sampling (i.e.,",
        "splitting a population into subpopulations and sampling each",
        "subpopulation independently) used."
      ),
      na = TRUE
    ),
    v4_item(
      "MD",
      "M4_compensation",
      "M4.5",
      "Compensation amount and method",
      paste(
        m4,
        "Compensation amount and method (e.g., same payment to all, pay based",
        "on performance, lottery)."
      ),
      na = TRUE
    ),
    v4_item(
      "MD",
      "M5",
      "M5",
      "How will participant drop-out be handled?",
      paste(
        "Indicate any special treatment for participants who drop out (i.e.,",
        "who are recruited but withdrawn before the study concludes), e.g.,",
        "whether there is a follow-up in a manner different from the main",
        "sample, the last value is carried forward, or participants are",
        "replaced. Note that procedures to handle missing data during data",
        "analyses are inquired later in AP5."
      ),
      na = TRUE
    ),
    v4_item(
      "MD",
      "M6",
      "M6",
      "Masking of participants and researchers",
      paste(
        "Indicate whether you will use any form of masking. If yes, describe",
        "the masking procedure (e.g., are administrators, data collectors,",
        "raters, or confederates unaware of the condition to which",
        "participants were assigned; are participants, experimenters, or",
        "analysts unaware of the hypotheses). Please note: If you use masking,",
        "ethics approval should always be obtained (see item PD2). If you have",
        "not yet obtained one, please do so."
      ),
      na = TRUE
    ),
    v4_item(
      "MD",
      "M7",
      "M7",
      "Ensuring quality during data collection",
      paste(
        "Indicate all measures of data quality control during data collection",
        "(e.g., standard lab practices, lab protocols, interrater reliability",
        "training, approaches for coding of data, discarding of cases,",
        "correction of scores, or dealing with participants who were",
        "incorrectly sampled although they do not meet the inclusion",
        "criteria)."
      ),
      na = TRUE
    ),
    v4_item(
      "MD",
      "M8",
      "M8",
      "Other information",
      paste(
        "If there is anything else you want to add regarding your sampling",
        "procedure and data collection, describe it here."
      )
    ),

    # ── Method: conditions and design ────────────────────────────────────────
    v4_item(
      "MC",
      "M9",
      "M9",
      "Research method",
      "Characterize the study type by means of research method.",
      type = "radio",
      required = TRUE,
      choices = c(
        "Experiment",
        "Correlational/observational study",
        "Other"
      )
    ),
    v4_item(
      "MC",
      "M9_1",
      "M9.1",
      "Experimental design",
      "Please select an option.",
      type = "radio",
      choices = c(
        "Design with repeated measures (only \"within-subject\" variables)",
        "Group comparison (only \"between-subjects\" variables)",
        "Mixed design (\"within-subjects\" and \"between-subjects\" variables)"
      ),
      shown_if = c("M9", "Experiment")
    ),
    v4_item(
      "MC",
      "M9_1_1",
      "M9.1.1",
      "Conditions and their implementation",
      paste(
        "Please name the conditions and describe how they are implemented",
        "(e.g., summary of procedure, number of stimuli in a within design)."
      ),
      shown_if = c("M9", "Experiment")
    ),
    v4_item(
      "MC",
      "M9_2",
      "M9.2",
      "Time dimension of data collection",
      paste(
        "State the time dimension of your data collection, whether it is a",
        "single or multiple measurement occasion."
      ),
      type = "radio",
      choices = c(
        "Cross-sectional study",
        "Longitudinal study"
      ),
      shown_if = c("M9", "Correlational/observational study")
    ),
    v4_item(
      "MC",
      "M9_2_1",
      "M9.2.1",
      "Measurement plan",
      paste(
        "Please provide information on the measurement plan that will be",
        "applied (e.g., follow-up, longitudinal analysis, cohort study,",
        "sequence model). If applicable, differentiate information by group or",
        "cohort."
      ),
      shown_if = c("M9_2", "Longitudinal study")
    ),
    v4_item(
      "MC",
      "M9_3",
      "M9.3",
      "Please describe the study type",
      "Please describe the study type in more detail.",
      shown_if = c("M9", "Other")
    ),
    v4_item(
      "MC",
      "M10",
      "M10",
      "Multilevel structure",
      paste(
        "If your data is clustered into multiple levels (e.g., students nested",
        "within classrooms, employees nested within organizations), provide",
        "more information about the data structure."
      ),
      na = TRUE
    ),
    v4_item(
      "MC",
      "M11",
      "M11",
      "Randomization of participants and/or experimental materials",
      paste(
        "If applicable, describe a) how participants are assigned to",
        "conditions or treatments, b) how stimuli are assigned to conditions,",
        "and c) how the presentation of tests, trials, etc. is randomized or",
        "counterbalanced. Indicate the randomization technique and whether",
        "constraints were applied (pseudo-randomization). Indicate any type of",
        "balancing across participants (e.g., assignments of responses to",
        "hands, etc.)."
      ),
      na = TRUE
    ),
    v4_item(
      "MC",
      "M12",
      "M12",
      "Variables used in the confirmatory analyses",
      paste(
        "This section shall be used to unambiguously clarify which variables",
        "are used to operationalize the hypotheses specified in item I3.",
        "Please a) list all measured and manipulated variables relevant to",
        "test your hypotheses, b) explicitly state the functional role of each",
        "variable (i.e., independent variable, dependent variable, covariate,",
        "mediator, moderator), c) describe any relevant study materials (e.g.,",
        "stimulus materials, questionnaires) or provide their persistent",
        "identifier, and describe adaptations made to existing measures, d)",
        "provide information about the number of items/observations and how",
        "you will combine them into indices. The description here shall be",
        "consistent with the statistical analysis plans specified in the",
        "analyses section under AP8."
      ),
      required = TRUE,
      na = TRUE
    ),
    v4_item(
      "MC",
      "M13",
      "M13",
      "Other variables",
      paste(
        "Here, you can list any other variables that will be collected in the",
        "study, e.g., for answering the exploratory research questions",
        "specified in item I4."
      ),
      required = TRUE,
      na = TRUE
    ),
    v4_item(
      "MC",
      "M14",
      "M14",
      "Exhaustiveness of variables",
      paste(
        "Confirm that the above information is complete before proceeding.",
        "Whichever option you choose, please note that adding or removing",
        "variables or changing their roles at a later date is a deviation from",
        "this preregistration."
      ),
      type = "radio",
      required = TRUE,
      choices = c(
        paste(
          "All variables of the planned study are outlined here and their",
          "roles are clearly defined. We are aware that any changes to the",
          "variables and their defined roles is a deviation from this",
          "preregistration and must be reported in the article."
        ),
        paste(
          "Our planned study includes additional variables that are not listed",
          "here."
        )
      )
    ),
    v4_item(
      "MC",
      "M14_1",
      "M14.1",
      "More information",
      "Please explain why not all variables are listed above.",
      shown_if = c(
        "M14",
        paste(
          "Our planned study includes additional variables that are not listed",
          "here."
        )
      )
    ),
    v4_item(
      "MC",
      "M15",
      "M15",
      "Collection mode",
      paste(
        "Describe the method that will be used to collect data (multiple",
        "answers are possible)."
      ),
      type = "checkboxgroup",
      choices = c(
        "Observations of participant behavior by a project contributor",
        "Recordings or pictures (audio/video/photo)",
        "Surveys (e.g., interviews, questionnaires)",
        paste(
          "Instrumental measurements (e.g., response times, eye tracking,",
          "motion tracking, EEG, ECG, pulse sensor, breathing, (f)MRI, body",
          "dimensions)"
        ),
        "Other"
      )
    ),
    v4_item(
      "MC",
      "M15_1",
      "M15.1",
      "Please specify the collection mode",
      paste(
        "Describe the method that will be used to collect data (multiple",
        "answers are possible)."
      ),
      type = "text",
      shown_if = c("M15", "Other")
    ),
    v4_item(
      "MC",
      "M16",
      "M16",
      "Study materials",
      paste(
        "Please describe any relevant study materials (e.g., stimulus",
        "materials used for experiments, questionnaires used for rating",
        "studies, training protocols for intervention studies) or provide",
        "their persistent identifier. State any adaptations made to existing",
        "measures."
      )
    ),
    v4_item(
      "MC",
      "M17",
      "M17",
      "Study procedures",
      paste(
        "Please describe any relevant information about how the study will be",
        "conducted (e.g., the number of blocks, the number of training",
        "sessions in interventional studies, questionnaire/study",
        "administration for online assessments/laboratory setting)."
      )
    ),
    v4_item(
      "MC",
      "M18",
      "M18",
      "Other information",
      paste(
        "If there is anything else you want to add regarding your conditions",
        "and design, describe it here."
      )
    ),

    # ── Analysis plan ────────────────────────────────────────────────────────
    v4_item(
      "AP",
      "AP1",
      "AP1",
      "Criteria for exclusion of participants & outlier treatment",
      paste(
        "Describe: a) inclusion/exclusion criteria for the data collection",
        "(e.g., age, visual acuity, language facility), b) all criteria that",
        "will lead to the exclusion of a participant's data during analyses",
        "(e.g., attention checks, performance criteria, non-responding in",
        "physiological measures, incomplete data). Be as specific as possible",
        "(e.g., detail cut-offs, use of list- or case-wise exclusion). Clarify",
        "whether a participant who is an outlier in one variable will only be",
        "excluded from the analyses that involve that specific variable or if",
        "they will be excluded from all tests."
      ),
      required = TRUE,
      na = TRUE
    ),
    v4_item(
      "AP",
      "AP2",
      "AP2",
      "Criteria for post-data collection exclusions on trial or item level",
      paste(
        "Describe all criteria that will lead to the exclusion of a trial or",
        "item (e.g., statistical outliers, response time, reliability scores,",
        "incorrect trials). Be as specific as possible."
      ),
      required = TRUE,
      na = TRUE
    ),
    v4_item(
      "AP",
      "AP3",
      "AP3",
      "Exhaustiveness of exclusion criteria",
      paste(
        "Confirm that the above information is complete before proceeding.",
        "Whichever option you choose, adding additional exclusion criteria at",
        "a later date is a deviation from this preregistration."
      ),
      type = "radio",
      required = TRUE,
      choices = c(
        paste(
          "All exclusion criteria (on the participant and trial/item level)",
          "that are currently planned are listed above."
        ),
        "Not all exclusion criteria are listed above."
      )
    ),
    v4_item(
      "AP",
      "AP3_1",
      "AP3.1",
      "More information",
      "Please explain why some exclusion criteria are not described.",
      shown_if = c("AP3", "Not all exclusion criteria are listed above.")
    ),
    v4_item(
      "AP",
      "AP4",
      "AP4",
      "Data preprocessing",
      paste(
        "Describe all data manipulations that are performed in preparation of",
        "the main analyses (e.g., calculation of variables or scales,",
        "recoding, any data transformations, preprocessing steps for imaging",
        "or physiological data). In addition, indicate all steps related to",
        "data quality control during analyses."
      ),
      na = TRUE
    ),
    v4_item(
      "AP",
      "AP5",
      "AP5",
      "How will missing data be handled?",
      paste(
        "Indicate a) the definition of missing cases, b) any procedures that",
        "will be applied during the analysis to deal with missing data, such",
        "as case deletions; averaging across scale items (to handle missing",
        "items for some); imputation procedures; intention to treat analysis",
        "and per protocol analysis, c) the method to check for randomness of",
        "missingness (MAR, MCAR, MNAR assumptions)."
      ),
      na = TRUE
    ),
    v4_item(
      "AP",
      "AP6",
      "AP6",
      "Reliability analysis (if applicable)",
      paste(
        "Specify the type of scale reliability that will be estimated. In a",
        "study involving measure development, researchers should specify",
        "criteria for removing items from measures a priori (e.g., largest",
        "factor loading magnitude, smallest drop in alpha-if-item removed)."
      ),
      na = TRUE
    ),
    v4_item(
      "AP",
      "AP7",
      "AP7",
      "Descriptive statistics",
      paste(
        "Specify which descriptive statistics will be calculated for which",
        "variables (refer to the variables specified in the previous section).",
        "If descriptive statistics are linked to specific hypotheses in I3,",
        "explicitly link the information given here to the respective",
        "hypothesis."
      ),
      na = TRUE
    ),
    v4_item(
      "AP",
      "AP8",
      "AP8",
      "Statistical models (for each hypothesis if varies)",
      paste(
        "If you test hypotheses in your study: Specify the statistical model",
        "(e.g., t-test, ANOVA, LMM) that will be used to test each of your",
        "hypotheses described in I3. Give all necessary information about",
        "model specification (e.g., variables, interactions, planned",
        "contrasts, estimation technique, how will the standard error be",
        "calculated) and follow-up analyses. Describe precisely for which",
        "hypothesis which effect (e.g., main effect, interaction) is being",
        "tested. Include model selection criteria (e.g., fit indices) and",
        "corrections for multiple testing, if applicable."
      ),
      required = TRUE,
      na = TRUE
    ),
    v4_item(
      "AP",
      "AP9",
      "AP9",
      "Assumption tests",
      paste(
        "For all statistical models, describe the following: a) which",
        "assumptions are checked (e.g., normality, homoscedasticity,",
        "linearity, homogeneity of variances, sphericity), b) how these",
        "assumptions are checked (e.g., type of test like Levene's test, alpha",
        "level etc.), c) what is to be done in case of violations (e.g.,",
        "transformations, non-parametric tests, etc.)."
      ),
      na = TRUE
    ),
    v4_item(
      "AP",
      "AP10",
      "AP10",
      "Inference criteria",
      paste(
        "If you test hypotheses in your study: Specify the criteria used for",
        "inferences (e.g., p-values, Bayes factors, confidence intervals,",
        "effect size measures) and the thresholds for accepting or rejecting",
        "your hypotheses. State clearly whether all sub-hypotheses must be",
        "significant for the overall hypothesis to be supported. If possible,",
        "define the smallest effect size of interest. Describe which effect",
        "size measures will be reported and how they are calculated."
      ),
      na = TRUE
    ),
    v4_item(
      "AP",
      "AP11",
      "AP11",
      "Exploratory analyses (optional)",
      paste(
        "Based on the exploratory research questions outlined in I4, describe",
        "any exploratory analyses to be conducted with your data. Include any",
        "planned analyses that are not confirmatory in the sense of being a",
        "direct test of one of the specified hypotheses. Please note that",
        "other non-registered exploratory analyses can be added in the",
        "article."
      )
    ),
    v4_item(
      "AP",
      "AP12",
      "AP12",
      "Other information",
      paste(
        "If there is anything else you want to add regarding your analysis",
        "plan, describe it here."
      )
    ),

    # ── Other information and references ─────────────────────────────────────
    v4_item(
      "O",
      "O1",
      "O1",
      "Other information",
      paste(
        "If there is any other information you want to add to your",
        "preregistration, describe it here."
      )
    ),
    v4_item(
      "R",
      "R1",
      "R1",
      "References (optional)",
      paste(
        "Enter your references below. Feel free to include any references you",
        "plan to cite in your study, even if you have not yet referenced them",
        "in this protocol. Use a consistent format (e.g.,",
        "https://apastyle.apa.org/style-grammar-guidelines/references/examples)."
      )
    )
  )
}


#' Answer keys of the web form's state file that hold no answer
#'
#' The headers of item groups and the closing notes, which the state file
#' saved by the web form keeps as `NULL`, placed before the item they precede.
#'
#' @returns A named character: the header key, named by the item it precedes.
v4_header_keys <- function() {
  c(
    PD1_start      = "PD1",
    PD6_experimental_code = "PD6",
    M3_sample_size = "M3",
    M4_population  = "M4"
  )
}


#' Validation expression of a PRP-QUANT V4 item
#'
#' @param type,choices See [v4_item()].
#'
#' @returns Character(1), an R expression in `VALUE`, or `NA` for free text.
v4_validation <- function(type, choices) {
  quote_vec <- function(x) paste(deparse(x), collapse = "")

  switch(
    type,
    radio = sprintf("is.na(VALUE) || (VALUE %%in%% %s)", quote_vec(choices)),
    checkboxgroup = sprintf(
      "is.na(VALUE) || all(trimws(strsplit(VALUE, ';', fixed = TRUE)[[1]]) %%in%% %s)",
      quote_vec(choices)
    ),
    checkbox = "is.na(VALUE) || (VALUE %in% c('Yes', 'No'))",
    NA_character_
  )
}


#' The PRP-QUANT Template, version 4, as a {preregr} form
#'
#' @param na_text Character(1). The text that marks an item as not applicable,
#'   which the web form records with its "NA" checkbox.
#'
#' @returns A `{preregr}` form, to pass to [preregr::prereg_initialize()]. Its
#'   items carry the extra columns `item_number`, `item_title` (the label
#'   without the star), `item_type`, `item_choices` (options separated by
#'   `" || "`), `item_required`, `item_na`, `item_shown_if_id` and
#'   `item_shown_if_value`.
form_prpquant_v4 <- function(na_text = "Not applicable.") {
  sections <- tibble::tribble(
    ~id,  ~label,
    "T",  "Title page",
    "PD", "Project documentation",
    "A",  "Abstract",
    "I",  "Introduction",
    "MD", "Method - Data collection",
    "MC", "Method - Conditions and design",
    "AP", "Analysis plan",
    "O",  "Other information",
    "R",  "References"
  )
  items <- v4_items()
  stopifnot(
    !anyDuplicated(items$id),
    all(items$section %in% sections$id),
    all(stats::na.omit(items$shown_if_id) %in% items$id),
    sum(items$required) == 24L
  )

  form <- preregr::form_create(
    title    = "Psychological Research Preregistration-Quantitative (PRP-QUANT) Template, version 4",
    version  = "4",
    author   = "Preregistration Task Force",
    date     = "2026-07-16",
    source   = "https://doi.org/10.23668/psycharchives.22316",
    comments = "Adapted for {preregr} under CC BY 4.0; see common/R/prereg-prpquant-v4.R for the changes."
  )

  form <- preregr::form_add_instruction(
    form,
    heading = "PRP-QUANT Template, version 4",
    description = paste(
      "The items follow the PreReg web form of the PRP-QUANT Template, version",
      "4 (https://doi.org/10.23668/psycharchives.22316), licensed CC BY 4.0.",
      "Required items are starred. Choice items take one of their options",
      "exactly; several options of a multiple-choice item are separated by",
      "semicolons."
    )
  )

  for (i in seq_len(nrow(sections))) {
    form <- preregr::form_add_section(
      form,
      id          = sections$id[i],
      label       = sections$label[i],
      description = NA
    )
  }

  for (i in seq_len(nrow(items))) {
    it <- items[i, ]
    choices <- it$choices[[1L]]

    description <- it$description
    if (!is.null(choices)) {
      description <- paste0(
        description, " Options: ",
        paste0("\u201c", choices, "\u201d", collapse = "; "), "."
      )
    }
    if (it$type == "checkbox") {
      description <- paste(description, "Answer \u201cYes\u201d or \u201cNo\u201d.")
    }
    if (!is.na(it$shown_if_id)) {
      description <- paste0(
        description, " Shown on the web form only if ", it$shown_if_id,
        " is \u201c", it$shown_if_value, "\u201d."
      )
    }

    form <- preregr::form_add_item(
      form,
      id          = it$id,
      label       = if (it$required) paste(it$label, "\u2605") else it$label,
      description = description,
      section_id  = it$section,
      validation  = v4_validation(it$type, choices)
    )
  }

  rows <- match(form$items$item_id, items$id)
  form$items$item_number <- items$number[rows]
  form$items$item_title <- items$label[rows]
  form$items$item_type <- items$type[rows]
  form$items$item_choices <- vapply(
    items$choices[rows],
    \(x) if (is.null(x)) NA_character_ else paste(x, collapse = " || "),
    character(1)
  )
  form$items$item_required <- items$required[rows]
  form$items$item_na <- items$na[rows]
  form$items$item_shown_if_id <- items$shown_if_id[rows]
  form$items$item_shown_if_value <- items$shown_if_value[rows]
  form$items$item_error <- ifelse(
    is.na(form$items$item_validation),
    NA_character_,
    "NAME is not one of its options."
  )
  # {preregr} names the metadata column `contents` here and copies it to
  # `content` on initialisation; appending by position works for both
  form$metadata[nrow(form$metadata) + 1L, ] <- c("na_text", na_text)

  form
}
