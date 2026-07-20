# Summary tables for pitch grades, arsenal grades, and simple validation.

summarize_pitch_grades <- function(scored_data, min_pitcher_pitch_n = 5) {
  load_stuff_packages()

  scored_data %>%
    group_by(.data$Pitcher, .data$PitcherTeam, .data$PitchType) %>%
    summarize(
      Pitches = dplyr::n(),
      AvgVelo = safe_mean(.data$RelSpeed),
      MaxVelo = safe_max(.data$RelSpeed),
      AvgSpin = safe_mean(.data$SpinRate),
      AvgIVB = safe_mean(.data$InducedVertBreak),
      AvgHB = safe_mean(.data$HorzBreak),
      AvgAbsHB = safe_mean(abs(.data$HorzBreak)),
      AvgExtension = safe_mean(.data$Extension),
      AvgVeloSep = safe_mean(.data$VeloSep),
      AvgIVBSep = safe_mean(.data$IVBSep),
      AvgHBSep = safe_mean(.data$HBSep),
      PitchStuffPlus = safe_mean(.data$StuffPlus),
      BestSinglePitchStuffPlus = safe_max(.data$StuffPlus),
      Strikes = sum(.data$Strike %in% TRUE, na.rm = TRUE),
      Swings = sum(.data$Swing %in% TRUE, na.rm = TRUE),
      Whiffs = sum(.data$Whiff %in% TRUE, na.rm = TRUE),
      CalledStrikes = sum(.data$CalledStrike %in% TRUE, na.rm = TRUE),
      CSW = sum(.data$CSW %in% TRUE, na.rm = TRUE),
      InZone = sum(.data$InZone %in% TRUE, na.rm = TRUE),
      Chases = sum(.data$Chase %in% TRUE, na.rm = TRUE),
      BattedBalls = sum(.data$BattedBall %in% TRUE, na.rm = TRUE),
      HardHitsAllowed = sum(.data$HardHitAllowed %in% TRUE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(.data$Pitcher) %>%
    mutate(
      UsagePct = .data$Pitches / sum(.data$Pitches, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      StrikePct = safe_divide(.data$Strikes, .data$Pitches),
      SwingPct = safe_divide(.data$Swings, .data$Pitches),
      WhiffPct = safe_divide(.data$Whiffs, .data$Swings),
      CSWPct = safe_divide(.data$CSW, .data$Pitches),
      ZonePct = safe_divide(.data$InZone, .data$Pitches),
      ChasePct = safe_divide(.data$Chases, .data$Pitches - .data$InZone),
      HardHitPctAllowed = safe_divide(.data$HardHitsAllowed, .data$BattedBalls),
      PitchStuffGrade = stuff_grade_label(.data$PitchStuffPlus),
      SampleFlag = dplyr::case_when(
        .data$Pitches < min_pitcher_pitch_n ~ "Small sample",
        TRUE ~ "OK"
      )
    ) %>%
    arrange(desc(.data$PitchStuffPlus))
}

add_trait_percentiles <- function(pitch_grades) {
  load_stuff_packages()

  pitch_grades %>%
    group_by(.data$PitchType) %>%
    mutate(
      VeloPct = safe_percentile(.data$AvgVelo),
      SpinPct = safe_percentile(.data$AvgSpin),
      IVBPct = safe_percentile(.data$AvgIVB),
      LowIVBPct = safe_percentile(-.data$AvgIVB),
      HBPct = safe_percentile(abs(.data$AvgHB)),
      ExtensionPct = safe_percentile(.data$AvgExtension),
      VeloSepPct = safe_percentile(.data$AvgVeloSep),
      IVBSepPct = safe_percentile(.data$AvgIVBSep),
      HBSepPct = safe_percentile(.data$AvgHBSep)
    ) %>%
    ungroup()
}

make_pitch_note <- function(PitchType, PitchStuffPlus, VeloPct, IVBPct, LowIVBPct,
                            HBPct, SpinPct, VeloSepPct, IVBSepPct, HBSepPct,
                            WhiffPct, CSWPct, UsagePct) {
  strengths <- c()
  needs <- c()

  add_strength <- function(label, pct) {
    if (!is.na(pct) && pct >= 75) paste0(label, " ", round(pct), "th pct") else NULL
  }

  add_need <- function(label, pct) {
    if (!is.na(pct) && pct <= 30) paste0(label, " ", round(pct), "th pct") else NULL
  }

  if (PitchType == "Four-Seam") {
    strengths <- c(strengths, add_strength("velo", VeloPct), add_strength("IVB", IVBPct), add_strength("spin", SpinPct))
    needs <- c(needs, add_need("velo", VeloPct), add_need("IVB", IVBPct))
  } else if (PitchType == "Sinker") {
    strengths <- c(strengths, add_strength("velo", VeloPct), add_strength("run", HBPct), add_strength("sink", LowIVBPct))
    needs <- c(needs, add_need("run", HBPct), add_need("sink", LowIVBPct))
  } else if (PitchType %in% c("Slider", "Curveball")) {
    strengths <- c(strengths, add_strength("movement", HBPct), add_strength("depth", LowIVBPct), add_strength("spin", SpinPct), add_strength("FB separation", HBSepPct))
    needs <- c(needs, add_need("movement", HBPct), add_need("depth", LowIVBPct), add_need("FB separation", HBSepPct))
  } else if (PitchType %in% c("Changeup", "Splitter")) {
    strengths <- c(strengths, add_strength("velo sep", VeloSepPct), add_strength("drop sep", IVBSepPct), add_strength("fade", HBPct))
    needs <- c(needs, add_need("velo sep", VeloSepPct), add_need("drop sep", IVBSepPct), add_need("fade", HBPct))
  }

  strengths <- strengths[!vapply(strengths, is.null, logical(1))]
  needs <- needs[!vapply(needs, is.null, logical(1))]

  outcome_note <- dplyr::case_when(
    !is.na(WhiffPct) & WhiffPct >= 0.30 ~ "Misses bats.",
    !is.na(CSWPct) & CSWPct >= 0.30 ~ "Gets CSW.",
    TRUE ~ NA_character_
  )

  if (!is.na(PitchStuffPlus) && PitchStuffPlus >= 110) {
    base <- paste0("Plus pitch. Strengths: ", ifelse(length(strengths) > 0, paste(strengths, collapse = ", "), "raw score"), ".")
  } else if (!is.na(PitchStuffPlus) && PitchStuffPlus < 95) {
    base <- paste0("Development focus. Needs: ", ifelse(length(needs) > 0, paste(needs, collapse = ", "), "clearer carrying trait"), ".")
  } else {
    base <- paste0("Average-ish pitch. ", ifelse(length(strengths) > 0, paste0("Best traits: ", paste(strengths, collapse = ", "), "."), "Look for usage or command fit."))
  }

  if (!is.na(outcome_note)) {
    base <- paste(base, outcome_note)
  }

  if (!is.na(UsagePct) && UsagePct < 0.10 && !is.na(PitchStuffPlus) && PitchStuffPlus >= 110) {
    base <- paste(base, "Consider whether usage should increase.")
  }

  base
}

add_development_notes <- function(pitch_grades) {
  load_stuff_packages()

  pitch_grades %>%
    rowwise() %>%
    mutate(
      DevelopmentNote = make_pitch_note(
        PitchType = .data$PitchType,
        PitchStuffPlus = .data$PitchStuffPlus,
        VeloPct = .data$VeloPct,
        IVBPct = .data$IVBPct,
        LowIVBPct = .data$LowIVBPct,
        HBPct = .data$HBPct,
        SpinPct = .data$SpinPct,
        VeloSepPct = .data$VeloSepPct,
        IVBSepPct = .data$IVBSepPct,
        HBSepPct = .data$HBSepPct,
        WhiffPct = .data$WhiffPct,
        CSWPct = .data$CSWPct,
        UsagePct = .data$UsagePct
      )
    ) %>%
    ungroup()
}

summarize_arsenal_grades <- function(pitch_grades, min_pitcher_pitch_n = 5) {
  load_stuff_packages()

  best_pitch <- pitch_grades %>%
    filter(.data$Pitches >= min_pitcher_pitch_n) %>%
    group_by(.data$Pitcher) %>%
    dplyr::slice_max(order_by = .data$PitchStuffPlus, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      Pitcher,
      BestPitch = .data$PitchType,
      BestPitchStuffPlus = .data$PitchStuffPlus,
      BestPitchUsagePct = .data$UsagePct,
      BestPitchGrade = .data$PitchStuffGrade
    )

  pitch_grades %>%
    group_by(.data$Pitcher, .data$PitcherTeam) %>%
    summarize(
      TotalPitches = sum(.data$Pitches, na.rm = TRUE),
      ArsenalStuffPlus = stats::weighted.mean(.data$PitchStuffPlus, w = .data$Pitches, na.rm = TRUE),
      PitchTypes = dplyr::n_distinct(.data$PitchType[.data$Pitches >= min_pitcher_pitch_n]),
      PitchesAbove105 = sum(.data$PitchStuffPlus >= 105 & .data$Pitches >= min_pitcher_pitch_n, na.rm = TRUE),
      PitchesAbove110 = sum(.data$PitchStuffPlus >= 110 & .data$Pitches >= min_pitcher_pitch_n, na.rm = TRUE),
      AvgCSWPct = stats::weighted.mean(.data$CSWPct, w = .data$Pitches, na.rm = TRUE),
      AvgWhiffPct = stats::weighted.mean(.data$WhiffPct, w = .data$Swings, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(best_pitch, by = "Pitcher") %>%
    mutate(
      ArsenalGrade = stuff_grade_label(.data$ArsenalStuffPlus),
      DevelopmentScore = .data$BestPitchStuffPlus +
        5 * .data$PitchesAbove105 +
        5 * .data$PitchesAbove110 +
        2 * pmax(.data$PitchTypes - 1, 0),
      DevelopmentTier = dplyr::case_when(
        .data$DevelopmentScore >= 135 ~ "Priority follow-up",
        .data$DevelopmentScore >= 120 ~ "Interesting raw traits",
        .data$DevelopmentScore >= 105 ~ "Solid foundation",
        TRUE ~ "Needs clearer carrying trait"
      )
    ) %>%
    arrange(desc(.data$DevelopmentScore), desc(.data$ArsenalStuffPlus))
}

summarize_validation <- function(pitch_grades) {
  load_stuff_packages()

  pitch_grades %>%
    summarize(
      n_pitcher_pitch_types = dplyr::n(),
      cor_stuff_csw = suppressWarnings(stats::cor(.data$PitchStuffPlus, .data$CSWPct, use = "complete.obs")),
      cor_stuff_whiff = suppressWarnings(stats::cor(.data$PitchStuffPlus, .data$WhiffPct, use = "complete.obs")),
      cor_stuff_hard_hit_allowed = suppressWarnings(stats::cor(.data$PitchStuffPlus, .data$HardHitPctAllowed, use = "complete.obs"))
    )
}
