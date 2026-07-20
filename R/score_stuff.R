# Creates Stuff+ traits and scores individual pitch rows.

add_stuff_traits <- function(pitch_data) {
  load_stuff_packages()

  pitch_data <- pitch_data %>%
    mutate(
      AbsHB = abs(.data$HorzBreak),
      LowIVB = -.data$InducedVertBreak,
      LowSpin = -.data$SpinRate
    )

  fb_baseline <- pitch_data %>%
    filter(.data$PitchType %in% c("Four-Seam", "Sinker")) %>%
    group_by(.data$Pitcher) %>%
    summarize(
      FB_RelSpeed = safe_mean(.data$RelSpeed),
      FB_IVB = safe_mean(.data$InducedVertBreak),
      FB_HB = safe_mean(.data$HorzBreak),
      .groups = "drop"
    )

  pitch_data %>%
    left_join(fb_baseline, by = "Pitcher") %>%
    mutate(
      VeloSep = .data$FB_RelSpeed - .data$RelSpeed,
      IVBSep = .data$FB_IVB - .data$InducedVertBreak,
      HBSep = abs(.data$HorzBreak - .data$FB_HB)
    ) %>%
    group_by(.data$PitchType) %>%
    mutate(
      z_velo = safe_z(.data$RelSpeed),
      z_ivb = safe_z(.data$InducedVertBreak),
      z_low_ivb = safe_z(.data$LowIVB),
      z_abs_hb = safe_z(.data$AbsHB),
      z_spin = safe_z(.data$SpinRate),
      z_low_spin = safe_z(.data$LowSpin),
      z_ext = safe_z(.data$Extension),
      z_velo_sep = safe_z(.data$VeloSep),
      z_ivb_sep = safe_z(.data$IVBSep),
      z_hb_sep = safe_z(.data$HBSep)
    ) %>%
    ungroup()
}

score_pitch_rows <- function(pitch_data,
                             weights = default_stuff_weights(),
                             stuff_scale = 10,
                             cap_range = c(50, 150)) {
  load_stuff_packages()

  z_cols <- c(
    "z_velo", "z_ivb", "z_low_ivb", "z_abs_hb", "z_spin",
    "z_low_spin", "z_ext", "z_velo_sep", "z_ivb_sep", "z_hb_sep"
  )

  pitch_data <- pitch_data %>%
    add_missing_cols(z_cols, value = NA_real_) %>%
    mutate(row_id = dplyr::row_number())

  raw_scores <- pitch_data %>%
    select(.data$row_id, .data$PitchType, dplyr::all_of(z_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(z_cols),
      names_to = "trait",
      values_to = "trait_value"
    ) %>%
    left_join(weights, by = c("PitchType", "trait")) %>%
    filter(!is.na(.data$weight), !is.na(.data$trait_value)) %>%
    group_by(.data$row_id) %>%
    summarize(
      raw_stuff_score = sum(.data$trait_value * .data$weight, na.rm = TRUE) /
        sum(abs(.data$weight), na.rm = TRUE),
      traits_used = paste(.data$description, collapse = "; "),
      .groups = "drop"
    )

  pitch_data %>%
    left_join(raw_scores, by = "row_id") %>%
    group_by(.data$PitchType) %>%
    mutate(
      raw_stuff_z = safe_z(.data$raw_stuff_score),
      StuffPlus = 100 + stuff_scale * .data$raw_stuff_z,
      StuffPlus = pmin(pmax(.data$StuffPlus, cap_range[[1]]), cap_range[[2]])
    ) %>%
    ungroup() %>%
    mutate(
      StuffGrade = stuff_grade_label(.data$StuffPlus)
    )
}
