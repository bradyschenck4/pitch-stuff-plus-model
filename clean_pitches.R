# Cleans TrackMan-style pitch data into a consistent format.

clean_pitch_type <- function(x) {
  x_chr <- stringr::str_to_lower(as.character(x))
  x_chr <- stringr::str_replace_all(x_chr, "[-_ ]", "")

  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x_chr %in% c("undefined", "unknown", "other", "na", "") ~ NA_character_,
    stringr::str_detect(x_chr, "fourseam|4seam|fourseamfastball|fastball") ~ "Four-Seam",
    stringr::str_detect(x_chr, "twoseam|2seam|sinker") ~ "Sinker",
    stringr::str_detect(x_chr, "cutter|cutfastball") ~ "Cutter",
    stringr::str_detect(x_chr, "slider|sweeper") ~ "Slider",
    stringr::str_detect(x_chr, "curve|curveball") ~ "Curveball",
    stringr::str_detect(x_chr, "change|changeup") ~ "Changeup",
    stringr::str_detect(x_chr, "split|splitter") ~ "Splitter",
    TRUE ~ stringr::str_to_title(as.character(x))
  )
}

standardize_pitch_data <- function(df,
                                   pitch_type_col = NULL,
                                   prefer_auto_pitch_type = TRUE) {
  load_stuff_packages()

  if (!is.data.frame(df)) {
    stop("df must be a data frame.", call. = FALSE)
  }

  pitch_col <- if (!is.null(pitch_type_col)) {
    pick_col(df, pitch_type_col)
  } else if (prefer_auto_pitch_type) {
    pick_col(df, c("AutoPitchType", "TaggedPitchType", "PitchType"))
  } else {
    pick_col(df, c("TaggedPitchType", "AutoPitchType", "PitchType"))
  }

  if (is.null(pitch_col)) {
    stop("Could not find AutoPitchType, TaggedPitchType, or PitchType.", call. = FALSE)
  }

  df <- df %>%
    add_missing_cols(c(
      "source_file", "Date", "Pitcher", "PitcherTeam", "PitcherThrows",
      "Batter", "BatterTeam", "PitchCall", "TaggedHitType",
      "RelSpeed", "SpinRate", "InducedVertBreak", "HorzBreak",
      "Extension", "RelHeight", "RelSide", "PlateLocSide", "PlateLocHeight",
      "VertApprAngle", "HorzApprAngle", "ZoneSpeed", "ExitSpeed", "Angle",
      "Distance", "Swing", "Whiff", "CalledStrike", "CSW", "Strike",
      "InZone", "Chase", "HardHitAllowed", "SweetSpotAllowed",
      "QualityContactAllowed"
    )) %>%
    mutate(
      PitchType = clean_pitch_type(.data[[pitch_col]]),
      RelSpeed = safe_num(.data$RelSpeed),
      SpinRate = safe_num(.data$SpinRate),
      InducedVertBreak = safe_num(.data$InducedVertBreak),
      HorzBreak = safe_num(.data$HorzBreak),
      Extension = safe_num(.data$Extension),
      RelHeight = safe_num(.data$RelHeight),
      RelSide = safe_num(.data$RelSide),
      PlateLocSide = safe_num(.data$PlateLocSide),
      PlateLocHeight = safe_num(.data$PlateLocHeight),
      VertApprAngle = safe_num(.data$VertApprAngle),
      HorzApprAngle = safe_num(.data$HorzApprAngle),
      ZoneSpeed = safe_num(.data$ZoneSpeed),
      ExitSpeed = safe_num(.data$ExitSpeed),
      Angle = safe_num(.data$Angle),
      Distance = safe_num(.data$Distance),
      Swing = to_logical_flag(.data$Swing),
      Whiff = to_logical_flag(.data$Whiff),
      CalledStrike = to_logical_flag(.data$CalledStrike),
      CSW = to_logical_flag(.data$CSW),
      Strike = to_logical_flag(.data$Strike),
      InZone = to_logical_flag(.data$InZone),
      Chase = to_logical_flag(.data$Chase),
      HardHitAllowed = to_logical_flag(.data$HardHitAllowed),
      SweetSpotAllowed = to_logical_flag(.data$SweetSpotAllowed),
      QualityContactAllowed = to_logical_flag(.data$QualityContactAllowed)
    )

  # Fill common pitch flags from PitchCall when they are not already present.
  df %>%
    mutate(
      pitch_call_clean = stringr::str_to_lower(as.character(.data$PitchCall)),
      Swing = dplyr::coalesce(
        .data$Swing,
        .data$pitch_call_clean %in% c("strikeswinging", "strike swinging", "foulball", "foul", "inplay", "in play")
      ),
      Whiff = dplyr::coalesce(
        .data$Whiff,
        .data$pitch_call_clean %in% c("strikeswinging", "strike swinging", "swingingstrike")
      ),
      CalledStrike = dplyr::coalesce(
        .data$CalledStrike,
        .data$pitch_call_clean %in% c("strikecalled", "strike called", "calledstrike")
      ),
      CSW = dplyr::coalesce(.data$CSW, .data$Whiff | .data$CalledStrike),
      Strike = dplyr::coalesce(
        .data$Strike,
        .data$pitch_call_clean %in% c(
          "strikecalled", "strike called", "calledstrike",
          "strikeswinging", "strike swinging", "swingingstrike",
          "foulball", "foul", "inplay", "in play"
        )
      ),
      BattedBall = !is.na(.data$ExitSpeed)
    ) %>%
    filter(
      !is.na(.data$Pitcher),
      !is.na(.data$PitchType),
      !is.na(.data$RelSpeed),
      !is.na(.data$InducedVertBreak),
      !is.na(.data$HorzBreak)
    )
}
