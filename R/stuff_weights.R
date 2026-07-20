# Pitch-specific Stuff+ weights.
# Every trait is z-scored within pitch type before the weights are applied.

default_stuff_weights <- function() {
  tibble::tribble(
    ~PitchType,   ~trait,        ~weight, ~description,
    "Four-Seam", "z_velo",       0.35,    "velocity",
    "Four-Seam", "z_ivb",        0.30,    "ride / induced vertical break",
    "Four-Seam", "z_spin",       0.15,    "spin rate",
    "Four-Seam", "z_ext",        0.10,    "extension",
    "Four-Seam", "z_abs_hb",     0.10,    "horizontal movement",

    "Sinker",    "z_velo",       0.30,    "velocity",
    "Sinker",    "z_abs_hb",     0.30,    "arm-side run",
    "Sinker",    "z_low_ivb",    0.25,    "sink",
    "Sinker",    "z_ext",        0.10,    "extension",
    "Sinker",    "z_spin",       0.05,    "spin rate",

    "Cutter",    "z_velo",       0.30,    "velocity",
    "Cutter",    "z_abs_hb",     0.25,    "cut",
    "Cutter",    "z_ivb",        0.15,    "carry",
    "Cutter",    "z_ext",        0.15,    "extension",
    "Cutter",    "z_spin",       0.15,    "spin rate",

    "Slider",    "z_velo",       0.25,    "velocity",
    "Slider",    "z_abs_hb",     0.30,    "sweep",
    "Slider",    "z_low_ivb",    0.20,    "depth",
    "Slider",    "z_spin",       0.10,    "spin rate",
    "Slider",    "z_hb_sep",     0.15,    "fastball shape separation",

    "Curveball", "z_low_ivb",    0.40,    "vertical depth",
    "Curveball", "z_spin",       0.20,    "spin rate",
    "Curveball", "z_abs_hb",     0.15,    "horizontal movement",
    "Curveball", "z_velo",       0.15,    "velocity",
    "Curveball", "z_hb_sep",     0.10,    "fastball shape separation",

    "Changeup",  "z_velo_sep",   0.30,    "velocity separation from fastball",
    "Changeup",  "z_abs_hb",     0.25,    "fade",
    "Changeup",  "z_ivb_sep",    0.25,    "vertical separation from fastball",
    "Changeup",  "z_low_spin",   0.10,    "lower spin / tumble proxy",
    "Changeup",  "z_ext",        0.10,    "extension",

    "Splitter",  "z_velo_sep",   0.30,    "velocity separation from fastball",
    "Splitter",  "z_low_ivb",    0.35,    "drop",
    "Splitter",  "z_abs_hb",     0.15,    "movement",
    "Splitter",  "z_low_spin",   0.10,    "lower spin / tumble proxy",
    "Splitter",  "z_ext",        0.10,    "extension"
  )
}
