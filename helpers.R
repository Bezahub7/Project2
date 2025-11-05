# ---- Helpers ----
numeric_vars <- c(
  "app_usage_time_min_day",
  "screen_on_time_hours_day",
  "battery_drain_m_ah_day",
  "number_of_apps_installed",
  "data_usage_mb_day",
  "age"
)

categorical_vars <- c(
  "gender",
  "operating_system",
  "user_behavior_class_label",
  "device_model",
  "age_group" 
)
# to nicely label the sliders
nice_var_label <- function(x) {
  x |>
    str_replace_all("_", " ") |>
    str_to_title()
}
