kable_pitch_metrics_cond_frmt_hot_cold <- function(df) {
  
  get_background_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    colors <- hot_cold
    case_when(
      metric == 0 ~ 'white',
      metric <= quantiles[1] ~ colors[1],
      metric <= quantiles[2] ~ colors[2],
      metric <= quantiles[3] ~ colors[3],
      metric <= quantiles[4] ~ colors[4],
      metric <= quantiles[5] ~ colors[5],
      metric <= quantiles[6] ~ colors[6],
      metric >= quantiles[6] ~ colors[7],
      TRUE ~ '#FFFFFF'  # Fallback color for unexpected cases
    )
  }
  
  get_text_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    case_when(
      metric == 0 ~ 'black',
      metric <= quantiles[1] ~ 'white',
      metric <= quantiles[4] ~ 'black',
      metric >= quantiles[5] ~ 'white',
      TRUE ~ 'black'  # Fallback color for unexpected cases
    )
  }
  
  
  df <- df %>%
    column_spec(4, # Velo
                background = mapply(get_background_color, as.character(game_summary_table$Pitch), game_summary_table$`Velo`, MoreArgs = list(metric_type = 'Velo')),
                color = mapply(get_text_color, as.character(game_summary_table$Pitch), game_summary_table$`Velo`, MoreArgs = list(metric_type = 'Velo')))  %>%
    column_spec(15, # Ext
                background = mapply(get_background_color, as.character(game_summary_table$Pitch), game_summary_table$`Ext`, MoreArgs = list(metric_type = 'Ext')),
                color = mapply(get_text_color, as.character(game_summary_table$Pitch), game_summary_table$`Ext`, MoreArgs = list(metric_type = 'Ext')))
  
  
  df
}


kable_pitch_stats_cond_frmt_hi_low <- function(df) {
  
  get_background_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    colors <- higher_lower
    case_when(
      metric == 0 ~ 'white',
      metric <= quantiles[1] ~ colors[1],
      metric <= quantiles[2] ~ colors[2],
      metric <= quantiles[3] ~ colors[3],
      metric <= quantiles[4] ~ colors[4],
      metric <= quantiles[5] ~ colors[5],
      metric <= quantiles[6] ~ colors[6],
      metric >= quantiles[6] ~ colors[7],
      TRUE ~ '#ffffff'
    )
  }
  
  get_text_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    case_when(
      metric == 0 ~ 'black',
      metric <= quantiles[1] ~ 'white',
      metric <= quantiles[4] ~ 'black',
      metric >= quantiles[5] ~ 'white',
      TRUE ~ 'black'
    )
  }
  
  df <- df %>%
    column_spec(7, # Zone%
                background = mapply(get_background_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Zone%`, MoreArgs = list(metric_type = 'Zone%')),
                color = mapply(get_text_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Zone%`, MoreArgs = list(metric_type = 'Zone%'))) %>%
    column_spec(8, # Strk%
                background = mapply(get_background_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Strk%`, MoreArgs = list(metric_type = 'Strk%')),
                color = mapply(get_text_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Strk%`, MoreArgs = list(metric_type = 'Strk%'))) %>%
    column_spec(9, # Chase%
                background = mapply(get_background_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Chase%`, MoreArgs = list(metric_type = 'Chase%')),
                color = mapply(get_text_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Chase%`, MoreArgs = list(metric_type = 'Chase%')))
  
  df
}


kable_pitch_metrics_cond_frmt_hi_low <- function(df) {
  
  get_background_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    colors <- higher_lower
    case_when(
      metric == 0 ~ 'white',
      metric <= quantiles[1] ~ colors[1],
      metric <= quantiles[2] ~ colors[2],
      metric <= quantiles[3] ~ colors[3],
      metric <= quantiles[4] ~ colors[4],
      metric <= quantiles[5] ~ colors[5],
      metric <= quantiles[6] ~ colors[6],
      metric >= quantiles[6] ~ colors[7],
      TRUE ~ '#ffffff'
    )
  }
  
  get_text_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    case_when(
      metric == 0 ~ 'black',
      metric <= quantiles[1] ~ 'white',
      metric <= quantiles[4] ~ 'black',
      metric >= quantiles[5] ~ 'white',
      TRUE ~ 'black'
    )
  }
  
  df <- df %>%
    column_spec(6, # IVB
                background = mapply(get_background_color, as.character(game_summary_table$Pitch), game_summary_table$`IVB`, MoreArgs = list(metric_type = 'IVB')),
                color = mapply(get_text_color, as.character(game_summary_table$Pitch), game_summary_table$`IVB`, MoreArgs = list(metric_type = 'IVB')))%>%
    column_spec(9, # Spin
                background = mapply(get_background_color, as.character(game_summary_table$Pitch), game_summary_table$`Spin`, MoreArgs = list(metric_type = 'Spin')),
                color = mapply(get_text_color, as.character(game_summary_table$Pitch), game_summary_table$`Spin`, MoreArgs = list(metric_type = 'Spin')))
  
  df
}