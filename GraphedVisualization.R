# Katelyn N Diaz -- Graphed Visualization Code Example

# Input DF: call start and end times with outcome, as well as post-download processing of date and time joined in a single col and
# converted to POSIXct format, and date converted to categorical day of the week using lubridate::wday

#' Function 1: Create QUARTILE plot dataframe
#' @param df A data frame containing the date and time columns post-RedCap download and clean
#' @export
#' @return A ggplot
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom lubridate hm
#' @importFrom lubridate hour
#' @importFrom ggpubr ggarrange
#' @importFrom ggpubr rremove
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @import ggplot2

quartileGraphNew <- function(df){
  timeDow <- df %>%
    filter(!(redcap_event_name == "baseline_arm_1"))

  # Create day1 column
  timeDow <- timeDow %>%
    mutate(day1 = case_when(
      dow1 == 1 ~ "SUN",
      dow1 == 2 ~ "MON",
      dow1 == 3 ~ "TUE",
      dow1 == 4 ~ "WED",
      dow1 == 5 ~ "THU",
      dow1 == 6 ~ "FRI",
      dow1 == 7 ~ "SAT",
      TRUE ~ NA_character_
    ))


  #convert fc_st into hour
  timeDow <- timeDow %>%
    mutate(fc_st = hm(fc_st))

  # Create time1 column based on hour extraction
  timeDow1 <- timeDow %>%
    mutate(hour_fc_st = hour(fc_st),
           time1 = case_when(
             hour_fc_st == 0 ~ "0",
             hour_fc_st == 1 ~ "1",
             hour_fc_st == 2 ~ "2",
             hour_fc_st == 3 ~ "3",
             hour_fc_st == 4 ~ "4",
             hour_fc_st == 5 ~ "5",
             hour_fc_st == 6 ~ "6",
             hour_fc_st == 7 ~ "7",
             hour_fc_st == 8 ~ "8",
             hour_fc_st == 9 ~ "9",
             hour_fc_st == 10 ~ "10",
             hour_fc_st == 11 ~ "11",
             hour_fc_st == 12 ~ "12",
             hour_fc_st == 13 ~ "13",
             hour_fc_st == 14 ~ "14",
             hour_fc_st == 15 ~ "15",
             hour_fc_st == 16 ~ "16",
             hour_fc_st == 17 ~ "17",
             hour_fc_st == 18 ~ "18",
             hour_fc_st == 19 ~ "19",
             hour_fc_st == 20 ~ "20",
             hour_fc_st == 21 ~ "21",
             hour_fc_st == 22 ~ "22",
             hour_fc_st == 23 ~ "23",
             TRUE ~ "other"
           ))

  # Create quartile column
  timeDow1 <- timeDow1 %>%
    mutate(timeQ = case_when(
      time1 %in% c("8", "9", "10") ~ "Q1",  # 8-10:59am
      time1 %in% c("11", "12", "13") ~ "Q2", # 11am - 1:59pm
      time1 %in% c("14", "15", "16") ~ "Q3", # 2-4:59pm
      time1 %in% c("17", "18", "19") ~ "Q4", # 5-7:59pm
      TRUE ~ "Q5"  # Default to Q5 for other times; catch-all bucket
    ))

  # Combine day and time into a single column
  timeDow1 <- timeDow1 %>%
    mutate(dayTime1 = paste(day1, time1),
           dayTimeQ = paste(day1, timeQ))

  # Filter out rows with NA in dow1 or fc_st
  timeDowFilter <- timeDow1 %>%
    filter(!is.na(dow1) & !is.na(fc_st))

  # Order dayTime1 as a factor with specific levels
  timeDowFIN <- timeDowFilter %>%
    mutate(dayTime1factored = factor(dayTime1, levels = c(
      paste(rep(c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"), each = 24), rep(0:23, 7), sep = " ")
    )))

  # Add descriptive text for fc
  timeDowFIN2 <- timeDowFIN %>%
    mutate(fcWord = case_when(
      fc == 1 ~ "Spoke to participant",
      fc == 2 ~ "Spoke to other",
      fc == 3 ~ "Left voicemail",
      fc == 4 ~ "No answer",
      fc == 5 ~ "Out of service",
      TRUE ~ NA_character_
    ))

  # Remove rows where fc_st is NA
  timeDowFIN3 <- timeDowFIN2 %>%
    filter(!is.na(fc_st))


  # Calculate counts and percentages for quartiles
  timeDowFIN5 <- timeDowFIN3 %>%
    group_by(dayTimeQ, fcWord, day1) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(perc = count / sum(count) * 100)

  timeDowFIN5

}



#' Function 2: Takes output DF and creates graphical visualization of quartile-separated day and time graph with 5 graphs, one per day of week
#' @param df A dataframe from the output of quartileGraphNew()
#' @param ycap Numerical, the top of the graph, auto-default 35
#' @export
#' @return A ggplot
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom ggpubr ggarrange
#' @importFrom ggpubr annotate_figure
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @import ggplot2


quartileDailyDayTimeGraph <- function(df, ycap=35){
  ### Monday
  monday <- df %>%
    filter(day1 == "MON")

  monPlot <- ggplot(monday, aes(x = dayTimeQ, y = count, fill = fcWord)) +
    geom_bar(stat = "identity", position = "stack") +
    ylim(0, ycap) + #add ylimit with the first num being the smallest and second being largest
    scale_fill_manual(values = c("Spoke to participant" = "royalblue", "Spoke to other" = "pink", "Left voicemail" = "orange", "No answer" = "lightgreen", "Out of service" = "lightblue")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme_minimal() +
    geom_text(aes(label = paste0(round(perc, 1), "%")),     # add percentages labels
              position = position_stack(vjust = 0.5),
              size = 3)

  ### Tuesday
  tuesday <- df %>%
    filter(day1 == "TUE")

  tuesPlot <- ggplot(tuesday, aes(x = dayTimeQ, y = count, fill = fcWord)) +
    geom_bar(stat = "identity", position = "stack") +
    ylim(0, ycap) +
    scale_fill_manual(values = c("Spoke to participant" = "royalblue", "Spoke to other" = "pink", "Left voicemail" = "orange", "No answer" = "lightgreen", "Out of service" = "lightblue")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme_minimal() +
    geom_text(aes(label = paste0(round(perc, 1), "%")),
              position = position_stack(vjust = 0.5),
              size = 3)

  ### Wednesday
  wednesday <- df %>%
    filter(day1 == "WED")

  wedPlot <- ggplot(wednesday, aes(x = dayTimeQ, y = count, fill = fcWord)) +
    geom_bar(stat = "identity", position = "stack") +
    ylim(0, ycap) +
    scale_fill_manual(values = c("Spoke to participant" = "royalblue", "Spoke to other" = "pink", "Left voicemail" = "orange", "No answer" = "lightgreen", "Out of service" = "lightblue")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme_minimal() +
    geom_text(aes(label = paste0(round(perc, 1), "%")),
              position = position_stack(vjust = 0.5),
              size = 3)

  ### Thursday
  thursday <- df %>%
    filter(day1 == "THU")

  thuPlot <- ggplot(thursday, aes(x = dayTimeQ, y = count, fill = fcWord)) +
    geom_bar(stat = "identity", position = "stack") +
    ylim(0, ycap) +
    scale_fill_manual(values = c("Spoke to participant" = "royalblue", "Spoke to other" = "pink", "Left voicemail" = "orange", "No answer" = "lightgreen", "Out of service" = "lightblue")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme_minimal() +
    geom_text(aes(label = paste0(round(perc, 1), "%")),
              position = position_stack(vjust = 0.5),
              size = 3)

  ### Friday
  friday <- df %>%
    filter(day1 == "FRI")

  friPlot <- ggplot(friday, aes(x = dayTimeQ, y = count, fill = fcWord)) +
    geom_bar(stat = "identity", position = "stack") +
    ylim(0, ycap) +
    scale_fill_manual(values = c("Spoke to participant" = "royalblue", "Spoke to other" = "pink", "Left voicemail" = "orange", "No answer" = "lightgreen", "Out of service" = "lightblue")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme_minimal() +
    geom_text(aes(label = paste0(round(perc, 1), "%")),
              position = position_stack(vjust = 0.5),
              size = 3)

  #consolidate the 5 days worth of plots into one
  figureTry <- ggarrange(
    monPlot + rremove("ylab") + rremove("xlab"),
    tuesPlot + rremove("ylab") + rremove("xlab"),
    wedPlot + rremove("ylab") + rremove("xlab"),
    thuPlot + rremove("ylab") + rremove("xlab"),
    friPlot + rremove("ylab") + rremove("xlab"),
    legend = FALSE, common.legend = TRUE)     # TO REMOVE LEGEND (don't need the same legend to appear five times)

  annotate_figure(figureTry, left = textGrob("Call Outcome", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                  bottom = textGrob("Call Day and Quartile Time (8am - 7:59pm)", gp = gpar(cex = 1.3))) #from grid library

}
