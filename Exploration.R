#
# D. Koster
#
#  Salary schedule
#
#
#
#

library(dplyr)
#install.packages("arrow")
library(arrow)

rm(list = ls())

filledPositions <- read.csv("filled_positions_info.csv")
full_p65 <- read_parquet("full_p65_with_nearest.parquet")
# Filling in where we can with ranges.
classOutline <- read.csv("classOutline.csv")
missingCount <- read.csv("missing_class_count.csv")

hist(as.numeric(full_p65$range))

# BU wasn't repeated in the salary study. 
posCount <- filledPositions %>%
  group_by(JCC) %>%
  summarize(nMatches = n()) %>%
  ungroup()

full_p65 <- full_p65 %>%
  left_join(posCount, by=c("code" = "JCC"))

full_p65 <- full_p65 %>%
  left_join(missingCount, by=c("code"))

full_p65 <- full_p65 %>%
  mutate(
    nMatches = if_else(is.na(nMatches), nMatchesSalStudy, nMatches)
  )

# Set nMatches = NA if no market salary is posted.
full_p65$nMatches[which(is.na(full_p65$market_target_annual_salary))] = NA

#  missingFromPosCount <- full_p65 %>%
#    filter(is.na(nMatches) & step == "A")

# Small data frame to try and get step into the 
rangeByClass <- classOutline %>%
  select(Class_Code, Range)

# Exclude positions not included in the class outline.
# 
filledPositions_withRange <- filledPositions %>%
  left_join(rangeByClass, by=c("JCC" = "Class_Code")) %>%
  filter(!is.na(Range))

# ########################################################
# Coverage - positions in study vs. occupied positions
# ########################################################

# Positions included in study.
countStudyPositions = nrow(filter(full_p65, 
                                  step == "A"))
  
occupiedByPosition <- filledPositions_withRange %>% 
  group_by(JCC) %>%
  summarize(count = n())

countOccupiedPositions = nrow(occupiedByPosition)

# ########################################################
# Weight for aggregation to job and groups.
# ########################################################

# Weight each job code according to the number of occupied positions.
full_p65 <- full_p65 %>%
  mutate(
    wgt_ak_annual_salary = ak_annual_salary * nMatches,
    wgt_mkt_target_annual_salary = market_target_annual_salary * nMatches
  )

p65_by_family = full_p65 %>%
  group_by(family, step) %>%
  summarize(
    nMatches = sum(nMatches, na.rm=TRUE),
    wgt_ak_annual_salary = sum(wgt_ak_annual_salary, na.rm=TRUE),
    wgt_mkt_target_annual_salary = sum(wgt_mkt_target_annual_salary, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    wgt_ak_annual_salary = wgt_ak_annual_salary / nMatches,
    wgt_mkt_target_annual_salary = wgt_mkt_target_annual_salary / nMatches
  )

p65_by_group = full_p65 %>%
  group_by(group, step) %>%
    summarize(
      nMatches = sum(nMatches, na.rm=TRUE),
      wgt_ak_annual_salary = sum(wgt_ak_annual_salary, na.rm=TRUE),
      wgt_mkt_target_annual_salary = sum(wgt_mkt_target_annual_salary, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      wgt_ak_annual_salary = wgt_ak_annual_salary / nMatches,
      wgt_mkt_target_annual_salary = wgt_mkt_target_annual_salary / nMatches
    )

write.csv(p65_by_family, "job_family_summary_65th_percentile.csv")
write.csv(p65_by_group, "job_group_summary_65th_percentile.csv")

# ########################################################
# Percentage of employees in positions where benchmarks
#  AK salaries at step less than -15%, -10%, -5%, 0% of benchmark
#   peercentage of employees greater than 0%, 5%, 10%, 15%
# ########################################################

full_p65$pctDiff <- (full_p65$ak_annual_salary - full_p65$market_target_annual_salary) / full_p65$ak_annual_salary
full_p65 <- full_p65 %>%
  mutate(
    ltm_15pct = if_else(pctDiff < -.15, nMatches, 0L),
    ltm_10pct = if_else(pctDiff < -.10, nMatches, 0L),
    ltm_5pct = if_else(pctDiff < -.05, nMatches, 0L),
    ltm_0pct = if_else(pctDiff < 0, nMatches, 0L),
    gtm_0pct = if_else(pctDiff >= 0, nMatches, 0L),
    gtm_5pct = if_else(pctDiff > .05, nMatches, 0L),
    gtm_10pct = if_else(pctDiff > .10, nMatches, 0L),    
    gtm_15pct = if_else(pctDiff > .15, nMatches, 0L)
  )

# Get summaries by family, group, then overall.
overUnderAnalysis.family <- full_p65 %>%
  group_by(step, group, family) %>%
  summarize(
    nMatches = sum(nMatches, na.rm=TRUE),
    ltm_15pct = sum(ltm_15pct, na.rm=TRUE),
    ltm_10pct = sum(ltm_10pct, na.rm=TRUE),
    ltm_5pct = sum(ltm_5pct, na.rm=TRUE),
    ltm_0pct = sum(ltm_0pct, na.rm=TRUE),
    gtm_0pct = sum(gtm_0pct, na.rm=TRUE),
    gtm_5pct = sum(gtm_5pct, na.rm=TRUE),
    gtm_10pct = sum(gtm_10pct, na.rm=TRUE),    
    gtm_15pct = sum(gtm_15pct, na.rm=TRUE)
  ) %>%
    ungroup() %>%
      mutate(
        ltm_15pct = ltm_15pct / nMatches,
        ltm_10pct = ltm_10pct / nMatches,
        ltm_5pct = ltm_5pct / nMatches,
        ltm_0pct = ltm_0pct / nMatches,
        gtm_0pct = gtm_0pct / nMatches,
        gtm_5pct = gtm_5pct / nMatches,
        gtm_10pct = gtm_10pct / nMatches,    
        gtm_15pct = gtm_15pct / nMatches
      )

  overUnderAnalysis.group <- full_p65 %>%
    group_by(step, group) %>%
    summarize(
      nMatches = sum(nMatches, na.rm=TRUE),
      ltm_15pct = sum(ltm_15pct, na.rm=TRUE),
      ltm_10pct = sum(ltm_10pct, na.rm=TRUE),
      ltm_5pct = sum(ltm_5pct, na.rm=TRUE),
      ltm_0pct = sum(ltm_0pct, na.rm=TRUE),
      gtm_0pct = sum(gtm_0pct, na.rm=TRUE),
      gtm_5pct = sum(gtm_5pct, na.rm=TRUE),
      gtm_10pct = sum(gtm_10pct, na.rm=TRUE),    
      gtm_15pct = sum(gtm_15pct, na.rm=TRUE)
    ) %>%
    ungroup() %>%
      mutate(
        ltm_15pct = ltm_15pct / nMatches,
        ltm_10pct = ltm_10pct / nMatches,
        ltm_5pct = ltm_5pct / nMatches,
        ltm_0pct = ltm_0pct / nMatches,
        gtm_0pct = gtm_0pct / nMatches,
        gtm_5pct = gtm_5pct / nMatches,
        gtm_10pct = gtm_10pct / nMatches,    
        gtm_15pct = gtm_15pct / nMatches
      )

    overUnderAnalysis.statewide <- full_p65 %>%
      group_by(step) %>%
      summarize(
        nMatches = sum(nMatches, na.rm=TRUE),
        ltm_15pct = sum(ltm_15pct, na.rm=TRUE),
        ltm_10pct = sum(ltm_10pct, na.rm=TRUE),
        ltm_5pct = sum(ltm_5pct, na.rm=TRUE),
        ltm_0pct = sum(ltm_0pct, na.rm=TRUE),
        gtm_0pct = sum(gtm_0pct, na.rm=TRUE),
        gtm_5pct = sum(gtm_5pct, na.rm=TRUE),
        gtm_10pct = sum(gtm_10pct, na.rm=TRUE),    
        gtm_15pct = sum(gtm_15pct, na.rm=TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        ltm_15pct = ltm_15pct / nMatches,
        ltm_10pct = ltm_10pct / nMatches,
        ltm_5pct = ltm_5pct / nMatches,
        ltm_0pct = ltm_0pct / nMatches,
        gtm_0pct = gtm_0pct / nMatches,
        gtm_5pct = gtm_5pct / nMatches,
        gtm_10pct = gtm_10pct / nMatches,    
        gtm_15pct = gtm_15pct / nMatches
      )


    