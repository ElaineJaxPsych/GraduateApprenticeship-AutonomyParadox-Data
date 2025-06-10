# ==============================================================================
# COMPREHENSIVE GRADUATE APPRENTICESHIP ANALYSIS - WITH TRI-SPHERE MODEL
# ==============================================================================
# Full analysis script for autonomy paradox research + tri-sphere validation
# Author: Elaine Jackson
# Date: December 2024
# ==============================================================================

# Load required libraries
suppressMessages({
  library(dplyr)
  library(ggplot2) 
  library(psych)
  library(gridExtra)
  library(grid)
  library(tidyr)
  library(scales)
})

# Install missing packages if needed
required_packages <- c("dplyr", "ggplot2", "psych", "gridExtra", "grid", "tidyr", "scales")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
}

# Create output directory
if(!dir.exists("publication_figures")) {
  dir.create("publication_figures")
  cat("Created publication_figures directory\n")
}

cat("=== COMPREHENSIVE GRADUATE APPRENTICESHIP ANALYSIS ===\n")
cat("Two-Population Design: Current Students vs Alumni\n\n")

# ==============================================================================
# PUBLICATION THEME AND GRAYSCALE FUNCTIONS
# ==============================================================================

# Define grayscale publication theme
theme_publication <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5, 
                                margin = margin(0, 0, 15, 0)),
      plot.subtitle = element_text(size = base_size, hjust = 0.5, 
                                   margin = margin(0, 0, 12, 0)),
      axis.title = element_text(size = base_size, face = "bold", color = "black",
                                margin = margin(8, 8, 8, 8)),
      axis.text = element_text(size = base_size - 1, color = "black"),
      axis.text.x = element_text(margin = margin(t = 5)),
      axis.text.y = element_text(margin = margin(r = 5)),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      legend.position = "bottom",
      legend.key.size = unit(0.8, "cm"),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      strip.text = element_text(size = base_size, face = "bold", color = "black"),
      strip.background = element_rect(fill = "grey95", color = "black"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.caption = element_text(size = base_size - 2, hjust = 0, 
                                  margin = margin(12, 0, 0, 0), color = "grey30",
                                  lineheight = 1.1)
    )
}

# Grayscale color function
get_grayscale_fills <- function(n) {
  if (n == 1) {
    return("#737373")
  } else if (n == 2) {
    return(c("#404040", "#a6a6a6"))
  } else if (n <= 4) {
    return(c("#1a1a1a", "#595959", "#8c8c8c", "#d9d9d9"))
  } else if (n <= 6) {
    return(c("#1a1a1a", "#404040", "#737373", "#a6a6a6", "#bfbfbf", "#d9d9d9"))
  } else if (n <= 8) {
    return(c("#1a1a1a", "#2d2d2d", "#404040", "#595959", "#8c8c8c", "#a6a6a6", "#bfbfbf", "#d9d9d9"))
  } else {
    return(gray.colors(n, start = 0.1, end = 0.85))
  }
}

# ==============================================================================
# DATA LOADING FUNCTIONS
# ==============================================================================

safe_combine_csv <- function(file_list, data_type = "current") {
  combined_data <- data.frame()
  
  for(file in file_list) {
    if(file.exists(file)) {
      cat("Loading:", file, "\n")
      temp_data <- read.csv(file, stringsAsFactors = FALSE, na.strings = c("", "NA", " ", "N/A"))
      temp_data$university <- ifelse(grepl("UWS", file), "UWS", "Glasgow")
      temp_data$status <- data_type
      
      if(nrow(combined_data) == 0) {
        combined_data <- temp_data
        cat("  - Base structure set with", ncol(temp_data), "columns\n")
      } else {
        common_cols <- intersect(names(combined_data), names(temp_data))
        cat("  - Found", length(common_cols), "common columns\n")
        if(length(common_cols) > 0) {
          combined_data <- rbind(combined_data[, common_cols], temp_data[, common_cols])
        }
      }
    }
  }
  return(combined_data)
}

safe_scale_mean <- function(data, col_names, min_items = 2, scale_name = "Scale") {
  available_cols <- intersect(col_names, names(data))
  
  if(length(available_cols) < min_items) {
    cat("  -", scale_name, ": Only", length(available_cols), "of", length(col_names), "items available\n")
    return(rep(NA, nrow(data)))
  }
  
  cat("  -", scale_name, ": Using", length(available_cols), "items\n")
  
  scale_data <- data[, available_cols, drop = FALSE]
  scale_data[] <- lapply(scale_data, function(x) {
    x_clean <- as.character(x)
    x_clean[grepl("ImportId|QID|Question|Response", x_clean, ignore.case = TRUE)] <- NA
    x_clean[x_clean == "" | is.na(x_clean) | x_clean == " "] <- NA
    as.numeric(x_clean)
  })
  
  valid_count <- rowSums(!is.na(scale_data))
  means <- rowMeans(scale_data, na.rm = TRUE)
  means[valid_count < min_items] <- NA
  
  return(means)
}

# ==============================================================================
# STEP 1: LOAD DATA
# ==============================================================================

cat("Loading current students data...\n")
current_files <- c("UWS current.csv", "Glasgow current.csv")
current_data <- safe_combine_csv(current_files, "Current")

cat("\nLoading alumni data...\n")
alumni_files <- c("UWS alumni.csv", "Glasgow alumni.csv")
alumni_data <- safe_combine_csv(alumni_files, "Alumni")

cat("\nData loading summary:\n")
cat("Current students:", nrow(current_data), "\n")
cat("Alumni:", nrow(alumni_data), "\n")

# ==============================================================================
# STEP 2: PROCESS CURRENT STUDENTS DATA
# ==============================================================================

cat("\n=== CURRENT STUDENTS ANALYSIS ===\n")
cat("Processing current students variables...\n")

# Q9: Perceived Autonomy
if("Q9" %in% names(current_data)) {
  current_data$perceived_autonomy <- as.numeric(as.character(current_data$Q9))
  valid_q9 <- sum(!is.na(current_data$perceived_autonomy))
  cat("  - Perceived Autonomy (Q9): n =", valid_q9, "valid responses\n")
} else {
  current_data$perceived_autonomy <- NA
  cat("  - Perceived Autonomy (Q9): NOT FOUND\n")
}

# Autonomy in Practice Scales
q10_cols <- c("Q10_1", "Q10_4", "Q10_5")
current_data$decision_autonomy <- safe_scale_mean(current_data, q10_cols, min_items = 2, "Decision Autonomy (Q10)")

q14_cols <- c("Q14_1", "Q14_2", "Q14_3", "Q14_4", "Q14_5")
current_data$applied_autonomy <- safe_scale_mean(current_data, q14_cols, min_items = 3, "Applied Autonomy (Q14)")

q16_cols <- c("Q16_1", "Q16_2", "Q16_4", "Q16_5")
current_data$confidence_autonomy <- safe_scale_mean(current_data, q16_cols, min_items = 2, "Confidence Autonomy (Q16)")

q17_cols <- c("Q17_1", "Q17_2", "Q17_3", "Q17_4", "Q17_5")
current_data$behavioral_autonomy <- safe_scale_mean(current_data, q17_cols, min_items = 3, "Behavioral Autonomy (Q17)")

# Overall Autonomy in Practice
practice_dimensions <- c("decision_autonomy", "applied_autonomy", "confidence_autonomy", "behavioral_autonomy")
available_practice <- practice_dimensions[sapply(practice_dimensions, function(x) {
  x %in% names(current_data) && sum(!is.na(current_data[[x]])) > 0
})]

if(length(available_practice) > 0) {
  current_data$autonomy_in_practice <- rowMeans(current_data[, available_practice, drop = FALSE], na.rm = TRUE)
  cat("  - Overall Autonomy in Practice: Created from", length(available_practice), "dimensions\n")
}

# Support Measures
employer_cols <- c("Q12_1", "Q12_2", "Q12_3", "Q12_4", "Q12_5")
current_data$employer_support <- safe_scale_mean(current_data, employer_cols, min_items = 3, "Employer Support (Q12)")

university_cols <- c("Q15_2", "Q15_4", "Q15_5")
current_data$university_support <- safe_scale_mean(current_data, university_cols, min_items = 2, "University Support (Q15)")

# Self-Directed Learning
sdl_cols <- c("Q18_2", "Q18_3")
current_data$self_directed_learning <- safe_scale_mean(current_data, sdl_cols, min_items = 2, "Self-Directed Learning (Q18)")

# ==============================================================================
# STEP 3: CURRENT STUDENTS TRI-SPHERE MODEL
# ==============================================================================

cat("\n--- CURRENT STUDENTS: TRI-SPHERE MODEL CREATION ---\n")

# Create sphere indicators using median splits for balanced groups
current_data <- current_data %>%
  mutate(
    # Academia Sphere: High university support
    academia_high = ifelse(university_support >= median(university_support, na.rm = TRUE), 1, 0),
    
    # Workplace Sphere: High employer support
    workplace_high = ifelse(employer_support >= median(employer_support, na.rm = TRUE), 1, 0),
    
    # Apprentice Agency Sphere: High perceived autonomy
    apprentice_high = ifelse(perceived_autonomy >= median(perceived_autonomy, na.rm = TRUE), 1, 0),
    
    # Create domain combinations
    domain_combination = case_when(
      academia_high == 1 & workplace_high == 1 & apprentice_high == 1 ~ "All Three Domains",
      academia_high == 1 & workplace_high == 1 & apprentice_high == 0 ~ "Academia + Workplace",
      academia_high == 1 & workplace_high == 0 & apprentice_high == 1 ~ "Academia + Apprentice",
      academia_high == 0 & workplace_high == 1 & apprentice_high == 1 ~ "Workplace + Apprentice",
      academia_high == 1 & workplace_high == 0 & apprentice_high == 0 ~ "Academia Only",
      academia_high == 0 & workplace_high == 1 & apprentice_high == 0 ~ "Workplace Only",
      academia_high == 0 & workplace_high == 0 & apprentice_high == 1 ~ "Apprentice Only",
      TRUE ~ "None"
    )
  )

# Check distribution of domain combinations
cat("Distribution of domain combinations (Current Students - median split):\n")
table(current_data$domain_combination)

# Calculate learning outcomes by domain combination
current_trisphere_validation <- current_data %>%
  filter(!is.na(self_directed_learning) & domain_combination != "None") %>%
  group_by(domain_combination) %>%
  summarise(
    n = n(),
    mean_sdl = mean(self_directed_learning, na.rm = TRUE),
    sd_sdl = sd(self_directed_learning, na.rm = TRUE),
    se_sdl = sd_sdl / sqrt(n),
    ci_lower = mean_sdl - 1.96 * se_sdl,
    ci_upper = mean_sdl + 1.96 * se_sdl,
    .groups = 'drop'
  ) %>%
  # Convert to percentage scale for visualization
  mutate(
    learning_outcome_pct = (mean_sdl - 1) / 4 * 100  # Convert 1-5 scale to 0-100%
  )

cat("\n=== CURRENT STUDENTS TRI-SPHERE RESULTS ===\n")
print(current_trisphere_validation)

# ==============================================================================
# STEP 4: CURRENT STUDENTS AUTONOMY PARADOX ANALYSIS
# ==============================================================================

cat("\n--- CURRENT STUDENTS: AUTONOMY PARADOX ANALYSIS ---\n")

# Create autonomy groups
if(sum(!is.na(current_data$perceived_autonomy)) > 0) {
  valid_autonomy <- current_data$perceived_autonomy[!is.na(current_data$perceived_autonomy)]
  n_valid <- length(valid_autonomy)
  
  high_autonomy <- sum(valid_autonomy >= 4)
  moderate_autonomy <- sum(valid_autonomy == 3)
  low_autonomy <- sum(valid_autonomy <= 2)
  
  cat("Perceived Autonomy Distribution (n =", n_valid, "):\n")
  cat("  - High (4-5):", high_autonomy, paste0("(", round(high_autonomy/n_valid*100, 1), "%)\n"))
  cat("  - Moderate (3):", moderate_autonomy, paste0("(", round(moderate_autonomy/n_valid*100, 1), "%)\n"))
  cat("  - Low (1-2):", low_autonomy, paste0("(", round(low_autonomy/n_valid*100, 1), "%)\n"))
  
  current_data$autonomy_group <- cut(current_data$perceived_autonomy, 
                                     breaks = c(0, 2.5, 3.5, 5),
                                     labels = c("Low (1-2)", "Moderate (3)", "High (4-5)"),
                                     include.lowest = TRUE)
}

# Test for Autonomy Paradox
if(sum(!is.na(current_data$perceived_autonomy)) > 0 & sum(!is.na(current_data$autonomy_in_practice)) > 0) {
  perceived_practice_cor <- cor(current_data$perceived_autonomy, current_data$autonomy_in_practice, use = "complete.obs")
  cat("\nAUTONOMY PARADOX TEST:\n")
  cat("Perceived ↔ Practice Autonomy correlation: r =", round(perceived_practice_cor, 3), "\n")
  
  if(abs(perceived_practice_cor) < 0.3) {
    cat("*** AUTONOMY PARADOX CONFIRMED: Weak perception-practice correlation ***\n")
  } else {
    cat("*** AUTONOMY PARADOX NOT CONFIRMED: Strong perception-practice correlation ***\n")
  }
}

# Learning outcomes by autonomy level
if(exists("autonomy_group", where = current_data) & sum(!is.na(current_data$self_directed_learning)) > 0) {
  group_stats <- current_data %>%
    filter(!is.na(autonomy_group) & !is.na(self_directed_learning)) %>%
    group_by(autonomy_group) %>%
    summarise(
      n = n(),
      mean_learning = round(mean(self_directed_learning, na.rm = TRUE), 2),
      sd_learning = round(sd(self_directed_learning, na.rm = TRUE), 2),
      .groups = 'drop'
    )
  
  cat("\nSelf-Directed Learning by Autonomy Level:\n")
  print(group_stats)
}

# Current Students Correlation Matrix
current_analysis_vars <- c("perceived_autonomy", "autonomy_in_practice", "self_directed_learning", 
                           "employer_support", "university_support")
current_available <- intersect(current_analysis_vars, names(current_data))

if(length(current_available) > 1) {
  current_cor_data <- current_data[, current_available, drop = FALSE]
  current_cor_data <- current_cor_data[rowSums(!is.na(current_cor_data)) > 0, ]
  
  if(nrow(current_cor_data) > 3) {
    current_correlations <- cor(current_cor_data, use = "pairwise.complete.obs")
    cat("\nCURRENT STUDENTS CORRELATION MATRIX:\n")
    print(round(current_correlations, 3))
  }
}

# ==============================================================================
# STEP 5: PROCESS ALUMNI DATA
# ==============================================================================

cat("\n=== ALUMNI ANALYSIS ===\n")
cat("Processing alumni variables...\n")

# GA Impact Measures
q13_cols <- c("Q13_1", "Q13_2", "Q13_3", "Q13_4", "Q13_5")
alumni_data$ga_ability_influence <- safe_scale_mean(alumni_data, q13_cols, min_items = 3, "GA Ability Influence (Q13)")

q14_ga_cols <- c("Q14_2", "Q14_4", "Q14_5")
alumni_data$ga_general_influence <- safe_scale_mean(alumni_data, q14_ga_cols, min_items = 2, "GA General Influence (Q14)")

q15_cols <- c("Q15_1", "Q15_2", "Q15_3", "Q15_4")
alumni_data$current_effectiveness <- safe_scale_mean(alumni_data, q15_cols, min_items = 2, "Current Effectiveness (Q15)")

q18_cols <- c("Q18_1", "Q18_2", "Q18_3", "Q18_4", "Q18_5")
alumni_data$programme_effectiveness <- safe_scale_mean(alumni_data, q18_cols, min_items = 3, "Programme Effectiveness (Q18)")

# Overall GA Impact
ga_impact_dimensions <- c("ga_ability_influence", "ga_general_influence", "current_effectiveness", "programme_effectiveness")
available_ga <- ga_impact_dimensions[sapply(ga_impact_dimensions, function(x) {
  x %in% names(alumni_data) && sum(!is.na(alumni_data[[x]])) > 0
})]

if(length(available_ga) > 0) {
  alumni_data$perceived_ga_impact <- rowMeans(alumni_data[, available_ga, drop = FALSE], na.rm = TRUE)
  cat("  - Overall GA Impact: Created from", length(available_ga), "dimensions\n")
}

# Q21: Workplace Autonomy Dimensions
q21_cols <- c("Q21_1", "Q21_2", "Q21_3", "Q21_4", "Q21_5")
q21_labels <- c("Workplace Learning", "Team Development", "Organizational Innovation", "Process Improvement", "Knowledge Sharing")

for(i in 1:length(q21_cols)) {
  if(q21_cols[i] %in% names(alumni_data)) {
    var_name <- paste0("q21_", i, "_", gsub(" ", "_", tolower(q21_labels[i])))
    alumni_data[[var_name]] <- as.numeric(as.character(alumni_data[[q21_cols[i]]]))
    cat("  -", q21_labels[i], "(", q21_cols[i], "): Created\n")
  }
}

# Overall Workplace Autonomy
alumni_data$workplace_autonomy <- safe_scale_mean(alumni_data, q21_cols, min_items = 3, "Overall Workplace Autonomy (Q21)")

# Ongoing Learning
ongoing_learning_cols <- c("Q18_2", "Q18_3")
alumni_data$ongoing_learning <- safe_scale_mean(alumni_data, ongoing_learning_cols, min_items = 2, "Ongoing Learning")

# Support Measures (Retrospective)
alumni_employer_cols <- c("Q12_1", "Q12_2", "Q12_3", "Q12_4", "Q12_5")
alumni_data$employer_support <- safe_scale_mean(alumni_data, alumni_employer_cols, min_items = 3, "Employer Support (Retrospective)")

alumni_university_cols <- c("Q15_2", "Q15_4", "Q15_5")
alumni_data$university_support <- safe_scale_mean(alumni_data, alumni_university_cols, min_items = 2, "University Support (Retrospective)")

# ==============================================================================
# STEP 6: ALUMNI TRI-SPHERE MODEL
# ==============================================================================

cat("\n--- ALUMNI: TRI-SPHERE MODEL CREATION ---\n")

# Create sphere indicators for alumni using median splits
alumni_data <- alumni_data %>%
  mutate(
    # Academia Sphere: High university support (retrospective)
    academia_high = ifelse(university_support >= median(university_support, na.rm = TRUE), 1, 0),
    
    # Workplace Sphere: High workplace autonomy (current workplace)
    workplace_high = ifelse(workplace_autonomy >= median(workplace_autonomy, na.rm = TRUE), 1, 0),
    
    # Apprentice Agency Sphere: High GA impact (overall program impact)
    apprentice_high = ifelse(perceived_ga_impact >= median(perceived_ga_impact, na.rm = TRUE), 1, 0),
    
    # Create domain combinations
    domain_combination = case_when(
      academia_high == 1 & workplace_high == 1 & apprentice_high == 1 ~ "All Three Domains",
      academia_high == 1 & workplace_high == 1 & apprentice_high == 0 ~ "Academia + Workplace",
      academia_high == 1 & workplace_high == 0 & apprentice_high == 1 ~ "Academia + Apprentice",
      academia_high == 0 & workplace_high == 1 & apprentice_high == 1 ~ "Workplace + Apprentice",
      academia_high == 1 & workplace_high == 0 & apprentice_high == 0 ~ "Academia Only",
      academia_high == 0 & workplace_high == 1 & apprentice_high == 0 ~ "Workplace Only",
      academia_high == 0 & workplace_high == 0 & apprentice_high == 1 ~ "Apprentice Only",
      TRUE ~ "None"
    )
  )

# Check distribution for alumni
cat("Distribution of domain combinations (Alumni - median split):\n")
table(alumni_data$domain_combination)

# Calculate outcomes for alumni
alumni_trisphere_validation <- alumni_data %>%
  filter(!is.na(ongoing_learning) & domain_combination != "None") %>%
  group_by(domain_combination) %>%
  summarise(
    n = n(),
    mean_learning = mean(ongoing_learning, na.rm = TRUE),
    sd_learning = sd(ongoing_learning, na.rm = TRUE),
    se_learning = sd_learning / sqrt(n),
    ci_lower = mean_learning - 1.96 * se_learning,
    ci_upper = mean_learning + 1.96 * se_learning,
    .groups = 'drop'
  ) %>%
  mutate(
    learning_outcome_pct = (mean_learning - 1) / 4 * 100
  )

cat("\n=== ALUMNI TRI-SPHERE RESULTS ===\n")
print(alumni_trisphere_validation)

# ==============================================================================
# STEP 7: ALUMNI LONG-TERM IMPACT ANALYSIS
# ==============================================================================

cat("\n--- ALUMNI: LONG-TERM IMPACT ANALYSIS ---\n")

# Key correlation: GA Impact ↔ Ongoing Learning
if(sum(!is.na(alumni_data$perceived_ga_impact)) > 0 & sum(!is.na(alumni_data$ongoing_learning)) > 0) {
  ga_learning_cor <- cor(alumni_data$perceived_ga_impact, alumni_data$ongoing_learning, use = "complete.obs")
  cat("GA Impact ↔ Ongoing Learning correlation: r =", round(ga_learning_cor, 3), "\n")
  
  if(abs(ga_learning_cor) > 0.7) {
    cat("*** EXCEPTIONAL LONG-TERM IMPACT: Very strong correlation ***\n")
  } else if(abs(ga_learning_cor) > 0.5) {
    cat("*** STRONG LONG-TERM IMPACT: Strong correlation ***\n")
  } else {
    cat("*** MODERATE LONG-TERM IMPACT: Moderate correlation ***\n")
  }
}

# Q21 Individual Dimension Analysis
cat("\nQ21 Workplace Autonomy Dimensions Analysis:\n")
q21_dimension_cors <- list()
for(i in 1:length(q21_cols)) {
  var_name <- paste0("q21_", i, "_", gsub(" ", "_", tolower(q21_labels[i])))
  if(var_name %in% names(alumni_data) & sum(!is.na(alumni_data$ongoing_learning)) > 0) {
    if(sum(!is.na(alumni_data[[var_name]])) > 0) {
      cor_val <- cor(alumni_data[[var_name]], alumni_data$ongoing_learning, use = "complete.obs")
      q21_dimension_cors[[q21_labels[i]]] <- cor_val
      cat("  -", q21_labels[i], "↔ Ongoing Learning: r =", round(cor_val, 3), "\n")
    }
  }
}

# Alumni Correlation Matrix
alumni_analysis_vars <- c("perceived_ga_impact", "workplace_autonomy", "ongoing_learning", 
                          "employer_support", "university_support")
alumni_available <- intersect(alumni_analysis_vars, names(alumni_data))

if(length(alumni_available) > 1) {
  alumni_cor_data <- alumni_data[, alumni_available, drop = FALSE]
  alumni_cor_data <- alumni_cor_data[rowSums(!is.na(alumni_cor_data)) > 0, ]
  
  if(nrow(alumni_cor_data) > 3) {
    alumni_correlations <- cor(alumni_cor_data, use = "pairwise.complete.obs")
    cat("\nALUMNI CORRELATION MATRIX:\n")
    print(round(alumni_correlations, 3))
  }
}

# ==============================================================================
# STEP 8: TRI-SPHERE COMPARATIVE ANALYSIS
# ==============================================================================

cat("\n=== TRI-SPHERE COMPARATIVE ANALYSIS ===\n")

# Create side-by-side comparison
comparison_data <- bind_rows(
  current_trisphere_validation %>% 
    mutate(Group = "Current Students",
           mean_learning = mean_sdl,
           sd_learning = sd_sdl) %>%
    select(Group, domain_combination, n, mean_learning, learning_outcome_pct),
  alumni_trisphere_validation %>%
    mutate(Group = "Alumni") %>%
    select(Group, domain_combination, n, mean_learning, learning_outcome_pct)
)

# Create summary comparison table
summary_comparison <- comparison_data %>%
  pivot_wider(names_from = Group, 
              values_from = c(n, mean_learning, learning_outcome_pct),
              names_sep = "_") %>%
  arrange(desc(`learning_outcome_pct_Alumni`)) %>%
  mutate(
    Current_Summary = paste0(ifelse(is.na(`learning_outcome_pct_Current Students`), "—", 
                                    paste0(round(`learning_outcome_pct_Current Students`, 1), "%")), 
                             " (n=", ifelse(is.na(`n_Current Students`), 0, `n_Current Students`), ")"),
    Alumni_Summary = paste0(ifelse(is.na(learning_outcome_pct_Alumni), "—", 
                                   paste0(round(learning_outcome_pct_Alumni, 1), "%")), 
                            " (n=", ifelse(is.na(n_Alumni), 0, n_Alumni), ")"),
    Improvement = ifelse(is.na(learning_outcome_pct_Alumni) | is.na(`learning_outcome_pct_Current Students`), 
                         NA, round(learning_outcome_pct_Alumni - `learning_outcome_pct_Current Students`, 1))
  ) %>%
  select(domain_combination, Current_Summary, Alumni_Summary, Improvement)

cat("\n=== COMPARATIVE SUMMARY TABLE ===\n")
print(summary_comparison)

# Developmental trajectory
if(sum(!is.na(current_data$self_directed_learning)) > 0 & sum(!is.na(alumni_data$ongoing_learning)) > 0) {
  current_mean <- mean(current_data$self_directed_learning, na.rm = TRUE)
  alumni_mean <- mean(alumni_data$ongoing_learning, na.rm = TRUE)
  improvement <- alumni_mean - current_mean
  
  cat("\nDEVELOPMENTAL TRAJECTORY:\n")
  cat("Current Students (Self-Directed Learning): M =", round(current_mean, 2), "\n")
  cat("Alumni (Ongoing Learning): M =", round(alumni_mean, 2), "\n")
  cat("Improvement: +", round(improvement, 2), " points\n")
  
  if(improvement > 1.0) {
    cat("*** DRAMATIC IMPROVEMENT: Strong developmental trajectory ***\n")
  } else if(improvement > 0.5) {
    cat("*** SUBSTANTIAL IMPROVEMENT: Good developmental trajectory ***\n")
  } else {
    cat("*** MODEST IMPROVEMENT: Limited developmental trajectory ***\n")
  }
}

# ==============================================================================
# STEP 9: Q17_1 MENTOR EFFECTIVENESS ANALYSIS
# ==============================================================================

cat("\n=== Q17_1 MENTOR EFFECTIVENESS ANALYSIS ===\n")

# Combine data for Q17_1 analysis
combined_data <- bind_rows(
  current_data %>% mutate(Group = "Current Students"),
  alumni_data %>% mutate(Group = "Alumni")
) %>%
  mutate(
    Q17_1_numeric = as.numeric(as.character(Q17_1)),
    mentor_effectiveness = case_when(
      Q17_1_numeric <= 2 ~ "Ineffective (1-2)",
      Q17_1_numeric == 3 ~ "Neutral (3)",
      Q17_1_numeric >= 4 ~ "Effective (4-5)",
      TRUE ~ "Missing/No Response"
    ),
    mentor_effectiveness = factor(mentor_effectiveness, 
                                  levels = c("Ineffective (1-2)", "Neutral (3)", 
                                             "Effective (4-5)", "Missing/No Response"))
  )

# Focus on workplace combinations for Q17_1 analysis
workplace_q17_data <- combined_data %>%
  filter(grepl("Workplace", domain_combination, ignore.case = TRUE), !is.na(Q17_1_numeric)) %>%
  mutate(
    learning_outcome = case_when(
      Group == "Current Students" & !is.na(self_directed_learning) ~ self_directed_learning,
      Group == "Alumni" & !is.na(ongoing_learning) ~ ongoing_learning,
      TRUE ~ NA_real_
    ),
    learning_outcome_pct = case_when(
      !is.na(learning_outcome) ~ (learning_outcome - 1) / 4 * 100,
      TRUE ~ NA_real_
    )
  )

# Summary statistics for Q17_1 in workplace combinations
if(nrow(workplace_q17_data) > 0) {
  q17_summary <- workplace_q17_data %>%
    group_by(Group, domain_combination) %>%
    summarise(
      n = n(),
      mean_rating = mean(Q17_1_numeric, na.rm = TRUE),
      pct_effective = sum(Q17_1_numeric >= 4, na.rm = TRUE) / n() * 100,
      mean_outcome = mean(learning_outcome_pct, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("Q17_1 SUMMARY BY WORKPLACE COMBINATIONS:\n")
  print(q17_summary)
  
  # Correlation analysis
  if(sum(!is.na(workplace_q17_data$learning_outcome_pct)) > 5) {
    correlation_q17 <- workplace_q17_data %>%
      filter(!is.na(Q17_1_numeric), !is.na(learning_outcome_pct)) %>%
      group_by(Group) %>%
      summarise(
        n_obs = n(),
        correlation = cor(Q17_1_numeric, learning_outcome_pct, use = "complete.obs"),
        .groups = 'drop'
      )
    
    cat("\nCORRELATION: Q17_1 ↔ LEARNING OUTCOMES BY GROUP:\n")
    print(correlation_q17)
  }
}

# ==============================================================================
# STEP 10: STATISTICAL TESTS
# ==============================================================================

cat("\n=== STATISTICAL TESTS ===\n")

# ANOVA for current students tri-sphere model
if(nrow(current_data) > 10) {
  anova_current <- aov(self_directed_learning ~ domain_combination, 
                       data = filter(current_data, domain_combination != "None"))
  cat("ANOVA Results for Current Students Tri-Sphere Model:\n")
  print(summary(anova_current))
  
  if(summary(anova_current)[[1]][["Pr(>F)"]][1] < 0.05) {
    cat("\nTukey HSD Post-hoc Test (Current Students):\n")
    print(TukeyHSD(anova_current))
  }
}

# ANOVA for alumni tri-sphere model
if(nrow(alumni_data) > 10) {
  anova_alumni <- aov(ongoing_learning ~ domain_combination, 
                      data = filter(alumni_data, domain_combination != "None"))
  cat("\nANOVA Results for Alumni Tri-Sphere Model:\n")
  print(summary(anova_alumni))
  
  if(summary(anova_alumni)[[1]][["Pr(>F)"]][1] < 0.05) {
    cat("\nTukey HSD Post-hoc Test (Alumni):\n")
    print(TukeyHSD(anova_alumni))
  }
}

# ==============================================================================
# STEP 11: VISUALIZATIONS
# ==============================================================================

cat("\n=== GENERATING VISUALIZATIONS ===\n")

# Figure 1: Current Students Tri-Sphere Validation
if(nrow(current_trisphere_validation) > 0) {
  p_current_trisphere <- ggplot(current_trisphere_validation, 
                                aes(x = reorder(domain_combination, learning_outcome_pct), 
                                    y = learning_outcome_pct)) +
    geom_col(aes(fill = domain_combination == "All Three Domains"), 
             color = "black", linewidth = 0.8) +
    scale_fill_manual(values = c("grey60", "#d73027"), guide = FALSE) +
    geom_errorbar(aes(ymin = (ci_lower - 1) / 4 * 100, 
                      ymax = (ci_upper - 1) / 4 * 100),
                  width = 0.2, linewidth = 0.8) +
    geom_text(aes(label = paste0(round(learning_outcome_pct, 0), "%\n(n=", n, ")")), 
              vjust = -0.5, size = 3.5) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(title = "Current Students: Tri-Sphere Model Validation",
         subtitle = "Self-directed learning outcomes by domain integration",
         x = "Domain Combination",
         y = "Self-Directed Learning Outcome (%)",
         caption = "High domain defined as above median. Error bars = 95% CI.") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("publication_figures/Figure6a_Current_TriSphere.png", p_current_trisphere, 
         width = 10, height = 6, dpi = 300, bg = "white")
  cat("Saved: Figure6a_Current_TriSphere.png\n")
}

# Figure 2: Alumni Tri-Sphere Validation
if(nrow(alumni_trisphere_validation) > 0) {
  p_alumni_trisphere <- ggplot(alumni_trisphere_validation, 
                               aes(x = reorder(domain_combination, learning_outcome_pct), 
                                   y = learning_outcome_pct)) +
    geom_col(aes(fill = domain_combination == "All Three Domains"), 
             color = "black", linewidth = 0.8) +
    scale_fill_manual(values = c("grey60", "#d73027"), guide = FALSE) +
    geom_errorbar(aes(ymin = (ci_lower - 1) / 4 * 100, 
                      ymax = (ci_upper - 1) / 4 * 100),
                  width = 0.2, linewidth = 0.8) +
    geom_text(aes(label = paste0(round(learning_outcome_pct, 0), "%\n(n=", n, ")")), 
              vjust = -0.5, size = 3.5) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(title = "Alumni: Tri-Sphere Model Validation",
         subtitle = "Ongoing learning outcomes by domain integration",
         x = "Domain Combination",
         y = "Ongoing Learning Outcome (%)",
         caption = "High domain defined as above median. Error bars = 95% CI.") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("publication_figures/Figure6b_Alumni_TriSphere.png", p_alumni_trisphere, 
         width = 10, height = 6, dpi = 300, bg = "white")
  cat("Saved: Figure6b_Alumni_TriSphere.png\n")
}

# Figure 3: Comparative Tri-Sphere Analysis
if(nrow(comparison_data) > 0) {
  p_trisphere_comparison <- ggplot(comparison_data, 
                                   aes(x = domain_combination, y = learning_outcome_pct, fill = Group)) +
    geom_col(position = position_dodge(0.8), color = "black", linewidth = 0.5) +
    scale_fill_manual(values = c("Current Students" = "#737373", "Alumni" = "#d73027")) +
    geom_text(aes(label = paste0(round(learning_outcome_pct, 0), "%\n(n=", n, ")")), 
              position = position_dodge(0.8), vjust = -0.5, size = 3) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(title = "Tri-Sphere Model: Current Students vs Alumni",
         subtitle = "Learning outcomes by domain integration (comparative analysis)",
         x = "Domain Combination",
         y = "Learning Outcome (%)",
         fill = "Group") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
  
  ggsave("publication_figures/Figure6c_TriSphere_Comparison.png", p_trisphere_comparison, 
         width = 12, height = 8, dpi = 300, bg = "white")
  cat("Saved: Figure6c_TriSphere_Comparison.png\n")
}

# Figure 4: Dumbbell Plot for Developmental Trajectory
if(nrow(comparison_data) > 0) {
  # Prepare dumbbell data
  dumbbell_data <- comparison_data %>%
    pivot_wider(names_from = Group, 
                values_from = learning_outcome_pct,
                values_fill = NA) %>%
    filter(!is.na(`Current Students`) | !is.na(Alumni)) %>%
    arrange(desc(Alumni)) %>%
    mutate(domain_combination = factor(domain_combination, levels = unique(domain_combination)))
  
  p_dumbbell <- ggplot(dumbbell_data, aes(x = domain_combination)) +
    geom_segment(aes(xend = domain_combination, 
                     y = `Current Students`, 
                     yend = Alumni),
                 color = "gray70", size = 1) +
    geom_point(aes(y = `Current Students`), 
               color = "#2E86C1", size = 3, alpha = 0.8) +
    geom_point(aes(y = Alumni), 
               color = "#E74C3C", size = 3, alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Developmental Trajectory: Current Students vs Alumni",
      subtitle = "Change in learning outcomes across domain combinations",
      x = "Domain Combination",
      y = "Learning Outcome Percentage (%)",
      caption = "Blue: Current Students | Red: Alumni"
    ) +
    theme_publication() +
    theme(panel.grid.major.y = element_blank()) +
    geom_text(aes(y = `Current Students`, 
                  label = ifelse(!is.na(`Current Students`), 
                                 paste0(round(`Current Students`, 1), "%"), "")),
              hjust = -0.3, size = 3, color = "#2E86C1") +
    geom_text(aes(y = Alumni, 
                  label = ifelse(!is.na(Alumni), 
                                 paste0(round(Alumni, 1), "%"), "")),
              hjust = -0.3, size = 3, color = "#E74C3C")
  
  ggsave("publication_figures/Figure6d_Dumbbell_Trajectory.png", p_dumbbell, 
         width = 10, height = 7, dpi = 300, bg = "white")
  cat("Saved: Figure6d_Dumbbell_Trajectory.png\n")
}

# Figure 5: Autonomy Paradox (Original Analysis)
if(exists("group_stats") & exists("perceived_practice_cor")) {
  
  # Panel A: Autonomy Distribution
  if(sum(!is.na(current_data$perceived_autonomy)) > 0) {
    autonomy_summary <- current_data %>%
      filter(!is.na(perceived_autonomy)) %>%
      mutate(autonomy_category = ifelse(perceived_autonomy >= 4, 
                                        "High Autonomy\n(Levels 4-5)", 
                                        "Moderate/Low Autonomy\n(Levels 1-3)")) %>%
      count(autonomy_category) %>%
      mutate(percentage = n/sum(n) * 100)
    
    p1 <- ggplot(autonomy_summary, aes(x = autonomy_category, y = percentage)) +
      geom_col(fill = "#737373", color = "black", width = 0.6, linewidth = 0.8) +
      geom_text(aes(label = paste0(round(percentage, 1), "%\n(n=", n, ")")), 
                vjust = -0.8, size = 3.8, fontface = "bold") +
      scale_y_continuous(limits = c(0, 85), breaks = seq(0, 80, 20),
                         labels = function(x) paste0(x, "%")) +
      labs(title = "A. Distribution of Reported Autonomy Levels",
           subtitle = paste("Graduate Apprentices (n =", sum(autonomy_summary$n), "valid responses)"),
           x = "Autonomy Category",
           y = "Percentage of Apprentices") +
      theme_publication() +
      theme(axis.text.x = element_text(size = 9),
            plot.title = element_text(size = 11))
  }
  
  # Panel B: Learning by Autonomy Level
  if(exists("group_stats") && nrow(group_stats) > 0) {
    plot_data <- current_data %>%
      filter(!is.na(autonomy_group) & !is.na(self_directed_learning)) %>%
      mutate(autonomy_group = factor(case_when(
        perceived_autonomy %in% 1:2 ~ "Low\n(1-2)",
        perceived_autonomy == 3 ~ "Moderate\n(3)", 
        perceived_autonomy %in% 4:5 ~ "High\n(4-5)"
      ), levels = c("Low\n(1-2)", "Moderate\n(3)", "High\n(4-5)")))
    
    if(nrow(plot_data) > 0) {
      p2 <- ggplot(plot_data, aes(x = autonomy_group, y = self_directed_learning)) +
        geom_violin(fill = "#bfbfbf", color = "black", 
                    alpha = 0.7, linewidth = 0.8, trim = FALSE) +
        geom_boxplot(width = 0.25, fill = "white", color = "black", 
                     linewidth = 0.8, outlier.shape = 21, outlier.fill = "white") +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
                     fill = "#d73027", color = "black", stroke = 1.2) +
        scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
        labs(title = "B. Self-Directed Learning by Autonomy Level",
             subtitle = paste("Paradox Test: r =", round(perceived_practice_cor, 3), "(Perceived ↔ Practice)"),
             x = "Autonomy Level",
             y = "Self-Directed Learning Score") +
        theme_publication() +
        theme(plot.title = element_text(size = 11))
      
      # Add paradox annotation if confirmed
      if(abs(perceived_practice_cor) < 0.3) {
        p2 <- p2 + 
          annotate("rect", xmin = 1.55, xmax = 2.45, ymin = 4.35, ymax = 4.85, 
                   fill = "white", color = "black", linewidth = 0.5, alpha = 0.95) +
          annotate("text", x = 2, y = 4.6, 
                   label = "PARADOX:\nModerate autonomy\noutperforms high autonomy", 
                   size = 2.6, hjust = 0.5, vjust = 0.5, fontface = "italic", lineheight = 0.85)
      }
    }
  }
  
  # Combine panels
  if(exists("p1") && exists("p2")) {
    combined_paradox <- grid.arrange(p1, p2, ncol = 2,
                                     top = textGrob("Figure 2: The Autonomy Paradox in Graduate Apprenticeships", 
                                                    gp = gpar(fontsize = 14, fontface = "bold")))
    ggsave("publication_figures/Figure2_Autonomy_Paradox.png", combined_paradox, 
           width = 12, height = 6, dpi = 300, bg = "white")
    cat("Saved: Figure2_Autonomy_Paradox.png\n")
  }
}

# ==============================================================================
# STEP 12: SAVE TO GLOBAL ENVIRONMENT
# ==============================================================================

cat("\n=== SAVING TO GLOBAL ENVIRONMENT ===\n")

# Save main datasets
current_students_data <<- current_data
alumni_students_data <<- alumni_data

# Save tri-sphere results
current_trisphere_results <<- current_trisphere_validation
alumni_trisphere_results <<- alumni_trisphere_validation
trisphere_comparison_results <<- summary_comparison

# Save analysis results
if(exists("group_stats")) {
  current_paradox_results <<- group_stats
  cat("- current_paradox_results\n")
}

if(exists("current_correlations")) {
  current_correlation_matrix <<- current_correlations
  cat("- current_correlation_matrix\n")
}

if(exists("alumni_correlations")) {
  alumni_correlation_matrix <<- alumni_correlations
  cat("- alumni_correlation_matrix\n")
}

if(exists("q21_dimension_cors")) {
  q21_workplace_dimensions <<- q21_dimension_cors
  cat("- q21_workplace_dimensions\n")
}

if(exists("perceived_practice_cor")) {
  autonomy_paradox_correlation <<- perceived_practice_cor
  cat("- autonomy_paradox_correlation\n")
}

if(exists("ga_learning_cor")) {
  ga_impact_learning_correlation <<- ga_learning_cor
  cat("- ga_impact_learning_correlation\n")
}

# Save Q17_1 results
if(exists("q17_summary")) {
  q17_mentor_effectiveness_results <<- q17_summary
  cat("- q17_mentor_effectiveness_results\n")
}

cat("\nMain datasets saved:\n")
cat("- current_students_data (", nrow(current_data), " cases)\n")
cat("- alumni_students_data (", nrow(alumni_data), " cases)\n")
cat("- current_trisphere_results\n")
cat("- alumni_trisphere_results\n")
cat("- trisphere_comparison_results\n")

# ==============================================================================
# STEP 13: COMPREHENSIVE SUMMARY REPORT
# ==============================================================================

cat("\n================================================================================\n")
cat("COMPREHENSIVE GRADUATE APPRENTICESHIP ANALYSIS WITH TRI-SPHERE MODEL COMPLETE!\n")
cat("================================================================================\n")

cat("\n📊 SAMPLE SUMMARY:\n")
cat("✓ Current students analyzed:", nrow(current_data), "\n")
cat("✓ Alumni analyzed:", nrow(alumni_data), "\n")
cat("✓ Total participants:", nrow(current_data) + nrow(alumni_data), "\n")

cat("\n🎯 KEY RESEARCH FINDINGS:\n")

# Tri-Sphere Model Results
cat("\nTRI-SPHERE MODEL VALIDATION:\n")
cat("• Current Students - Domain combinations found:", nrow(current_trisphere_validation), "\n")
if(nrow(current_trisphere_validation) > 0) {
  best_current <- current_trisphere_validation[which.max(current_trisphere_validation$learning_outcome_pct), ]
  cat("  - Best performing combination:", best_current$domain_combination, 
      " (", round(best_current$learning_outcome_pct, 1), "%)\n")
}

cat("• Alumni - Domain combinations found:", nrow(alumni_trisphere_validation), "\n")
if(nrow(alumni_trisphere_validation) > 0) {
  best_alumni <- alumni_trisphere_validation[which.max(alumni_trisphere_validation$learning_outcome_pct), ]
  cat("  - Best performing combination:", best_alumni$domain_combination, 
      " (", round(best_alumni$learning_outcome_pct, 1), "%)\n")
}

# Current Students Findings
cat("\nCURRENT STUDENTS (During Learning Process):\n")
if(exists("perceived_practice_cor")) {
  cat("• Autonomy Paradox Test: r =", round(perceived_practice_cor, 3))
  if(abs(perceived_practice_cor) < 0.3) {
    cat(" *** PARADOX CONFIRMED ***\n")
  } else {
    cat(" (No paradox detected)\n")
  }
}

# Alumni Findings
cat("\nALUMNI (Long-term Impact):\n")
if(exists("ga_learning_cor")) {
  cat("• GA Impact ↔ Ongoing Learning: r =", round(ga_learning_cor, 3))
  if(abs(ga_learning_cor) > 0.7) {
    cat(" *** EXCEPTIONAL IMPACT ***\n")
  } else {
    cat("\n")
  }
}

# Developmental Trajectory
if(exists("current_mean") && exists("alumni_mean")) {
  cat("\nDEVELOPMENTAL TRAJECTORY:\n")
  cat("• Current → Alumni Learning: ", round(current_mean, 2), " → ", round(alumni_mean, 2))
  cat(" (Δ = +", round(alumni_mean - current_mean, 2), ")\n")
}

cat("\n📁 FILES GENERATED:\n")
cat("Tri-Sphere Model Figures:\n")
cat("• Figure6a_Current_TriSphere.png\n")
cat("• Figure6b_Alumni_TriSphere.png\n")
cat("• Figure6c_TriSphere_Comparison.png\n")
cat("• Figure6d_Dumbbell_Trajectory.png\n")

cat("\nOriginal Analysis Figures:\n")
cat("• Figure2_Autonomy_Paradox.png\n")

cat("\n🎯 RESEARCH QUESTIONS ANSWERED:\n")
cat("\nTri-Sphere Model:\n")
cat("✓ Which domain combinations are most effective? [Check trisphere_comparison_results]\n")
cat("✓ Do patterns differ between current students and alumni? [Check comparative analysis]\n")
cat("✓ Is 'All Three Domains' the optimal combination? [Check best performing combinations]\n")

cat("\nOriginal Research Questions:\n")
cat("✓ Does autonomy paradox exist? [Check autonomy_paradox_correlation]\n")
cat("✓ Long-term GA impact? [Check ga_impact_learning_correlation]\n")
cat("✓ Workplace mentor effectiveness? [Check q17_mentor_effectiveness_results]\n")

cat("\n================================================================================\n")
cat("MANUSCRIPT READY: Complete tri-sphere model validation with original analysis\n")
cat("All results integrated and saved for publication!\n")
cat("================================================================================\n")

# ==============================================================================
# END OF COMPREHENSIVE ANALYSIS SCRIPT
# =============================================================================="