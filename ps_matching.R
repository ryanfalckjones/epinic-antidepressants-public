library(tidyverse)
library(mice)
library(MatchIt)
library(MatchThem)
library(marginaleffects)
library(gtsummary)
library(readr)
library(furrr)
library(future)

# Recreate the original data with missingness
# grp == 1 is epinic
orig_epinic <- readr::read_rds("data/imp_data.rds")$data
orig_combined <- orig_epinic %>%
  mutate(grp = 1) %>%
        filter(yr >2011) %>%
        select(-any_imv, -yr) %>%
        full_join(readr::read_rds("data/df_opic.rds") %>% 
                    mutate(grp = 0) %>%
                    filter(yr > 2011) %>%
                    select (-yr) %>%
          na.omit()
        ) %>%
        mutate(across(where(is.logical), as.factor),
               CONT_ICU_LOS_DAYS = fct_collapse(CONT_ICU_LOS_DAYS,
                  "0-1 days" = "[0,1]",
                  "1-4 days" = "(1,4]",
                  "4-11 days" = "(4,11]",
                  ">11 days" = c("(11,80.1]","(11,318]")),
               edu = factor(edu, ordered = TRUE),
               SAPS_consciousness_level = factor(SAPS_consciousness_level, ordered = TRUE)) %>%
  mutate(.imp = 0, .id = paste0("row", row_number()))

df_ps <- map(1:5, ~{readr::read_rds(paste0("data/imp_", ., "/df_ps_randforest.rds")) %>%
    mutate(epsilon_ps = pmax(pmin(propensity_score, 1 - 1e-10), 1e-10)) %>%
    mutate(logit_ps = log(epsilon_ps/(1-epsilon_ps))) %>%
    select(-propensity_score, -epsilon_ps) %>%
    na.omit()
  })

mi_long <- map(1:5, ~{
  df_ps[[.x]] %>%
    mutate(.imp = .x, .id = paste0("row", row_number()))
})

mids_datasets <- bind_rows(orig_combined, !!!mi_long) %>% 
  mutate(CONT_ICU_LOS_DAYS = factor(CONT_ICU_LOS_DAYS, ordered = TRUE),
         CCIw = factor(CCIw, 
          levels = c("0", "1", ">1"), 
          ordered = TRUE)) %>%
  as.mids()

# Run the match-algorithm and write to disk
m.out.3 <- map(df_ps, ~{matchit(grp ~ sex_female + age + poly(edu, 3) + Hypertension + 
                      Diabetes + psych + poly(CCIw, 2) + poly(SAPS_consciousness_level, 4) +
                      poly(CONT_ICU_LOS_DAYS, 3) + saps_adj, 
                      data = select(.x, -logit_ps),
                      distance = .x$logit_ps,
                      exact = ~sex_female,
                      ratio = 3)}
) %>%
  as.mimids(mids_datasets) 

m.out.4 <- map(df_ps, ~{matchit(grp ~ sex_female + age + poly(edu, 3) + Hypertension + 
                      Diabetes + psych + poly(CCIw, 2) + poly(SAPS_consciousness_level, 4) +
                      poly(CONT_ICU_LOS_DAYS, 3) + saps_adj, 
                      data = select(.x, -logit_ps),
                      distance = .x$logit_ps,
                      exact = ~sex_female,
                      ratio = 4)}
) %>%
  as.mimids(mids_datasets) 

m.out.5 <- map(df_ps, ~{matchit(grp ~ sex_female + age + poly(edu, 3) + Hypertension + 
                      Diabetes + psych + poly(CCIw, 2) + poly(SAPS_consciousness_level, 4) +
                      poly(CONT_ICU_LOS_DAYS, 3) + saps_adj, 
                      data = select(.x, -logit_ps),
                      distance = .x$logit_ps,
                      exact = ~sex_female,
                      ratio = 5)}
) %>%
  as.mimids(mids_datasets) 

m.out <- m.out.3 # Designate the matching spec to use
write_rds(m.out, "match_mimids.rds")

# Create a love-plot & write to disk
library(cobalt)
m_spec <- list(m.out.3, m.out.4, m.out.5)
write_rds(m_spec, "data/m_spec.rds")

# Create a named vector of human-readable labels

# Create a named character vector: names = variable names, values = new labels
covariate_labels <- c(
  "age" = "Age",
  "sex_female" = "Female Sex",
  "poly(edu, 3)_1" = "Education Cat.^1",
  "poly(edu, 3)_2" = "Education Cat.^2",
  "poly(edu, 3)_3" = "Education Cat.^3",
  "poly(SAPS_consciousness_level, 4)_1" = "Consciousness Level ^1",
  "poly(SAPS_consciousness_level, 4)_2" = "Consciousness Level ^2",
  "poly(SAPS_consciousness_level, 4)_3" = "Consciousness Level ^3",
  "poly(SAPS_consciousness_level, 4)_4" = "Consciousness Level ^4",
  "poly(CONT_ICU_LOS_DAYS, 3)_1" = "ICU LOS Cat. ^1",
  "poly(CONT_ICU_LOS_DAYS, 3)_2" = "ICU LOS Cat. ^2",
  "poly(CONT_ICU_LOS_DAYS, 3)_3" = "ICU LOS Cat. ^3",
  "poly(CCIw, 2)_1" = "CCI Cat. ^1",
  "poly(CCIw, 2)_2" = "CCI Cat. ^2",
  "Diabetes" = "Diabetes",
  "Hypertension" = "Hypertension",
  "psych" = "Psychiatric Condition",
  "saps_adj" = "SAPS Adjusted"
)

lp <- map(m_spec, ~{love.plot(.x, 
          stats = c("mean.diffs", "ks.statistics"),
          drop.distance = T, 
          abs = T, 
          thresholds = c(m = .1), 
          binary = "std", 
          #var.order = "unadjusted",
          var.names = covariate_labels,
          position = "bottom")})

write_rds(lp, "data/lp.rds")

# Create pooled summary table across all imputations and write to disk
pooled_summary_tbl_1 <- MatchThem::complete(m.out, action = 1) %>%
  filter(!is.na(subclass)) %>%
  mutate(grp = if_else(grp == 0, "Matched Controls", "Study Cohort")) %>%
  tbl_summary(
    by = grp,
    include = c(sex_female, age, edu, Hypertension, 
                Diabetes, psych, CCIw, SAPS_consciousness_level,
                saps_adj, CONT_ICU_LOS_DAYS),
    type = list(sex_female ~ "dichotomous",
                Hypertension ~ "dichotomous",
                Diabetes ~ "dichotomous",
                psych ~ "dichotomous"), 
    value = list(sex_female ~ TRUE,
                Hypertension ~ TRUE,
                Diabetes ~ TRUE,
                psych ~ TRUE),
    label = list(sex_female ~ "Female",
                 psych ~ "Psychiatric Cond.",
                 SAPS_consciousness_level ~ "Initial Consciousness Level",
                 CCIw ~ "Charlson Comorbidity Index",
                 CONT_ICU_LOS_DAYS ~ "ICU LOS",
                 saps_adj ~ "Adj. SAPS 3-score",
                 age  ~ "Age",
                 edu ~ "Education Level"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"),
  missing = "no") %>%
  bold_labels()

pooled_summary_tbl_2 <- MatchThem::complete(m.out, action = 1) %>%
  filter(grp == 0) %>%
  mutate(grp = "All Controls") %>%
  tbl_summary(
    by = grp,
    include = c(sex_female, age, edu, Hypertension, 
                Diabetes, psych, CCIw, SAPS_consciousness_level,
                saps_adj, CONT_ICU_LOS_DAYS),
    type = list(sex_female ~ "dichotomous",
                Hypertension ~ "dichotomous",
                Diabetes ~ "dichotomous",
                psych ~ "dichotomous"), 
    value = list(sex_female ~ TRUE,
                Hypertension ~ TRUE,
                Diabetes ~ TRUE,
                psych ~ TRUE),
    label = list(sex_female ~ "Female",
                 psych ~ "Psychiatric Cond.",
                 SAPS_consciousness_level ~ "Initial Consciousness Level",
                 CCIw ~ "Charlson Comorbidity Index",
                 CONT_ICU_LOS_DAYS ~ "ICU LOS",
                 saps_adj ~ "Adj. SAPS 3-score",
                 age  ~ "Age",
                 edu ~ "Education Level"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"),
  missing = "no") %>%
  bold_labels()

pooled_summary_tbl <- tbl_merge(tbls = list(pooled_summary_tbl_1, pooled_summary_tbl_2)) %>%
  modify_spanning_header(everything() ~ NA_character_)

write_rds(pooled_summary_tbl, "data/pooled_summary_tbl.rds")

# Extract matched datasets from mimids object
md_list <- MatchThem::complete(m.out, action = "all")

# Define bootstrap function for one dataset
cluster_boot_fun_single <- function(md, R = 2000) {
  pair_ids <- levels(md$subclass)
  split_inds <- split(seq_len(nrow(md)), md$subclass)
  
  boot_fun <- function(pairs, i) {
    ids <- unlist(split_inds[pairs[i]])
    boot_md <- md[ids,]
    
    fit <- glm(ad_treatment ~ grp * (sex_female + age + poly(edu, 3) + Hypertension + 
                                       Diabetes + CCIw + poly(SAPS_consciousness_level, 4) + 
                                       psych + CONT_ICU_LOS_DAYS + saps_adj),
               data = boot_md,
               weights = boot_md$weights,
               family = "quasibinomial")
    
    md1 <- subset(boot_md, grp == 1)
    p1 <- predict(fit, type = "response", newdata = transform(md1, grp = 1))
    p0 <- predict(fit, type = "response", newdata = transform(md1, grp = 0))
    
    Ep1 <- mean(p1)
    Ep0 <- mean(p0)
    
    Ep1 / Ep0
  }
  
  set.seed(54321)
  boot::boot(pair_ids, boot_fun, R = R)
}

# Plan on how to resolve futures
      if (supportsMulticore()) {
        plan(multicore)
      } else {
        plan(multisession)
      }

# Apply to all imputations
boot_results <- future_map(md_list, 
                           ~cluster_boot_fun_single(.x, R = 2000),
                          .options = furrr_options(seed = TRUE), 
                          .progress = TRUE)

# Extract confidence intervals for each imputation separately
# boot_cis <- map(boot_results, ~boot.ci(.x, type = "bca"))

# Pool the RR's across imputations according to Rubins rules

# Assume you have a list of boot objects from each imputed dataset
# Each element in boot_results contains $t: the bootstrap estimates

# Step 1: Extract bootstrap means and variances
boot_means <- map_dbl(boot_results, ~mean(.x$t))
boot_vars <- map_dbl(boot_results, ~var(.x$t))

# Step 2: Rubin's rules
m <- length(boot_results)  # number of imputations
Q_bar <- mean(boot_means)  # pooled estimate
U_bar <- mean(boot_vars)   # within-imputation variance
B <- var(boot_means)       # between-imputation variance
T_var <- U_bar + (1 + 1/m) * B  # total variance

# Step 3: Confidence interval (normal approximation)
se <- sqrt(T_var)
z <- 1.96  # for 95% CI
ci_lower <- Q_bar - z * se
ci_upper <- Q_bar + z * se

# Step 4: Output and write estimate and CI to disk
cat("Pooled Risk Ratio Estimate:", round(Q_bar, 3), "\n")
cat("95% Confidence Interval:", round(ci_lower, 3), "-", round(ci_upper, 3), "\n")
write_rds(tibble(qbar = Q_bar, lci = ci_lower, uci = ci_upper), "data/matched_att.rds")

# Extract bootstrap estimates from each imputation
df_rr <- bind_rows(lapply(seq_along(boot_results), function(i) {
  data.frame(
    Imputation = paste("Imputation", i),
    RR = boot_results[[i]]$t
  )
}))

# Plot


# Create a dummy data frame for legend lines
legend_lines <- data.frame(
  y = c(Q_bar, ci_lower, ci_upper),
  label = c("Pooled RR", "95% CI Lower", "95% CI Upper"),
  linetype = c("dashed", "dotted", "dotted"),
  color = c("red", "blue", "blue")
)


violinplot_ci <- ggplot(df_rr, aes(x = Imputation, y = RR)) +
  geom_violin(aes(fill = Imputation), color = "gray40", alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75), show.legend = FALSE) +
  geom_hline(yintercept = Q_bar, linetype = "dashed", color = "red", linewidth = 1.2) +
  geom_hline(yintercept = ci_lower, linetype = "dotted", color = "blue", linewidth = 1) +
  geom_hline(yintercept = ci_upper, linetype = "dotted", color = "blue", linewidth = 1) +
  scale_fill_brewer(palette = "Pastel1") +
  geom_hline(data = legend_lines, aes(yintercept = y, linetype = label, color = label), linewidth = 1) +
  scale_color_manual(values = c("Pooled RR" = "red", "95% CI Lower" = "blue", "95% CI Upper" = "blue")) +
  scale_linetype_manual(values = c("Pooled RR" = "dashed", "95% CI Lower" = "dotted", "95% CI Upper" = "dotted")) +
  labs(
    title = "Distribution of Bootstrapped Risk Ratios Across Imputations",
    x = "",
    y = "Risk Ratio (RR)",
    colour = NULL,
    linetype = NULL
  ) +
  annotate("text", x = 6, y = Q_bar, label = sprintf("Pooled RR = %.2f", Q_bar),
           vjust = -1.5, hjust = 0, color = "red", size = 3.5) +
  annotate("text", x = 6, y = ci_lower, label = sprintf("95%% CI Lower = %.2f", ci_lower),
           vjust = -1.5, hjust = 0, color = "blue", size = 3.2) +
  annotate("text", x = 6, y = ci_upper, label = sprintf("95%% CI Upper = %.2f", ci_upper),
           vjust = -1.5, hjust = 0, color = "blue", size = 3.2) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.box.background = element_rect(color = "black", linewidth = 0.25),
    legend.box.margin = margin(t = 5, r = 5, b = 5)
  )


write_rds(violinplot_ci, "data/violinplot_ci.rds")
save.image("data/ps_matching.RData")
