
# ============================================================
# Age-Sex Net International Migration Model
# Based on Raymer et al. (2025)
# Educational implementation of Equations 3-6
# ============================================================


library(ggplot2)

dir.create("migration_results", showWarnings = FALSE)


# 2. INPUT DATA ---------------------------------------------

population <- 10000000
net_migration <- 20000
m <- 0.005

male_in_share <- 0.55
male_out_share <- 0.53

# Options: "Western" or "LowDependency"
schedule_type <- "Western"

# Options: "Neutral", "MaleYounger", "FemaleYounger"
sex_pattern <- "MaleYounger"

ages <- 0:100

stopifnot(
  population > 0,
  m > abs(net_migration) / (2 * population),
  male_in_share >= 0, male_in_share <= 1,
  male_out_share >= 0, male_out_share <= 1
)


# 3. ROGERS-CASTRO AGE SCHEDULE -----------------------------

# Equation 3:
# mx = a0 + a1*exp(-alpha1*x) +
#      a2*exp(-alpha2*(x-mu2) -
#             exp(-lambda2*(x-mu2)))

migration_schedule <- function(
    age, a0, a1, alpha1, a2, alpha2, mu2, lambda2) {
  
  child <- a1 * exp(-alpha1 * age)
  
  labour <- a2 * exp(
    -alpha2 * (age - mu2) -
      exp(-lambda2 * (age - mu2))
  )
  
  baseline <- a0
  
  baseline + child + labour
}


# 4. PARAMETERS FROM TABLE 1 --------------------------------

parameters <- data.frame(
  schedule = c("Western", "LowDependency"),
  a0 = c(0.0028, 0.0028),
  a1 = c(0.0215, 0.0050),
  alpha1 = c(0.1050, 0.1050),
  a2 = c(0.0694, 0.0800),
  alpha2 = c(0.1120, 0.1120),
  lambda2 = c(0.3910, 0.3910),
  mu_center = c(21.04, 17.09)
)

par <- parameters[
  parameters$schedule == schedule_type, ,
  drop = FALSE
]

stopifnot(nrow(par) == 1)


# 5. FUNCTION TO CREATE AGE PROPORTIONS ---------------------

make_age_profile <- function(mu2) {
  
  raw <- migration_schedule(
    age = ages,
    a0 = par$a0,
    a1 = par$a1,
    alpha1 = par$alpha1,
    a2 = par$a2,
    alpha2 = par$alpha2,
    mu2 = mu2,
    lambda2 = par$lambda2
  )
  
  # Convert raw schedule to a probability composition
  profile <- raw / sum(raw)
  
  stopifnot(abs(sum(profile) - 1) < 1e-10)
  
  return(profile)
}


# 6. IMMIGRATION AND EMIGRATION LEVELS ---------------------

# Equations 5 and 6

I_total <- population * m + net_migration / 2

E_total <- population * m - net_migration / 2

stopifnot(I_total > 0, E_total > 0)

stopifnot(
  abs(I_total - E_total - net_migration) < 1e-8
)

print(data.frame(
  Immigration = I_total,
  Emigration = E_total,
  NetMigration = I_total - E_total
))


# 7. DIRECTION-SPECIFIC AGE PROFILES ------------------------

# Positive net migration: receiving country
# Negative net migration: sending country

if (net_migration >= 0) {
  mu_in <- par$mu_center - 1
  mu_out <- par$mu_center + 1
} else {
  mu_in <- par$mu_center + 1
  mu_out <- par$mu_center - 1
}

age_in_direction <- make_age_profile(mu_in)
age_out_direction <- make_age_profile(mu_out)


# 8. SEX-SPECIFIC AGE PROFILES ------------------------------

if (sex_pattern == "Neutral") {
  
  mu_male <- par$mu_center
  mu_female <- par$mu_center
  
} else if (sex_pattern == "MaleYounger") {
  
  mu_male <- par$mu_center - 1
  mu_female <- par$mu_center + 1
  
} else if (sex_pattern == "FemaleYounger") {
  
  mu_male <- par$mu_center + 1
  mu_female <- par$mu_center - 1
  
} else {
  stop("Unknown sex_pattern")
}

age_male <- make_age_profile(mu_male)
age_female <- make_age_profile(mu_female)


# 9. COMBINED AGE-SEX PROFILES ------------------------------

# Average direction-specific and sex-specific schedules
# as described on page 593.

i_male <- (age_in_direction + age_male) / 2
i_female <- (age_in_direction + age_female) / 2

e_male <- (age_out_direction + age_male) / 2
e_female <- (age_out_direction + age_female) / 2

# Each age distribution must sum to one.

stopifnot(
  abs(sum(i_male) - 1) < 1e-10,
  abs(sum(i_female) - 1) < 1e-10,
  abs(sum(e_male) - 1) < 1e-10,
  abs(sum(e_female) - 1) < 1e-10
)


# 10. SEX COMPOSITION ---------------------------------------

female_in_share <- 1 - male_in_share
female_out_share <- 1 - male_out_share


# 11. EQUATION 4: AGE-SEX NET MIGRATION ---------------------

I_male <- I_total * male_in_share * i_male
I_female <- I_total * female_in_share * i_female

E_male <- E_total * male_out_share * e_male
E_female <- E_total * female_out_share * e_female

N_male <- I_male - E_male
N_female <- I_female - E_female


# 12. OUTPUT DATA FRAME -------------------------------------

migration_df <- data.frame(
  age = ages,
  I_male = I_male,
  I_female = I_female,
  E_male = E_male,
  E_female = E_female,
  N_male = N_male,
  N_female = N_female
)

migration_df$I_total <- with(
  migration_df, I_male + I_female
)

migration_df$E_total <- with(
  migration_df, E_male + E_female
)

migration_df$N_total <- with(
  migration_df, N_male + N_female
)

print(head(migration_df, 15))


# 13. ACCOUNTING CONSISTENCY --------------------------------

stopifnot(
  abs(sum(migration_df$I_total) - I_total) < 1e-6,
  abs(sum(migration_df$E_total) - E_total) < 1e-6,
  abs(sum(migration_df$N_total) - net_migration) < 1e-6
)

cat("\nAccounting checks passed.\n")

cat(
  "Total net migration:",
  sum(migration_df$N_total), "\n"
)


# 14. SUMMARY TABLE -----------------------------------------

summary_table <- data.frame(
  Sex = c("Male", "Female", "Total"),
  Immigration = c(
    sum(I_male),
    sum(I_female),
    I_total
  ),
  Emigration = c(
    sum(E_male),
    sum(E_female),
    E_total
  ),
  NetMigration = c(
    sum(N_male),
    sum(N_female),
    net_migration
  )
)

print(summary_table)


# 15. PLOT 1: AGE PROFILES ----------------------------------

plot_df <- data.frame(
  age = rep(ages, 3),
  value = c(
    migration_df$I_total,
    -migration_df$E_total,
    migration_df$N_total
  ),
  flow = factor(
    rep(
      c("Immigration", "Emigration", "Net migration"),
      each = length(ages)
    ),
    levels = c(
      "Immigration", "Emigration", "Net migration"
    )
  )
)

p1 <- ggplot(
  plot_df,
  aes(x = age, y = value, color = flow)
) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(linewidth = 1.1) +
  labs(
    title = "Age profiles of international migration",
    x = "Age",
    y = "Number of migrants",
    color = "Flow"
  ) +
  theme_minimal(base_size = 12)

print(p1)

ggsave(
  "migration_results/01_age_flows.png",
  p1, width = 10, height = 6, dpi = 300
)


# 16. PLOT 2: NET MIGRATION BY SEX --------------------------

sex_df <- data.frame(
  age = rep(ages, 2),
  net = c(N_male, N_female),
  sex = rep(
    c("Male", "Female"),
    each = length(ages)
  )
)

p2 <- ggplot(
  sex_df,
  aes(age, net, color = sex)
) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(linewidth = 1) +
  labs(
    title = "Net migration by age and sex",
    x = "Age",
    y = "Net migrants"
  ) +
  theme_minimal(base_size = 12)

print(p2)

ggsave(
  "migration_results/02_net_by_sex.png",
  p2, width = 10, height = 6, dpi = 300
)


# 17. PLOT 3: FOUR AGE DISTRIBUTIONS ------------------------

profile_df <- data.frame(
  age = rep(ages, 4),
  proportion = c(
    i_male, i_female,
    e_male, e_female
  ),
  profile = rep(
    c(
      "Immigration: male",
      "Immigration: female",
      "Emigration: male",
      "Emigration: female"
    ),
    each = length(ages)
  )
)

p3 <- ggplot(
  profile_df,
  aes(age, proportion, color = profile)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Age-sex migration schedules",
    x = "Age",
    y = "Proportion",
    color = "Schedule"
  ) +
  theme_minimal(base_size = 12)

print(p3)

ggsave(
  "migration_results/03_age_sex_schedules.png",
  p3, width = 10, height = 6, dpi = 300
)


# 18. SENSITIVITY ANALYSIS FOR m ----------------------------

m_values <- seq(0.003, 0.015, by = 0.001)

m_values <- m_values[
  m_values > abs(net_migration) / (2 * population)
]

sensitivity <- do.call(
  rbind,
  lapply(m_values, function(m_test) {
    
    It <- population * m_test + net_migration / 2
    Et <- population * m_test - net_migration / 2
    
    immigration_age <- It * (
      male_in_share * i_male +
        female_in_share * i_female
    )
    
    emigration_age <- Et * (
      male_out_share * e_male +
        female_out_share * e_female
    )
    
    data.frame(
      m = m_test,
      age = ages,
      net = immigration_age - emigration_age
    )
  })
)

p4 <- ggplot(
  sensitivity,
  aes(
    age, net,
    group = factor(m),
    color = factor(m)
  )
) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Sensitivity of net migration to m",
    x = "Age",
    y = "Net migrants",
    color = "m"
  ) +
  theme_minimal(base_size = 12)

print(p4)

ggsave(
  "migration_results/04_sensitivity_m.png",
  p4, width = 10, height = 6, dpi = 300
)


# 19. EXPORT RESULTS ----------------------------------------

write.csv(
  migration_df,
  "migration_results/migration_age_sex.csv",
  row.names = FALSE
)

write.csv(
  summary_table,
  "migration_results/migration_summary.csv",
  row.names = FALSE
)

write.csv(
  sensitivity,
  "migration_results/m_sensitivity.csv",
  row.names = FALSE
)

cat("\nAll outputs saved in migration_results/\n")




################################ part 2
.
# ============================================================
# OPTIONAL: CALIBRATION USING OBSERVED NET MIGRATION
# Requires observed data by single-year age and sex
# ============================================================

# Required file columns:
# age, sex, net_observed
#
# sex values: "Male", "Female"

observed <- read.csv("observed_net_migration.csv")

stopifnot(
  all(c("age", "sex", "net_observed") %in% names(observed)),
  !anyNA(observed),
  all(observed$age %in% ages),
  all(observed$sex %in% c("Male", "Female"))
)

# Check that each age-sex cell is unique.
stopifnot(
  !anyDuplicated(observed[c("age", "sex")])
)

# Ensure complete age-sex coverage.
expected_cells <- expand.grid(
  age = ages,
  sex = c("Male", "Female")
)

observed <- merge(
  expected_cells,
  observed,
  by = c("age", "sex"),
  all.x = TRUE,
  sort = FALSE
)

stopifnot(!anyNA(observed$net_observed))

# The aggregate net total is assumed fixed in the model.
stopifnot(
  abs(sum(observed$net_observed) - net_migration) < 1e-5
)


# Rebuild the age profiles for a selected peak parameter.

make_profile_mu <- function(mu) {
  values <- migration_schedule(
    age = ages,
    a0 = par$a0,
    a1 = par$a1,
    alpha1 = par$alpha1,
    a2 = par$a2,
    alpha2 = par$alpha2,
    mu2 = mu,
    lambda2 = par$lambda2
  )
  
  values / sum(values)
}


predict_age_sex <- function(theta) {
  
  m_fit <- theta[1]
  mu_in_fit <- theta[2]
  mu_out_fit <- theta[3]
  
  I_fit <- population * m_fit + net_migration / 2
  E_fit <- population * m_fit - net_migration / 2
  
  if (I_fit <= 0 || E_fit <= 0) {
    stop("Invalid m")
  }
  
  in_direction <- make_profile_mu(mu_in_fit)
  out_direction <- make_profile_mu(mu_out_fit)
  
  # Sex-specific schedules are held fixed.
  in_male <- (in_direction + age_male) / 2
  in_female <- (in_direction + age_female) / 2
  
  out_male <- (out_direction + age_male) / 2
  out_female <- (out_direction + age_female) / 2
  
  result <- data.frame(
    age = rep(ages, 2),
    sex = rep(c("Male", "Female"), each = length(ages)),
    net_estimated = c(
      I_fit * male_in_share * in_male -
        E_fit * male_out_share * out_male,
      
      I_fit * female_in_share * in_female -
        E_fit * female_out_share * out_female
    )
  )
  
  result
}


# Sum of squared errors

objective <- function(theta) {
  
  pred <- predict_age_sex(theta)
  
  comparison <- merge(
    observed,
    pred,
    by = c("age", "sex"),
    sort = FALSE
  )
  
  sum(
    (comparison$net_observed -
       comparison$net_estimated)^2
  )
}


# Optimize m, immigration mu2, emigration mu2.
# Bounds can be adjusted for the application.

m_lower <- abs(net_migration) / (2 * population) + 1e-8

lower_bounds <- c(
  max(0.001, m_lower),
  12,
  12
)

upper_bounds <- c(
  0.03,
  40,
  40
)

stopifnot(lower_bounds[1] < upper_bounds[1])

initial <- c(
  max(m, lower_bounds[1] + 1e-5),
  mu_in,
  mu_out
)

initial <- pmax(lower_bounds, pmin(initial, upper_bounds))

fit <- optim(
  par = initial,
  fn = objective,
  method = "L-BFGS-B",
  lower = lower_bounds,
  upper = upper_bounds
)

print(fit)

optimized_parameters <- data.frame(
  parameter = c("m", "mu2_in", "mu2_out"),
  estimate = fit$par
)

print(optimized_parameters)

fitted_profiles <- predict_age_sex(fit$par)

comparison <- merge(
  observed,
  fitted_profiles,
  by = c("age", "sex"),
  sort = FALSE
)

p_fit <- ggplot(comparison, aes(x = age)) +
  geom_line(
    aes(y = net_observed, color = "Observed"),
    linewidth = 0.9
  ) +
  geom_line(
    aes(y = net_estimated, color = "Estimated"),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  facet_wrap(~ sex) +
  labs(
    title = "Observed and estimated net migration",
    x = "Age",
    y = "Net migration",
    color = "Series"
  ) +
  theme_minimal(base_size = 12)

print(p_fit)

ggsave(
  "migration_results/05_model_calibration.png",
  p_fit, width = 11, height = 5.5, dpi = 300
)

write.csv(
  optimized_parameters,
  "migration_results/optimized_parameters.csv",
  row.names = FALSE
)
