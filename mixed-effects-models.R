# =========================================================================================================
# LOAD PACKAGES ----
# =========================================================================================================

#load necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(lme4)
library(performance)
library(emmeans)
library(lmerTest)
library(ggplot2)
library(grid)
library(patchwork)

# =========================================================================================================
# DATA MANAGEMENT ----
# =========================================================================================================

#load data and identify NA cases ("-")
data <- read_excel("ips_data.xlsx", na = "-")

#select cases where diagnosis and data are aligned
data <- data %>%
  filter(data_alignment == "Aligned")

#remove cases where Exp_Met <.80
data$B1_T3_Ppt_Exp[data$B1_T3_Exp_Met < 0.8] <- NA
data$B2_T3_Ppt_Exp[data$B2_T3_Exp_Met < 0.8] <- NA
data$B2_T4_Ppt_Exp[data$B2_T4_Exp_Met < 0.8] <- NA
data$B3_T3_Ppt_Exp[data$B3_T3_Exp_Met < 0.8] <- NA
data$B3_T4_Ppt_Exp[data$B3_T4_Exp_Met < 0.8] <- NA
data$B4_T3_Ppt_Exp[data$B4_T3_Exp_Met < 0.8] <- NA
data$B4_T4_Ppt_Exp[data$B4_T4_Exp_Met < 0.8] <- NA

data$pseudo_B1_T3_Ppt_Exp[data$B1_T3_Exp_Met < 0.8] <- NA
data$pseudo_B2_T3_Ppt_Exp[data$B2_T3_Exp_Met < 0.8] <- NA
data$pseudo_B2_T4_Ppt_Exp[data$B2_T4_Exp_Met < 0.8] <- NA
data$pseudo_B3_T3_Ppt_Exp[data$B3_T3_Exp_Met < 0.8] <- NA
data$pseudo_B3_T4_Ppt_Exp[data$B3_T4_Exp_Met < 0.8] <- NA
data$pseudo_B4_T3_Ppt_Exp[data$B4_T3_Exp_Met < 0.8] <- NA
data$pseudo_B4_T4_Ppt_Exp[data$B4_T4_Exp_Met < 0.8] <- NA

#flag typical/low movement skills in ASD
data <- data %>% 
  mutate(
    neurotype_4 = case_when(
      neurotype == "ASD" & mabc_interpretation %in% c("Red", "Amber") ~ "ASD_lms",
      neurotype == "ASD" & mabc_interpretation %in% c("Green") ~ "ASD_tms",
      TRUE ~ neurotype)) %>% 
  relocate(neurotype_4, .after = neurotype)

#transform data from wide to long format
data_long <- data %>%
  pivot_longer(
    cols = B1_T2_Ppt_Met:B4_T4_Ppt_Exp,
    names_sep = "_",
    names_to = c("task","trial","participant","stimulus"),
    values_to = "rho"
  ) %>%
  mutate(
    code = paste(task, trial, participant, stimulus, sep = "_"),
    block = task,
    task = recode(block,
                  B1 = "finger tapping",
                  B2 = "ball sorting",
                  B3 = "finger tapping",
                  B4 = "ball sorting"),
    stimulus = recode(stimulus,
                      Met = "metronome",
                      Exp = "experimenter"),
    instruction = recode(code,
                   B1_T2_Ppt_Met = "spontaneous",
                   B1_T3_Ppt_Exp = "spontaneous",
                   B2_T2_Ppt_Met = "spontaneous",
                   B2_T3_Ppt_Exp = "spontaneous",
                   B2_T4_Ppt_Exp = "spontaneous",
                   B3_T1_Ppt_Met = "in-phase",
                   B3_T2_Ppt_Met = "anti-phase",
                   B3_T3_Ppt_Exp = "in-phase",
                   B3_T4_Ppt_Exp = "anti-phase",
                   B4_T1_Ppt_Met = "in-phase",
                   B4_T2_Ppt_Met = "anti-phase",
                   B4_T3_Ppt_Exp = "in-phase",
                   B4_T4_Ppt_Exp = "anti-phase")
  ) %>%
  filter(!grepl("Exp_Met", code)) %>%
  select(ppt, child_age_years, child_gender, neurotype, neurotype_4,
         code, block, task, trial, stimulus, instruction, rho)

#transform data from wide to long format for pseudo pairs & real data
data <- data %>%
  rename_with(
    ~paste0("real_", .x),
    B1_T2_Ppt_Met:B4_T4_Ppt_Exp
  )

long_pseudo <- data %>%
  pivot_longer(
    cols = real_B1_T2_Ppt_Met:pseudo_B4_T4_Ppt_Exp,
    names_sep = "_",
    names_to = c("pair_type","task","trial","participant","stimulus"),
    values_to = "rho"
  ) %>%
  mutate(
    code = paste(pair_type, task, trial, participant, stimulus, sep = "_"),
    block = task,
    task = recode(block,
                  B1 = "finger tapping",
                  B2 = "ball sorting",
                  B3 = "finger tapping",
                  B4 = "ball sorting"),
    stimulus = recode(stimulus,
                      Met = "metronome",
                      Exp = "experimenter"),
    instruction = recode(code,
                         real_B1_T2_Ppt_Met = "spontaneous",
                         real_B1_T3_Ppt_Exp = "spontaneous",
                         real_B2_T2_Ppt_Met = "spontaneous",
                         real_B2_T3_Ppt_Exp = "spontaneous",
                         real_B2_T4_Ppt_Exp = "spontaneous",
                         real_B3_T1_Ppt_Met = "in-phase",
                         real_B3_T2_Ppt_Met = "anti-phase",
                         real_B3_T3_Ppt_Exp = "in-phase",
                         real_B3_T4_Ppt_Exp = "anti-phase",
                         real_B4_T1_Ppt_Met = "in-phase",
                         real_B4_T2_Ppt_Met = "anti-phase",
                         real_B4_T3_Ppt_Exp = "in-phase",
                         real_B4_T4_Ppt_Exp = "anti-phase",
                         pseudo_B1_T2_Ppt_Met = "spontaneous",
                         pseudo_B1_T3_Ppt_Exp = "spontaneous",
                         pseudo_B2_T2_Ppt_Met = "spontaneous",
                         pseudo_B2_T3_Ppt_Exp = "spontaneous",
                         pseudo_B2_T4_Ppt_Exp = "spontaneous",
                         pseudo_B3_T1_Ppt_Met = "in-phase",
                         pseudo_B3_T2_Ppt_Met = "anti-phase",
                         pseudo_B3_T3_Ppt_Exp = "in-phase",
                         pseudo_B3_T4_Ppt_Exp = "anti-phase",
                         pseudo_B4_T1_Ppt_Met = "in-phase",
                         pseudo_B4_T2_Ppt_Met = "anti-phase",
                         pseudo_B4_T3_Ppt_Exp = "in-phase",
                         pseudo_B4_T4_Ppt_Exp = "anti-phase")
  ) %>%
  filter(!grepl("Exp_Met", code)) %>%
  select(ppt, child_age_years, child_gender, neurotype, neurotype_4,
         code, pair_type, block, task, trial, stimulus, instruction, rho)

#set TD as the reference group for neurotype
data_long <- data_long %>%
  mutate(
    neurotype = factor(neurotype, levels = c("TD", "ASD", "DCD")),
    neurotype_4 = factor(neurotype_4, levels = c("TD", "ASD_lms", "ASD_tms", "DCD"))
  )

long_pseudo <- long_pseudo %>%
  mutate(
    neurotype = factor(neurotype, levels = c("TD", "ASD", "DCD")),
    neurotype_4 = factor(neurotype_4, levels = c("TD", "ASD_lms", "ASD_tms", "DCD"))
  )

#standardise for main analysis
data_long <- data_long %>%
  mutate(rho_z    = scale(rho)[, 1])

#standardise for pseudo analysis 
long_pseudo <- long_pseudo %>%
  mutate(rho_z = scale(rho)[, 1])

# ==========================================================================================================
# RQ1: DO DIFFERENT NEUROTYPES COORDINATE ABOVE CHANCE? ----
# ==========================================================================================================
# RQ1.1: PSEUDO-ANALYSIS (FINGER TAPPING) ----

RQ1.1 <- lmer(
  rho_z ~ neurotype_4 * pair_type * stimulus * instruction + (1 + stimulus * instruction | ppt),
  data = long_pseudo,
  subset = task == "finger tapping"
) #failed to converge - simplify model (Brauer & Curtin, 2018)

RQ1.1 <- lmer(
  rho_z ~ neurotype_4 * pair_type * stimulus * instruction + (1 + stimulus + instruction | ppt),
  data = long_pseudo,
  subset = task == "finger tapping"
) #failed to converge - simplify model (Brauer & Curtin, 2018)

RQ1.1 <- lmer(
  rho_z ~ neurotype_4 * pair_type * stimulus * instruction + (1 + instruction | ppt),
  data = long_pseudo,
  subset = task == "finger tapping"
) #final model

summary(RQ1.1)

#check assumptions (Meteyard & Davies, 2020; Winter, 2013)

plot(
  fitted(RQ1.1),
  residuals(RQ1.1),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs. Fitted",
) #A1: absence of non-linearity established
  #A3: heteroscedasticity present but not expected to cause bias (Schielzeth et al., 2020)

check_collinearity(RQ1.1) #A2: interactions inflated VIFs but not SEs (O'Brien, 2007)

qqnorm(residuals(RQ1.1)) #A4: normality established, though robust to deviations (Winter, 2013; Gellman & Hill, 2007)

check_outliers(RQ1.1) #A5: no outliers present

#Tukey-corrected post-hoc comparisons using the emmeans package (Macpherson, 2025)

emm_options(lmer.df = "satterthwaite")

pairs(
  emmeans(RQ1.1, ~ pair_type)
  ) #marginal effect for pair_type

pairs(
  emmeans(RQ1.1, ~ pair_type | neurotype_4 * stimulus * instruction),
  adjust = "tukey",
) #conditional effects

# RQ1.2: PSEUDO-ANALYSIS (BALL SORTING) ----

RQ1.2 <- lmer(
  rho_z ~ neurotype_4 * pair_type * stimulus * instruction + (1 + stimulus * instruction | ppt),
  data = long_pseudo,
  subset = task == "ball sorting"
) #failed to converge - simplify model (Brauer & Curtin, 2018)

RQ1.2 <- lmer(
  rho_z ~ neurotype_4 * pair_type * stimulus * instruction + (1 + stimulus + instruction | ppt),
  data = long_pseudo,
  subset = task == "ball sorting"
) #fit is singular - simplify model (Brauer & Curtin, 2018)

RQ1.2 <- lmer(
  rho_z ~ neurotype_4 * pair_type * stimulus * instruction + (1 + instruction | ppt),
  data = long_pseudo,
  subset = task == "ball sorting"
) #fit is singular - simplify model (Brauer & Curtin, 2018)

RQ1.2 <- lmer(
  rho_z ~ neurotype_4 * pair_type * stimulus * instruction + (1 | ppt),
  data = long_pseudo,
  subset = task == "ball sorting"
) #final model

summary(RQ1.2)

#check assumptions (Meteyard & Davies, 2020; Winter, 2013)

plot(
  fitted(RQ1.2),
  residuals(RQ1.2),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs. Fitted",
) #A1: absence of non-linearity established
  #A3: heteroscedasticity present but not expected to cause bias (Schielzeth et al., 2020)

check_collinearity(RQ1.2) #A2: interactions inflated VIFs but not SEs (O'Brien, 2007)

qqnorm(residuals(RQ1.2)) #A4: normality established, though robust to deviations (Winter, 2013; Gellman & Hill, 2007)

check_outliers(RQ1.2) #A5: no outliers present

#Tukey-corrected post-hoc comparisons using the emmeans package (Macpherson, 2025)

emm_options(lmer.df = "satterthwaite")

pairs(
  emmeans(RQ1.2, ~ pair_type)
) #marginal effect for pair_type

pairs(
  emmeans(RQ1.2, ~ pair_type | neurotype_4 * stimulus * instruction),
  adjust = "tukey"
) #conditional effects

# ==========================================================================================================
# RQ2: HOW DOES NEUROTYPE INFLUENCE COORDINATION STABILITY? ----
# ==========================================================================================================
# RQ2.1: MANIPULATION CHECK (FINGER TAPPING) ----

RQ2.1 <- lmer(
  rho_z ~ neurotype_4 * stimulus * instruction + (1 + stimulus * instruction | ppt),
  data = data_long,
  subset = task == "finger tapping"
) #observations outweigh random effects - simplify model

RQ2.1 <- lmer(
  rho_z ~ neurotype_4 * stimulus * instruction + (1 + stimulus + instruction | ppt),
  data = data_long,
  subset = task == "finger tapping"
) #failed to converge - simplify model (Brauer & Curtin, 2018)

RQ2.1 <- lmer(
  rho_z ~ neurotype_4 * stimulus * instruction + (1 + instruction | ppt),
  data = data_long,
  subset = task == "finger tapping"
) #final model

summary(RQ2.1)

#check assumptions (Meteyard & Davies, 2020; Winter, 2013)

plot(
  fitted(RQ2.1),
  residuals(RQ2.1),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs. Fitted",
) #A1: absence of non-linearity established
  #A3: heteroscedasticity present but not expected to cause bias (Schielzeth et al., 2020)

check_collinearity(RQ2.1) #A2: interactions inflated VIFs but not SEs (O'Brien, 2007)

qqnorm(residuals(RQ2.1)) #A4: normality established, though robust to deviations (Winter, 2013; Gellman & Hill, 2007)

check_outliers(RQ2.1) #A5: no outliers present

#Tukey-corrected post-hoc comparisons using the emmeans package (Macpherson, 2025)

emm_options(lmer.df = "satterthwaite")

pairs(
  emmeans(RQ2.1, ~ neurotype_4),
  adjust = "tukey"
) #marginal effect for neurotype

pairs(
  emmeans(RQ2.1, ~ instruction),
  adjust = "tukey"
) #marginal effect for instruction

pairs(
  emmeans(RQ2.1, ~ stimulus),
  adjust = "tukey"
) #marginal effect for stimulus

pairs(
  emmeans(RQ2.1, ~ neurotype_4 | stimulus * instruction),
  adjust = "tukey"
) #conditional effects for neurotype

# RQ2.2: MANIPULATION CHECK (BALL SORTING) ----

RQ2.2 <- lmer(
  rho_z ~ neurotype_4 * stimulus * instruction + (1 + stimulus * instruction | ppt),
  data = data_long,
  subset = task == "ball sorting"
) #fit is singular - simplify model (Brauer & Curtin, 2018)

RQ2.2 <- lmer(
  rho_z ~ neurotype_4 * stimulus * instruction + (1 + stimulus + instruction | ppt),
  data = data_long,
  subset = task == "ball sorting"
) #failed to converge - simplify model (Brauer & Curtin, 2018)

RQ2.2 <- lmer(
  rho_z ~ neurotype_4 * stimulus * instruction + (1 + instruction | ppt),
  data = data_long,
  subset = task == "ball sorting"
) #final model

summary(RQ2.2)

#check assumptions (Meteyard & Davies, 2020; Winter, 2013)

plot(
  fitted(RQ2.2),
  residuals(RQ2.2),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs. Fitted",
) #A1: absence of non-linearity established
  #A3: heteroscedasticity present but not expected to cause bias (Schielzeth et al., 2020)

check_collinearity(RQ2.2) #A2: interactions inflated VIFs but not SEs (O'Brien, 2007)

qqnorm(residuals(RQ2.2)) #A4: normality established, though robust to deviations (Winter, 2013; Gellman & Hill, 2007)

check_outliers(RQ2.2) #A5: no outliers present

#Tukey-corrected post-hoc comparisons using the emmeans package (Macpherson, 2025)

emm_options(lmer.df = "satterthwaite")

pairs(
  emmeans(RQ2.2, ~ neurotype_4),
  adjust = "tukey"
) #marginal effect for neurotype

pairs(
  emmeans(RQ2.2, ~ instruction),
  adjust = "tukey"
) #marginal effect for instruction

pairs(
  emmeans(RQ2.2, ~ stimulus),
  adjust = "tukey"
) #marginal effect for stimulus

pairs(
  emmeans(RQ2.2, ~ neurotype_4 | stimulus * instruction),
  adjust = "tukey"
) #conditional effects for neurotype

# ==========================================================================================================
# DATA VISUALISATION ----
# ==========================================================================================================
# RQ1.1: PSEUDO-ANALYSIS (FINGER TAPPING) ----

#save estimated marginal means
RQ1.1_emm <- emmeans(RQ1.1, ~ neurotype_4 * pair_type * stimulus * instruction)
RQ1.1_emm <- as.data.frame(RQ1.1_emm)

#parameters for back-transformation (mean and SD of raw rho)
pseudo_ft_rho_mean <- mean(long_pseudo$rho[long_pseudo$task == "finger tapping"], na.rm = TRUE)
pseudo_ft_rho_sd   <- sd(long_pseudo$rho[long_pseudo$task == "finger tapping"], na.rm = TRUE)

#back transform emmeans to raw rho scale
RQ1.1_plot_df <- RQ1.1_emm %>%
  mutate(
    emmean   = emmean * pseudo_ft_rho_sd + pseudo_ft_rho_mean,
    lower.CL = lower.CL * pseudo_ft_rho_sd + pseudo_ft_rho_mean,
    upper.CL = upper.CL * pseudo_ft_rho_sd + pseudo_ft_rho_mean,
    neurotype_4 = factor(neurotype_4, levels = c("TD", "ASD_tms", "ASD_lms", "DCD")),
    pair_type = factor(pair_type, levels = c("real", "pseudo")),
    stimulus = factor(stimulus, levels = c("metronome", "experimenter")),
    instruction = factor(instruction, levels = c("spontaneous", "in-phase", "anti-phase")),
    line_id = interaction(pair_type, stimulus, sep = " — ")
  )

#create individual figures
RQ1.1_plot <- function(nt) {
  ggplot(
    RQ1.1_plot_df %>% filter(neurotype_4 == nt),
    aes(
      x = instruction,
      y = emmean,
      color = line_id,
      linetype = line_id,
      group = line_id
    )
  ) +
    geom_point(
      size = 2.6,
      position = position_dodge(0.15),
      show.legend = TRUE
    ) +
    geom_line(
      linewidth = 0.9,
      position = position_dodge(0.15),
      key_glyph = "path"
    ) +
    geom_errorbar(
      aes(ymin = lower.CL, ymax = upper.CL),
      width = 0.18,
      position = position_dodge(0.15),
      show.legend = FALSE,
      linetype = "solid"  
    ) +
    scale_x_discrete(
      limits = c("spontaneous", "in-phase", "anti-phase"),
      labels = c(
        "spontaneous" = "Spontaneous",
        "in-phase"    = "In-phase",
        "anti-phase"  = "Anti-phase"
      )
    ) +
    scale_color_manual(
      values = c(
        "real — metronome"      = "#333333",
        "real — experimenter"   = "#999999",
        "pseudo — metronome"    = "#333333",
        "pseudo — experimenter" = "#999999"
      ),
      breaks = c(
        "real — metronome",
        "real — experimenter",
        "pseudo — metronome",
        "pseudo — experimenter"
      ),
      labels = c(
        "Real — Metronome",
        "Real — Experimenter",
        "Pseudo — Metronome",
        "Pseudo — Experimenter"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "real — metronome"      = "solid",
        "real — experimenter"   = "solid",
        "pseudo — metronome"    = "longdash",
        "pseudo — experimenter" = "longdash"
      ),
      breaks = c(
        "real — metronome",
        "real — experimenter",
        "pseudo — metronome",
        "pseudo — experimenter"
      ),
      labels = c(
        "Real — Metronome",
        "Real — Experimenter",
        "Pseudo — Metronome",
        "Pseudo — Experimenter"
      )
    ) +
    scale_y_continuous(
      limits = c(-0.15, 1),
      breaks = c(0, 0.25, 0.50, 0.75, 1),
      expand = expansion(mult = 0)
    ) +
    labs(
      y = "Rho",
      x = "Instruction",
      color = "Trial type and partner",
      linetype = "Trial type and partner"
    ) +
    theme(
      text = element_text(size = 13, family = "serif"),
      plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title.y.left = element_text(size = 13, colour = "black", face = "bold"),
      axis.title.x.bottom = element_text(size = 13, colour = "black", face = "bold", vjust = -0.5),
      axis.text = element_text(size = 13, colour = "black"),
      axis.text.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.ticks.x = element_line("black"),
      strip.background = element_blank(),
      strip.text = element_text(size = 13, face = "bold", colour = "black"),
      legend.position = "right",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 13),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.width = unit(1, "cm")
    )
}

#save individual figures
for (nt in levels(RQ1.1_plot_df$neurotype_4)) {
  ggsave(
    filename = paste0("MEM_Figures/RQ1.1_", nt, ".png"),
    plot     = RQ1.1_plot(nt),
    width    = 9,
    height   = 5,
    units    = "in",
    dpi      = 600
  )
}

RQ1.1_p_TD      <- RQ1.1_plot("TD")       + labs(title = "Neurotypical")
RQ1.1_p_ASD_tms <- RQ1.1_plot("ASD_tms")  + labs(title = "Autistic-TMS")
RQ1.1_p_ASD_lms <- RQ1.1_plot("ASD_lms")  + labs(title = "Autistic-LMS")
RQ1.1_p_DCD     <- RQ1.1_plot("DCD")      + labs(title = "DCD")

#combine figures into grid
RQ1.1_grid <- (RQ1.1_p_TD | RQ1.1_p_ASD_tms) /
  (RQ1.1_p_ASD_lms | RQ1.1_p_DCD) +
  plot_layout(guides = "collect") &
  theme(
    axis.title.x.bottom = element_blank(),
    axis.title.y.left = element_text(face = "plain"),
    legend.position = "right",
    legend.justification = "top",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.box.margin = margin(l = 25),
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
  )

#save grid
ggsave(
  filename = "MEM_Figures/RQ1.1_grid.png",
  plot     = RQ1.1_grid,
  width    = 12,
  height   = 8,
  units    = "in",
  dpi      = 600
  )

# RQ1.2: PSEUDO-ANALYSIS (BALL SORTING) ----

#save estimated marginal means
RQ1.2_emm <- emmeans(RQ1.2, ~ neurotype_4 * pair_type * stimulus * instruction)
RQ1.2_emm <- as.data.frame(RQ1.2_emm)

#parameters for back-transformation (mean and SD of raw rho)
pseudo_bs_rho_mean <- mean(long_pseudo$rho[long_pseudo$task == "ball sorting"], na.rm = TRUE)
pseudo_bs_rho_sd   <- sd(long_pseudo$rho[long_pseudo$task == "ball sorting"], na.rm = TRUE)

#back transform emmeans to raw rho scale
RQ1.2_plot_df <- RQ1.2_emm %>%
  mutate(
    emmean   = emmean * pseudo_bs_rho_sd + pseudo_bs_rho_mean,
    lower.CL = lower.CL * pseudo_bs_rho_sd + pseudo_bs_rho_mean,
    upper.CL = upper.CL * pseudo_bs_rho_sd + pseudo_bs_rho_mean,
    neurotype_4 = factor(neurotype_4, levels = c("TD", "ASD_tms", "ASD_lms", "DCD")),
    pair_type = factor(pair_type, levels = c("real", "pseudo")),
    stimulus = factor(stimulus, levels = c("metronome", "experimenter")),
    instruction = factor(instruction, levels = c("spontaneous", "in-phase", "anti-phase")),
    line_id = interaction(pair_type, stimulus, sep = " — ")
  )

#create individual figures
RQ1.2_plot <- function(nt) {
  ggplot(
    RQ1.2_plot_df %>% filter(neurotype_4 == nt),
    aes(
      x = instruction,
      y = emmean,
      color = line_id,
      linetype = line_id,
      group = line_id
    )
  ) +
    geom_point(
      size = 2.6,
      position = position_dodge(0.15),
      show.legend = TRUE
    ) +
    geom_line(
      linewidth = 0.9,
      position = position_dodge(0.15),
      key_glyph = "path"
    ) +
    geom_errorbar(
      aes(ymin = lower.CL, ymax = upper.CL),
      width = 0.18,
      position = position_dodge(0.15),
      show.legend = FALSE,
      linetype = "solid"  
    ) +
    scale_x_discrete(
      limits = c("spontaneous", "in-phase", "anti-phase"),
      labels = c(
        "spontaneous" = "Spontaneous",
        "in-phase"    = "In-phase",
        "anti-phase"  = "Anti-phase"
      )
    ) +
    scale_color_manual(
      values = c(
        "real — metronome"      = "#333333",
        "real — experimenter"   = "#999999",
        "pseudo — metronome"    = "#333333",
        "pseudo — experimenter" = "#999999"
      ),
      breaks = c(
        "real — metronome",
        "real — experimenter",
        "pseudo — metronome",
        "pseudo — experimenter"
      ),
      labels = c(
        "Real — Metronome",
        "Real — Experimenter",
        "Pseudo — Metronome",
        "Pseudo — Experimenter"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "real — metronome"      = "solid",
        "real — experimenter"   = "solid",
        "pseudo — metronome"    = "longdash",
        "pseudo — experimenter" = "longdash"
      ),
      breaks = c(
        "real — metronome",
        "real — experimenter",
        "pseudo — metronome",
        "pseudo — experimenter"
      ),
      labels = c(
        "Real — Metronome",
        "Real — Experimenter",
        "Pseudo — Metronome",
        "Pseudo — Experimenter"
      )
    ) +
    scale_y_continuous(
      limits = c(-0.15, 1),
      breaks = c(0, 0.25, 0.50, 0.75, 1),
      expand = expansion(mult = 0)
    ) +
    labs(
      y = "Rho",
      x = "Instruction",
      color = "Trial type and partner",
      linetype = "Trial type and partner"
    ) +
    theme(
      text = element_text(size = 13, family = "serif"),
      plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title.y.left = element_text(size = 13, colour = "black", face = "bold"),
      axis.title.x.bottom = element_text(size = 13, colour = "black", face = "bold", vjust = -0.5),
      axis.text = element_text(size = 13, colour = "black"),
      axis.text.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.ticks.x = element_line("black"),
      strip.background = element_blank(),
      strip.text = element_text(size = 13, face = "bold", colour = "black"),
      legend.position = "right",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 13),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.width = unit(1, "cm")
    )
}

#save individual figures
for (nt in levels(RQ1.2_plot_df$neurotype_4)) {
  ggsave(
    filename = paste0("MEM_Figures/RQ1.2_", nt, ".png"),
    plot     = RQ1.2_plot(nt),
    width    = 9,
    height   = 5,
    units    = "in",
    dpi      = 600
  )
}

RQ1.2_p_TD      <- RQ1.2_plot("TD")       + labs(title = "Neurotypical")
RQ1.2_p_ASD_tms <- RQ1.2_plot("ASD_tms")  + labs(title = "Autistic-TMS")
RQ1.2_p_ASD_lms <- RQ1.2_plot("ASD_lms")  + labs(title = "Autistic-LMS")
RQ1.2_p_DCD     <- RQ1.2_plot("DCD")      + labs(title = "DCD")

#combine figures into grid
RQ1.2_grid <- (RQ1.2_p_TD | RQ1.2_p_ASD_tms) /
  (RQ1.2_p_ASD_lms | RQ1.2_p_DCD) +
  plot_layout(guides = "collect") &
  theme(
    axis.title.x.bottom = element_blank(),
    axis.title.y.left = element_text(face = "plain"),
    legend.position = "right",
    legend.justification = "top",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.box.margin = margin(l = 25),
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
  )

#save grid
ggsave(
  filename = "MEM_Figures/RQ1.2_grid.png",
  plot     = RQ1.2_grid,
  width    = 12,
  height   = 8,
  units    = "in",
  dpi      = 600
  )

# RQ2.1: MANIPULATION CHECK (FINGER TAPPING) ----

#save estimated marginal means
RQ2.1_emm <- emmeans(RQ2.1, ~ neurotype_4 * stimulus * instruction)
RQ2.1_emm <- as.data.frame(RQ2.1_emm)

#parameters for back-transformation (mean and SD of raw rho)
ft_rho_mean <- mean(data_long$rho[data_long$task == "finger tapping"], na.rm = TRUE)
ft_rho_sd   <- sd(data_long$rho[data_long$task == "finger tapping"], na.rm = TRUE)

#back transform emmeans to raw rho scale
RQ2.1_plot_df <- RQ2.1_emm %>%
  mutate(
    emmean   = emmean * ft_rho_sd + ft_rho_mean,
    lower.CL = lower.CL * ft_rho_sd + ft_rho_mean,
    upper.CL = upper.CL * ft_rho_sd + ft_rho_mean,
    neurotype_4 = factor(neurotype_4, levels = c("TD", "ASD_tms", "ASD_lms", "DCD")),
    stimulus = factor(stimulus, levels = c("metronome", "experimenter")),
    instruction = factor(instruction, levels = c("spontaneous", "in-phase", "anti-phase")),
  )

#create plot
RQ2.1_plot <- ggplot(
  RQ2.1_plot_df,
  aes(
    x = instruction,
    y = emmean,
    color = neurotype_4,
    group = neurotype_4
  )
) +
  geom_point(size = 2.6, position = position_dodge(0.15)) +
  geom_line(linewidth = 0.9, position = position_dodge(0.15)) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.18,
    position = position_dodge(0.15)
  ) +
  facet_wrap(
    ~ stimulus,
    labeller = labeller(
      stimulus = c(
        metronome    = "Metronome",
        experimenter = "Experimenter"
      )
    )
  ) +
  scale_x_discrete(
    limits = c("spontaneous", "in-phase", "anti-phase"),
    labels = c(
      "spontaneous" = "Spontaneous",
      "in-phase"    = "In-phase",
      "anti-phase"  = "Anti-phase"
    )
  ) +
  scale_color_manual(
    values = c(
      "TD"       = "#00BFC4",
      "ASD_tms"  = "#F8766D",
      "ASD_lms"  = "#C44E52",
      "DCD"      = "#8B0000"
    ),
    breaks = c("TD", "ASD_tms", "ASD_lms", "DCD"),
    labels = c(
      "Neurotypical",
      "Autistic-TMS",
      "Autistic-LMS",
      "DCD"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    expand = expansion(mult = 0)
  ) +
  labs(
    y = "Rho",
    x = NULL,
    color = NULL
  ) +
  theme(
    text = element_text(size =13, family = "serif"),
    plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.y.left = element_text(size = 13, colour = "black", face = "plain"),
    axis.title.x.bottom = element_text(size = 13, colour = "black", face = "bold", vjust = -0.5),
    axis.text = element_text(size = 13, colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_line("black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 13, face = "bold", colour = "black"),
    legend.position = "right",
    legend.justification = "top",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.box.margin = margin(l = 25),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.width = unit(1, "cm")
  )

#save plot
ggsave(
  "MEM_Figures/RQ2.1.png",
  plot = RQ2.1_plot,
  width = 9,
  height = 4.5,
  units = "in",
  dpi = 600
  )

# RQ2.2: MANIPULATION CHECK (BALL SORTING) ----

#save estimated marginal means
RQ2.2_emm <- emmeans(RQ2.2, ~ neurotype_4 * stimulus * instruction)
RQ2.2_emm <- as.data.frame(RQ2.2_emm)

#parameters for back-transformation (mean and SD of raw rho)
bs_rho_mean <- mean(data_long$rho[data_long$task == "ball sorting"], na.rm = TRUE)
bs_rho_sd   <- sd(data_long$rho[data_long$task == "ball sorting"], na.rm = TRUE)

#back transform emmeans to raw rho scale
RQ2.2_plot_df <- RQ2.2_emm %>%
  mutate(
    emmean   = emmean * bs_rho_sd + bs_rho_mean,
    lower.CL = lower.CL * bs_rho_sd + bs_rho_mean,
    upper.CL = upper.CL * bs_rho_sd + bs_rho_mean,
    neurotype_4 = factor(neurotype_4, levels = c("TD", "ASD_tms", "ASD_lms", "DCD")),
    stimulus = factor(stimulus, levels = c("metronome", "experimenter")),
    instruction = factor(instruction, levels = c("spontaneous", "in-phase", "anti-phase")),
  )

RQ2.2_plot <- ggplot(
  RQ2.2_plot_df,
  aes(
    x = instruction,
    y = emmean,
    color = neurotype_4,
    group = neurotype_4
  )
) +
  geom_point(size = 2.6, position = position_dodge(0.15)) +
  geom_line(linewidth = 0.9, position = position_dodge(0.15)) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.18,
    position = position_dodge(0.15)
  ) +
  facet_wrap(
    ~ stimulus,
    labeller = labeller(
      stimulus = c(
        metronome    = "Metronome",
        experimenter = "Experimenter"
      )
    )
  ) +
  scale_x_discrete(
    limits = c("spontaneous", "in-phase", "anti-phase"),
    labels = c(
      "spontaneous" = "Spontaneous",
      "in-phase"    = "In-phase",
      "anti-phase"  = "Anti-phase"
    )
  ) +
  scale_color_manual(
    values = c(
      "TD"       = "#00BFC4",
      "ASD_tms"  = "#F8766D",
      "ASD_lms"  = "#C44E52",
      "DCD"      = "#8B0000"
    ),
    breaks = c("TD", "ASD_tms", "ASD_lms", "DCD"),
    labels = c(
      "Neurotypical",
      "Autistic-TMS",
      "Autistic-LMS",
      "DCD"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    expand = expansion(mult = 0)
  ) +
  labs(
    y = "Rho",
    x = NULL,
    color = NULL
  ) +
  theme(
    text = element_text(size =13, family = "serif"),
    plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.y.left = element_text(size = 13, colour = "black", face = "plain"),
    axis.title.x.bottom = element_text(size = 13, colour = "black", face = "bold", vjust = -0.5),
    axis.text = element_text(size = 13, colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_line("black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 13, face = "bold", colour = "black"),
    legend.position = "right",
    legend.justification = "top",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.box.margin = margin(l = 25),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.width = unit(1, "cm")
  )

ggsave(
  "MEM_Figures/RQ2.2.png",
  plot = RQ2.2_plot,
  width = 9,
  height = 4.5,
  units = "in",
  dpi = 600
  )
