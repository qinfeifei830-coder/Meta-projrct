

rm(list = ls())    # Clears all variables from workspace



#Load packages
library(groundhog) 
# Load packags using ground hog after last analysis so results do not change
groundhog.day="2021-01-02"
groundhog.library("metafor", groundhog.day)
groundhog.library("tidyverse", groundhog.day)
groundhog.library("janitor", groundhog.day)
groundhog.library("readxl", groundhog.day)
groundhog.library("purrr", groundhog.day)
groundhog.library("multcomp", groundhog.day)
groundhog.library("broom", groundhog.day)



# Load in injury data (incidence, mean severity, burden)

data <- read_excel("data/Inc_sev_bur_data.xlsx", sheet = "inc_sev_bur_data")

ROB <- read_excel("data/ROB_&_NOS.xlsx", sheet = "Sheet1")

REF_NO <- read_excel("data/reference_number.xlsx", sheet = "Sheet1")

# Join ROB scores

df1 <- 
  left_join(data, ROB, by = c("reference"))

# Add ENDNOTE REF NO

df1 <- 
  left_join(df1, REF_NO, by = c("reference"))

df1$reference <- 
  paste0(df1$reference, " [", df1$ref_no, "]")

df1 <-
  df1 %>%
  mutate_at(c("mean_days_missed",
              "median_days_missed",
              "total_days_missed",
              "injury_count",
              "injured_players",
              "athletes"),
            as.numeric)


# Grouping into child and adolescent groups based on age / age group

child <- 
  c("U9",
    "U10",
    "U11",
    "U12",
    "U13",
    "U10-U12",
    "10-13"
    )

adolescent <-
  c("13-17",
    "13-18",
    "14-18",
    "14-19",
    "15-19",
    "16-19",
    "16-18",
    "U14",
    "U15",
    "U16",
    "U17",
    "U18",
    "U19",
    "13-18",
    "15-18",
    "15.4",
    "17",
    "16.7",
    "U14-U18")

# Sport Type Groups

collision_sport <-
  c("Rugby Union",
    "Australian Rules Football",
    "Lacrosse",
    "Ice Hockey")

track_field <-
  c("Distance Running",
    "Track & Field")


# add groups

df1 <- 
  df1 %>% 
  mutate(age_group = case_when(age_range %in% child ~ "child",
                            age_range %in% adolescent ~ "adolescent",
                            TRUE ~"child & adolescent")) %>%
  mutate(sport_type = case_when(sport %in% collision_sport ~ "Collision Team Sport",
                                sport %in% track_field ~ "Track & Field",
                               TRUE ~ sport))


# Total days lost and injury severity calcs
  

df1 <-
  df1 %>%
  mutate(total_days_missed = case_when(
    is.na(total_days_missed) ~ injury_count * mean_days_missed,
    TRUE ~ as.numeric(total_days_missed))
  ) %>%
  mutate(mean_days_missed = case_when(
    is.na(mean_days_missed) ~ total_days_missed / injury_count,
    TRUE ~ as.numeric(mean_days_missed))
  )


med_ROB <- median(ROB$ROB, round(ROB$ROB, digits = 0))



# MODELS ASSESSING SPORT / SPORT TYPE AS MODERATOR -------------------------------------------------

# Match data ---

match_data <-
  df1 %>%
  filter(activity == "match") %>%
  filter(!is.na(exposure)) %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)


overall_match <- 
  match_data %>%
  group_by(reference, setting, sport_type, sport, activity) %>%
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data



overall_match %>% 
  ungroup() %>%
  summarise(total_exposure = sum(exposure) * 1000,
                            total_count = sum(injury_count))


# OVERALL MATCH INCIDENCE --------------------------------------------------------------------------

png(filename= "figures/Fig. 3.png", width=8.27, height=11.69, units ="in", res = 800)


match_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_match,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, setting, injury_count, sep = " | ")
  )


summary(match_incidence)
coef(match_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  match_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-70, 110),
  xlab = "Match incidence rate (events per 1,000 match h)",
  mlab = "",
  order = order(overall_match$sport_type),
  ylim=c(-2, 32),
  rows=c(3:4, 7:9, 12:28),
  psize = 1,
  refline = exp(match_incidence$beta),
  addcred = T,
  header = c("Study Reference | Study Setting | Total injury count", "Match incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences


match_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ sport_type,
    data = overall_match,
    method = "ML", # random effects
    slab = reference
  )

summary(match_incidence)


### add text for the test of subgroup differences
text(-70, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
text(-70, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(match_incidence$QM, digits=2, format="f")), ", df = ", .(match_incidence$p - 1),
                                             ", p = ", .(formatC(match_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-70, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(match_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(match_incidence$tau2, digits=2, format="f")))))


### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-70, c(29,10,5), pos=4, cex = 1, 
     c("Soccer",
       "Handball",
       "Collision Team Sport"))


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
match_incidence_handball <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (sport == "Handball"), data = overall_match, method ="ML", slab= reference)
match_incidence_soccer <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (sport == "Soccer"), data = overall_match, method ="ML", slab= reference)
match_incidence_col <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (sport_type == "Collision Team Sport"), data = overall_match, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(match_incidence_handball, row=6, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(match_incidence_soccer, row= 11, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(match_incidence_col, row= 2, cex=0.8, transf = exp, mlab="", digits = 1)


dev.off()

# CONTRASTS 

(sum_stats <- summary(glht(match_incidence, linfct=rbind(contrMat(c("Collision Team Sport"=1,
                                                                    "Handball"=1,
                                                                    "soccer"=1), 
                                                                  type="Tukey"))), test=adjusted("fdr")))
# CONTRAST DF

match_inc_cont <- broom::tidy(sum_stats)


# OVERALL MATCH BURDEN ------------------------------------------------------------------------------

png(filename= "figures/supplementary Fig. 9.png", width=8.27, height=8, units ="in", res = 800)

overall_match_bur <-
  overall_match %>% filter(
    total_days_missed > 0 
  )

overall_match_bur %>% 
  ungroup() %>%
  summarise(total_exposure = sum(exposure) * 1000,
            total_count = sum(total_days_missed))

match_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    data = overall_match_bur,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, setting, total_days_missed, sep = " | ")
  )


summary(match_burden)
coef(match_burden)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  match_burden,
  cex = 0.8,
  transf = exp,
  digits = 0,
  xlim=c(-2500, 4500),
  xlab = "Match burden rate (days missed per 1,000 match h)",
  mlab = "",
  order = order(overall_match_bur$sport_type),
  ylim=c(-2, 15),
  rows=c(2:3, 6:11),
  psize = 1,
  refline = exp(match_burden$beta),
  addcred = T,
  header = c("Study Reference | Study Setting | Total days missed", "Match burden rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences

match_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    mods = ~ sport_type,
    data = overall_match_bur,
    method = "ML",
    slab = reference
  )
summary(match_burden)

### add text for the test of subgroup differences
#text(-2500, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
#text(-2500, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(match_burden$QM, digits=2, format="f")), ", df = ", .(match_burden$p - 1),
 #                                            ", p = ", .(formatC(match_burden$QMp, digits=4, format="f")))))

# Heterogeneity
text(-2500, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(match_burden$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(match_burden$tau2, digits=2, format="f")))))




### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-2500, c(12,4), pos=4, 
     c("Soccer",
       "Collision Team Sports"))

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups

match_burden_soccer <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (sport == "Soccer"), data = overall_match_bur, method ="ML", slab= reference)
match_burden_col <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (sport_type == "Collision Team Sport"), data = overall_match_bur, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(match_burden_soccer, row= 5, cex=0.8, transf = exp, mlab="", digits = 0)
addpoly(match_burden_col, row= 1, cex=0.8, transf = exp, mlab="", digits = 0)


dev.off()


# CONTRASTS

(sum_stats <- summary(glht(match_burden, linfct=rbind(contrMat(c("Collision Team Sport"=1,
                                                                 "soccer"=1), 
                                                               type="Tukey"))), test=adjusted("fdr")))
match_bur_cont <- broom::tidy(sum_stats)




# OVERALL MATCH SEVERITY ------------------------------------------------------------------------------


png(filename= "figures/Supplementary Fig. 6.png", width=8.27, height=8, units ="in", res = 800)


match_sev <-
  overall_match %>%
  filter(total_days_missed > 0) %>%
  mutate(mean_days_missed = (total_days_missed / injury_count),
         mean_days_missed = round(mean_days_missed, 0),
         exposure = exposure / 1000, 
         days_missed_lcl = mean_days_missed / (exp(1.96 / sqrt(injury_count))),
         days_missed_ucl = mean_days_missed * (exp(1.96 / sqrt(injury_count))),
         days_missed_se = (days_missed_ucl - days_missed_lcl)/3.92)


days_missed_model <-
  rma.mv(
    mean_days_missed,
    days_missed_se,
    random = ~ 1 | reference,
    data = match_sev,
    slab = paste(reference, setting, injury_count, sep = " | ")
  )

summary(days_missed_model)

forest(
  days_missed_model,
  order = order(match_sev$sport),
  cex = 1,
  xlab = "Mean days missed",
  digits = 0,
  mlab = "",
  xlim = c(-50, 80),
  ylim=c(-1, 15),
  rows=c(2:3, 6:11),
  psize = 1,
  refline = days_missed_model$beta,
  addcred = T,
  header = c("Study Reference | Study Setting | Total injury count", "Mean days missed [95% CI]")
)

### fit meta-regression model to test for subgroup differences
days_missed_model <-
  rma.mv(
    mean_days_missed,
    mods = ~ sport_type,
    days_missed_se,
    random = ~ 1 | reference,
    data = match_sev,
    slab = paste(reference, setting, sep = " | ")
  )
summary(days_missed_model)

### add text for the test of subgroup differences
#text(-50, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
#text(-50, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(days_missed_model$QM, digits=2, format="f")), ", df = ", .(days_missed_model$p - 1),
 #                                            ", p = ", .(formatC(days_missed_model$QMp, digits=4, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups
text(-50, c(12,4), pos=4, cex = 1.2,
     c("Soccer",
       "Collision Team Sports"))

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

## fit random-effects model in soccer subgroups
days_missed_soccer <- rma.mv(
  mean_days_missed,
  days_missed_se,
  subset = (sport == "Soccer"),
  random = ~ 1 | reference,
  data = match_sev,
  slab = paste(reference, setting, sep = " | ")
)

addpoly(days_missed_soccer, row=5, cex = 1, mlab="", digits = 0)


days_missed_col <- rma.mv(
  mean_days_missed,
  days_missed_se,
  subset = (sport_type == "Collision Team Sport"),
  random = ~ 1 | reference,
  data = match_sev,
  slab = paste(reference, setting, sep = " | ")
)

addpoly(days_missed_col, row=1, cex = 1, mlab="", digits = 0)


dev.off()

# Contrasts
(sum_stats <- summary(glht(days_missed_model, linfct=rbind(contrMat(c("Collision Team Sport"=1,
                                                                      "soccer"=1), 
                                                                    type="Tukey"))), test=adjusted("fdr")))

match_sev_cont <- broom::tidy(sum_stats)



# OVERALL TRAINING INCIDENCE ------------------------------------------------------------------------------

png(filename= "figures/Fig. 4.png", width=8.27, height=11.69, units ="in", res = 800)

# TRAINING DATA

training_data <-
  df1 %>%
  filter(activity == "training") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

overall_train <- 
  training_data %>%
  group_by(reference, setting, sport, activity) %>%
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


overall_train %>% 
  ungroup() %>%
  summarise(total_exposure = sum(exposure) * 1000,
                            total_count = sum(injury_count))


training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_train,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, setting, injury_count, sep = " | ")
  )


summary(training_incidence)
coef(training_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  training_incidence,
  cex = 1,
  transf = exp,
  digits = 1,
  xlim=c(-25, 25),
  xlab = "Training incidence rate (events per 1,000 training h)",
  mlab = "",
  order = order(overall_train$sport),
  ylim=c(-2, 27),
  rows=c(3, 6:8, 11:21, 23),
  psize = 1,
  refline = exp(training_incidence$beta),
  addcred = T,
  header = c("Study Reference | Study Setting | Total injury count", "Training incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences

training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ sport,
    data = overall_train,
    method = "ML",
    slab = reference
  )
summary(training_incidence)

### add text for the test of subgroup differences
text(-25, -0.8, pos=4, cex=1, bquote(paste("Test for Subgroup Differences: ")))
text(-25, -1.8, pos=4, cex=1, bquote(paste(Q[M], " = ", .(formatC(training_incidence$QM, digits=2, format="f")), ", df = ", .(training_incidence$p - 1),
                                             ", p = ", .(formatC(training_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-25, 0.5, pos=4, cex=0.95, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(training_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(training_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.95, font=2)

### add text for the subgroups
text(-25, c(24,22,9,4), pos=4, 
     c("Track & Field",
       "Soccer",
       "Handball",
       "Australian Rules Football"))

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
train_incidence_handball <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (sport == "Handball"), data = overall_train, method ="ML", slab= reference)
train_incidence_soccer <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (sport == "Soccer"), data = overall_train, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(train_incidence_handball, row=5, cex=1, transf = exp, mlab="", digits = 1)
addpoly(train_incidence_soccer, row= 10, cex=1, transf = exp, mlab="", digits = 1)


dev.off()

# CONTRASTS

sum_stats <- summary(glht(training_incidence, linfct=rbind(contrMat(c("Track & Field"=1,
                                                                      "soccer"=1,
                                                                      "Handball"=1,
                                                                      "Australian Rules Football"=1), 
                                                                    type="Tukey"))), test=adjusted("fdr"))

train_inc_cont <- broom::tidy(sum_stats)


# OVERALL TRAINING BURDEN ---------------------------------------------------------------------------------

png(filename= "figures/Supplementary Fig. 10.png", width=8.27, height=8, units ="in", res = 800)

overall_train_bur <-
  overall_train %>% filter(
    total_days_missed > 0 
  )


overall_train_bur %>% 
  ungroup() %>%
  summarise(total_exposure = sum(exposure) * 1000,
            total_count = sum(total_days_missed))

train_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    data = overall_train_bur,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, setting, total_days_missed, sep = " | ")
  )


summary(train_burden)
coef(train_burden)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  train_burden,
  cex = 1,
  transf = exp,
  digits = 0,
  xlim=c(-50, 100),
  xlab = "Training burden rate (days missed per 1,000 training h)",
  mlab = "",
  order = order(overall_train_bur$sport),
  ylim=c(-1, 12),
  rows=c(2, 5:8),
  psize = 1,
  refline = exp(train_burden$beta),
  addcred = T,
  header = c("Study Reference | Study Setting | Total days missed", "Training burden rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences
train_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    mods = ~ sport,
    data = overall_train_bur,
    method = "ML",
    slab = reference
  )
summary(train_burden)
# 
# ### add text for the test of subgroup differences
# text(-50, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
# 
# text(-50, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(train_burden$QM, digits=2, format="f")), ", df = ", .(train_burden$p - 1),
#                                                ", p = ", .(formatC(train_burden$QMp, digits=4, format="f")))))

# Heterogeneity
text(-50, 0.5, pos=4, cex=0.95, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(train_burden$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(train_burden$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.95, font=2)

### add text for the subgroups

text(-50, c(9, 3), pos=4, 
     c("Soccer",
       "Australian Rules Football"))

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
train_burden_soccer <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (sport == "Soccer"), data = overall_train_bur, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(train_burden_soccer, row= 4, cex=1, transf = exp, mlab="", digits = 0)


dev.off()



# OVERALL TRAINING SEVERITY  ---------------------------------------------------------------------------------


png(filename= "figures/Supplementary Fig. 7.png", width=8.27, height=8, units ="in", res = 800)


train_sev <-
  overall_train %>%
  filter(total_days_missed > 0) %>%
  mutate(mean_days_missed = (total_days_missed / injury_count),
         mean_days_missed = round(mean_days_missed, 0),
         exposure = exposure / 1000, 
         days_missed_lcl = mean_days_missed / (exp(1.96 / sqrt(injury_count))),
         days_missed_ucl = mean_days_missed * (exp(1.96 / sqrt(injury_count))),
         days_missed_se = (days_missed_ucl - days_missed_lcl)/3.92)


### Model

days_missed_model <-
  rma.mv(
    mean_days_missed,
    days_missed_se,
    random = ~ 1 | reference,
    data = train_sev,
    slab = paste(reference, setting, injury_count, sep = " | ")
  )

summary(days_missed_model)

forest(
  days_missed_model,
  order = order(train_sev$sport),
  cex = 1,
  xlab = "Mean days missed",
  digits = 0,
  mlab = "",
  xlim = c(-30, 40),
  ylim=c(-1, 12),
  rows=c(2, 5:8),
  psize = 1,
  refline = days_missed_model$beta,
  addcred = T,
  header = c("Study Reference | Study Setting | Total injury count", "Mean days missed [95% CI]")
)

### fit meta-regression model to test for subgroup differences
days_missed_model <-
  rma.mv(
    mean_days_missed,
    mods = ~ sport,
    days_missed_se,
    random = ~ 1 | reference,
    data = train_sev,
    slab = paste(reference, setting, sep = " | ")
  )
summary(days_missed_model)
# 
# ### add text for the test of subgroup differences
# text(-30, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

# text(-30, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(days_missed_model$QM, digits=2, format="f")), ", df = ", .(days_missed_model$p - 1),
#                                              ", p = ", .(formatC(days_missed_model$QMp, digits=2, format="f")))))


### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.95, font=2)

### add text for the subgroups

text(-30, c(9, 3), pos=4, 
     c("Soccer",
       "Australian Rules Football"))


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

# fit random-effects model in soccer subgroups
 days_missed_soccer <- rma.mv(
   mean_days_missed,
   days_missed_se,
   subset = (sport == "Soccer"),
   random = ~ 1 | reference,
   data = train_sev,
   slab = paste(reference, setting, sep = " | ")
 )

# add ploy
addpoly(days_missed_soccer, row=4, cex = 1, mlab="", digits = 0)


dev.off()


# TOTAL INJURY INCIDENCE ---------------------------------------------------------------------------------------


all_injury <-
  df1 %>%
  filter(activity == "overall") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

all_injury <- 
  all_injury %>%
  group_by(reference, setting, sport_type, sport, activity) %>%
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data

all_injury %>% 
  ungroup %>%
  summarise(total_exposure = sum(exposure) * 1000,
                       total_count = sum(injury_count))


png(filename= "figures/Fig. 2.png", width=8.27, height=11.69, units ="in", res = 800)


all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = all_injury,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, setting, injury_count, sep = " | ")
  )


summary(all_incidence)
coef(all_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  all_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-30, 40),
  xlab = "Total incidence rate (events per 1,000 h)",
  mlab = "",
  order = order(all_injury$sport_type),
  ylim=c(-2, 45),
  rows=c(3, 6, 9:10, 13, 16:18, 21:32, 35:37, 40:41),
  psize = 1,
  refline = exp(all_incidence$beta),
  addcred = T,
  header = c("Study Reference | Study Setting | Total injury count", "Total incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences
all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ sport_type,
    data = all_injury,
    method = "ML",
    slab = reference
  )

summary(all_incidence)

### add text for the test of subgroup differences
text(-30, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

text(-30, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(all_incidence$QM, digits=2, format="f")), ", df = ", .(all_incidence$p - 1),
                                             ", p = ", .(formatC(all_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-30, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(all_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(all_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-30, c(42, 38, 33, 19, 14, 11, 7, 4), pos=4, cex = 1, 
     c("Track & Field",
       "Tennis",
       "Soccer",
       "Handball",
       "Floorball",
       "Collision Team Sport",
       "Basketball",
       "Alpine Skiing"))


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
all_incidence_handball <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (sport == "Handball"),
                                   data = all_injury, method ="ML", slab= reference)

all_incidence_tennis <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (sport == "Tennis"),
                                   data = all_injury, method ="ML", slab= reference)

all_incidence_soccer <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (sport == "Soccer"),
                                   data = all_injury, method ="ML", slab= reference)

all_incidence_track <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (sport_type == "Track & Field"),
                              data = all_injury, method ="ML", slab= reference)

all_incidence_col <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (sport_type == "Collision Team Sport"),
                                   data = all_injury, method ="ML", slab= reference)


### add summary polygons for the three subgroups
addpoly(all_incidence_handball, row=15, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(all_incidence_tennis, row= 34, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(all_incidence_soccer, row= 20, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(all_incidence_track, row= 39, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(all_incidence_col, row= 8, cex=0.8, transf = exp, mlab="", digits = 1)

dev.off()

# contrasts

sum_stats <- summary(glht(all_incidence, linfct=rbind(contrMat(c("Alpine Skieing"=1,
                                                                 "Basketball"=1,
                                                                 "Collision Team Sport"=1,
                                                                 "Floorball" =1,
                                                                 "Handball"=1,
                                                                 "soccer"=1,
                                                                 "Tennis"=1,
                                                                 "Track & Field"=1), type="Tukey"))), test=adjusted("fdr"))

all_inc_cont <- broom::tidy(sum_stats)


# TOTAL INJURY BURDEN -----------------------------------------------------------------------------------------

png(filename= "figures/Supplementary Fig. 8.png", width=8.27, height=10, units ="in", res = 800)

all_injury_bur <-
  all_injury %>% filter(
    total_days_missed > 0)


all_injury_bur %>% 
  ungroup %>%
  summarise(total_exposure = sum(exposure) * 1000,
            total_count = sum(total_days_missed))


all_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    data = all_injury_bur,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, setting, total_days_missed, sep = " | ")
  )


summary(all_burden)
coef(all_burden)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  all_burden,
  cex = 0.8,
  transf = exp,
  digits = 0,
  xlim=c(-200, 300),
  xlab = "Total burden rate (days missed per 1,000 h)",
  mlab = "",
  order = order(all_injury_bur$sport),
  ylim=c(-2, 20),
  rows=c(2, 4, 7:14, 16),
  psize = 1,
  refline = exp(all_burden$beta),
  addcred = T,
  header = c("Study Reference | Study Setting | Total days missed", "Total burden rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences

all_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    mods = ~ sport,
    data = all_injury_bur,
    method = "ML",
    slab = reference
  )
summary(all_burden)
# 
 ### add text for the test of subgroup differences
#text(-200, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
# 
#text(-200, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(all_burden$QM, digits=2, format="f")), ", df = ", .(all_burden$p - 1),
 #                                               ", p = ", .(formatC(all_burden$QMp, digits=4, format="f")))))

# Heterogeneity
text(-200, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(all_burden$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(all_burden$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-200, c(17, 15,5, 3), pos=4, 
     c("Tennis",
       "Soccer",
       "Distance Running",
       "Australian Rules Football"))


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

all_burden_soccer <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (sport == "Soccer"), data = all_injury_bur, method ="ML", slab= reference)

addpoly(all_burden_soccer, row= 6, cex=0.8, transf = exp, mlab="", digits = 0)


dev.off()

# CONTRASTS


sum_stats <- summary(glht(all_burden, linfct=rbind(contrMat(c("Australian Rules Football"=1,
                                                              "Distance Running" =1,
                                                              "soccer"=1,
                                                              "Tennis"=1), type="Tukey"))), test=adjusted("fdr"))

all_bur_cont <- broom::tidy(sum_stats)


# TOTAL INJURY SEVERITY -----------------------------------------------------------------------------------------

png(filename= "figures/Supplementary Fig. 5.png", width=8.27, height=10, units ="in", res = 800)


all_sev <-
  all_injury %>%
  filter(total_days_missed > 0) %>%
  mutate(mean_days_missed = (total_days_missed / injury_count),
         mean_days_missed = round(mean_days_missed, 0),
         exposure = exposure / 1000, 
         days_missed_lcl = mean_days_missed / (exp(1.96 / sqrt(injury_count))),
         days_missed_ucl = mean_days_missed * (exp(1.96 / sqrt(injury_count))),
         days_missed_se = (days_missed_ucl - days_missed_lcl)/3.92)


### Model

days_missed_model <-
  rma.mv(
    mean_days_missed,
    days_missed_se,
    random = ~ 1 | reference,
    data = all_sev,
    slab = paste(reference, setting, injury_count, sep = " | ")
  )

summary(days_missed_model)

forest(
  days_missed_model,
  order = order(all_sev$sport),
  cex = 0.8,
  xlab = "Mean days missed",
  digits = 0,
  mlab = "",
  xlim = c(-40, 40),
  ylim=c(-2, 20),
  rows=c(2, 4, 7:14, 16),
  psize = 1,
  refline = days_missed_model$beta,
  addcred = T,
  header = c("Study Reference | Study Setting | Total injury count", "Mean days missed [95% CI]")
)



### fit meta-regression model to test for subgroup differences
days_missed_model <-
  rma.mv(
    mean_days_missed,
    mods = ~ sport,
    days_missed_se,
    random = ~ 1 | reference,
    data = all_sev,
    slab = paste(reference, setting, sep = " | ")
  )
summary(days_missed_model)
# 
# ### add text for the test of subgroup differences
# text(-40, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
# 
# text(-40, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(days_missed_model$QM, digits=2, format="f")), ", df = ", .(days_missed_model$p - 1),
#                                              ", p = ", .(formatC(days_missed_model$QMp, digits=4, format="f")))))
# 

 ### set font expansion factor (as in forest() above) and use bold italic
 ### font and save original settings in object 'op'
 op <- par(cex=0.75, font=2)
 
 ### add text for the subgroups
 
 text(-40, c(17, 15,5, 3), pos=4, 
      c("Tennis",
        "Soccer",
        "Distance Running",
        "Australian Rules Football"))
 
 
 ## switch to bold font
par(font=2)
 
 ### set par back to the original settings
par(op)



## fit random-effects model in soccer subgroups
days_missed_soccer <- rma.mv(
  mean_days_missed,
  days_missed_se,
  subset = (sport == "Soccer"),
  random = ~ 1 | reference,
  data = all_sev,
  slab = paste(reference, setting, sep = " | ")
)

addpoly(days_missed_soccer, row=6, cex = 0.8, mlab="", digits = 0)


dev.off()


# SAVE CONTRASTS


list_of_datasets <- list("Match Inc" = match_inc_cont,
                         "Match Bur" = match_bur_cont, 
                         "Match Sev" = match_sev_cont,
                         "Train Inc" = train_inc_cont,
                         "Total Inc" = all_inc_cont,
                         "Total Bur" = all_bur_cont)
write.xlsx(list_of_datasets, file = "results/Contrasts.xlsx", 
           colNames = TRUE, rowNames = FALSE, append = FALSE)




# MODELS ASSESSING STUDY QUALITY AS MODERATOR -------------------------------------------------

# OVERALL MATCH INCIDENCE ---------------------------------------------------------------------


match_data <-
  df1 %>%
  filter(activity == "match") %>%
  filter(!is.na(exposure)) %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

overall_match <- 
  match_data %>%
  group_by(reference, ROB, sport, activity) %>% # setting removed - not needed in this plot
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data



overall_match <-
  overall_match %>%
  mutate(quality = case_when(
    ROB >= med_ROB ~ "above",
    ROB < med_ROB ~ "below"
  ))


png(filename= "figures/Quality_Forest1(up).png", width=8.27, height=11.69, units ="in", res = 400)


match_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_match,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


# FOREST

forest(
  match_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-60, 100),
  xlab = "Match incidence rate (events per 1,000 match h)",
  mlab = "",
  order = order(overall_match$quality),
  ylim=c(-2, 27),
  rows=c(3:12, 15:23),
  psize = 1,
  refline = exp(match_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Match incidence rate [95% CI]")
)


match_qual <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ quality,
    data = overall_match,
    method = "ML", # random effects
    slab = reference
  )

summary(match_qual)



### add text for the test of subgroup differences
text(-60, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

text(-60, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(match_qual$QM, digits=2, format="f")), ", df = ", .(match_qual$p - 1),
                                             ", p = ", .(formatC(match_qual$QMp, digits=4, format="f")))))

# Heterogeneity
text(-60, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(match_qual$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(match_qual$tau2, digits=2, format="f")))))


### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-60, c(13,24), pos=4, cex = 1, 
     c(paste0("\u2265", med_ROB, " STROBE-SIIS rating"),
       paste0("<", med_ROB, " STROBE-SIIS rating"))
)


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
match_incidence_below <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (quality == "above"), data = overall_match, method ="ML", slab= reference)

match_incidence_above <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (quality == "below"), data = overall_match, method ="ML", slab= reference)


addpoly(match_incidence_below, row= 14, cex=0.8, transf = exp, mlab="", digits = 1)

addpoly(match_incidence_above, row= 2, cex=0.8, transf = exp, mlab="", digits = 1)


dev.off()

# OVERALL MATCH BURDEN -------------------------------------------------------------------------------------------

png(filename= "figures/Quality_Forest2(up).png", width=8.27, height=8, units ="in", res = 400)

overall_match_bur <-
  overall_match %>% filter(
    total_days_missed > 0 
  )


match_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    data = overall_match_bur,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, total_days_missed, sep = " | ")
  )


summary(match_burden)
coef(match_burden)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  match_burden,
  cex = 0.8,
  transf = exp,
  digits = 0,
  xlim=c(-2500, 4000),
  xlab = "Match burden rate (days missed per 1,000 match h)",
  mlab = "",
  order = order(overall_match_bur$quality),
  ylim=c(-2, 16),
  rows=c(3:7, 10:12),
  psize = 1,
  refline = exp(match_burden$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total days missed", "Match burden rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences

match_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    mods = ~ quality,
    data = overall_match_bur,
    method = "ML",
    slab = reference
  )
summary(match_burden)

### add text for the test of subgroup differences
text(-2500, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
text(-2500, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(match_burden$QM, digits=2, format="f")), ", df = ", .(match_burden$p - 1),
                                               ", p = ", .(formatC(match_burden$QMp, digits=4, format="f")))))

# Heterogeneity
text(-2500, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(match_burden$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(match_burden$tau2, digits=2, format="f")))))




### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-2500, c(8,13), pos=4, 
     c(paste0("\u2265", med_ROB, " STROBE-SIIS rating"),
       paste0("<", med_ROB, " STROBE-SIIS rating"))
)

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups

match_burden_above <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (quality == "above"), data = overall_match_bur, method ="ML", slab= reference)
match_burden_below <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (quality == "below"), data = overall_match_bur, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(match_burden_above, row= 2, cex=0.8, transf = exp, mlab="", digits = 0)
addpoly(match_burden_below, row= 9, cex=0.8, transf = exp, mlab="", digits = 0)


dev.off()


# OVERALL MATCH SEVERITY --------------------------------------------



png(filename= "figures/Quality_Forest3(up).png", width=8.27, height=8, units ="in", res = 400)


match_sev <-
  overall_match %>%
  filter(total_days_missed > 0) %>%
  mutate(mean_days_missed = (total_days_missed / injury_count),
         mean_days_missed = round(mean_days_missed, 0),
         exposure = exposure / 1000, 
         days_missed_lcl = mean_days_missed / (exp(1.96 / sqrt(injury_count))),
         days_missed_ucl = mean_days_missed * (exp(1.96 / sqrt(injury_count))),
         days_missed_se = (days_missed_ucl - days_missed_lcl)/3.92)


days_missed_model <-
  rma.mv(
    mean_days_missed,
    days_missed_se,
    random = ~ 1 | reference,
    data = match_sev,
    slab = paste(reference, sport, injury_count, sep = " | ")
  )

summary(days_missed_model)

forest(
  days_missed_model,
  order = order(match_sev$quality),
  cex = 0.8,
  xlab = "Mean days missed",
  digits = 0,
  mlab = "",
  xlim = c(-50, 80),
  ylim=c(-2, 16),
  rows=c(3:7, 10:12),
  psize = 1,
  refline = days_missed_model$beta,
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Mean days missed [95% CI]")
)

### fit meta-regression model to test for subgroup differences
days_missed_model <-
  rma.mv(
    mean_days_missed,
    mods = ~ quality,
    days_missed_se,
    random = ~ 1 | reference,
    data = match_sev,
    slab = paste(reference, sport, sep = " | ")
  )
summary(days_missed_model)

### add text for the test of subgroup differences
text(-50, -0.8, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ")))
text(-50, -1.2, pos=4, cex=0.75, bquote(paste(Q[M], " = ", .(formatC(days_missed_model$QM, digits=2, format="f")), ", df = ", .(days_missed_model$p - 1),
                                             ", p = ", .(formatC(days_missed_model$QMp, digits=4, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups
text(-50, c(8,13), pos=4, 
     c(paste0("\u2265", med_ROB, " STROBE-SIIS rating"),
       paste0("<", med_ROB, " STROBE-SIIS rating"))
)

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

## fit random-effects model in soccer subgroups
days_missed_above <- rma.mv(mean_days_missed, days_missed_se, subset = (quality == "above"), random = ~ 1 | reference, data = match_sev, slab = paste(reference, sport, sep = " | "))
days_missed_below <- rma.mv(mean_days_missed, days_missed_se, subset = (quality == "below"), random = ~ 1 | reference, data = match_sev, slab = paste(reference, sport, sep = " | "))

addpoly(days_missed_above, row=2, cex = 0.8, mlab="", digits = 0)
addpoly(days_missed_below, row=9, cex = 0.8, mlab="", digits = 0)


dev.off()


# OVERALL TRAINING INCIDENCE ----------------------------------------------------------------------


training_data <-
  df1 %>%
  filter(activity == "training") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

overall_train <- 
  training_data %>%
  group_by(reference, ROB, sport, activity) %>% # setting removed - not needed in this plot
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


overall_train <-
  overall_train %>%
  mutate(quality = case_when(
    ROB >= med_ROB ~ "above",
    ROB < med_ROB ~ "below"
  ))



png(filename= "figures/Quality_Forest4(up).png", width=8.27, height=11.69, units ="in", res = 400)



training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_train,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


summary(training_incidence)
coef(training_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  training_incidence,
  cex = 1,
  transf = exp,
  digits = 1,
  xlim=c(-25, 18),
  xlab = "Training incidence rate (events per 1,000 training h)",
  mlab = "",
  order = order(overall_train$quality),
  ylim=c(-2, 22),
  rows=c(3:11, 14:18),
  psize = 1,
  refline = exp(training_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Training incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences

training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ quality,
    data = overall_train,
    method = "ML",
    slab = reference
  )
summary(training_incidence)

### add text for the test of subgroup differences
text(-25, -0.8, pos=4, cex=1, bquote(paste("Test for Subgroup Differences: ")))
text(-25, -1.8, pos=4, cex=1, bquote(paste(Q[M], " = ", .(formatC(training_incidence$QM, digits=2, format="f")), ", df = ", .(training_incidence$p - 1),
                                           ", p = ", .(formatC(training_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-25, 0.5, pos=4, cex=0.95, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(training_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(training_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.95, font=2)

### add text for the subgroups
text(-25, c(12,19), pos=4, 
     c(paste0("\u2265", med_ROB, " STROBE-SIIS rating"),
       paste0("<", med_ROB, " STROBE-SIIS rating"))
)
       
  

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
train_incidence_above <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (quality == "above"), data = overall_train, method ="ML", slab= reference)
train_incidence_below <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (quality == "below"), data = overall_train, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(train_incidence_above, row=2, cex=1, transf = exp, mlab="", digits = 1)
addpoly(train_incidence_below, row= 13, cex=1, transf = exp, mlab="", digits = 1)


dev.off()


# OVERALL TRAINING BURDEN & OVERALL TRAINING SEVERITY

# Only above quality journals so no comparison can be made



# TOTAL INJURY INCIDENCE -------------------------------------------------------------------------

all_injury <-
  df1 %>%
  filter(activity == "overall") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

all_injury <- 
  all_injury %>%
  group_by(reference, ROB, sport, activity) %>%
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data

all_injury %>% 
  ungroup %>%
  summarise(total_exposure = sum(exposure) * 1000,
            total_count = sum(injury_count))

all_injury <-
  all_injury %>%
  mutate(quality = case_when(
    ROB >= med_ROB ~ "above",
    ROB < med_ROB ~ "below"
  ))



png(filename= "figures/Quality_Forest5(up).png", width=8.27, height=11.69, units ="in", res = 400)


all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = all_injury,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


summary(all_incidence)
coef(all_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  all_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-30, 40),
  xlab = "Total incidence rate (events per 1,000 h)",
  mlab = "",
  order = order(all_injury$quality),
  ylim=c(-2, 32),
  rows=c(3:16, 19:27),
  psize = 1,
  refline = exp(all_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Total incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences
all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ quality,
    data = all_injury,
    method = "ML",
    slab = reference
  )

summary(all_incidence)

### add text for the test of subgroup differences
text(-30, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

text(-30, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(all_incidence$QM, digits=2, format="f")), ", df = ", .(all_incidence$p - 1),
                                             ", p = ", .(formatC(all_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-30, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(all_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(all_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-30, c(17,28), pos=4, 
     c(paste0("\u2265", med_ROB, " STROBE-SIIS rating"),
       paste0("<", med_ROB, " STROBE-SIIS rating"))
)


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
all_incidence_above <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (quality == "above"),
                                   data = all_injury, method ="ML", slab= reference)

all_incidence_below <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (quality == "below"),
                                 data = all_injury, method ="ML", slab= reference)



### add summary polygons for the three subgroups
addpoly(all_incidence_above, row=2, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(all_incidence_below, row= 18, cex=0.8, transf = exp, mlab="", digits = 1)


dev.off()


# TOTAL INJURY BURDEN ------------------------------------------------------------------------------------



png(filename= "figures/Quality_Forest6.png", width=8.27, height=10, units ="in", res = 400)

all_injury_bur <-
  all_injury %>% filter(
    total_days_missed > 0)


all_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    data = all_injury_bur,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, total_days_missed, sep = " | ")
  )


summary(all_burden)
coef(all_burden)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  all_burden,
  cex = 0.8,
  transf = exp,
  digits = 0,
  xlim=c(-200, 300),
  xlab = "Total burden rate (days missed per 1,000 h)",
  mlab = "",
  order = order(all_injury_bur$quality),
  ylim=c(-2, 17),
  rows=c(3:10, 13),
  psize = 1,
  refline = exp(all_burden$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total days missed", "Total burden rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences

all_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    mods = ~ quality,
    data = all_injury_bur,
    method = "ML",
    slab = reference
  )
summary(all_burden)
# 
### add text for the test of subgroup differences
text(-200, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
# 
text(-200, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(all_burden$QM, digits=2, format="f")), ", df = ", .(all_burden$p - 1),
                                              ", p = ", .(formatC(all_burden$QMp, digits=4, format="f")))))

# Heterogeneity
text(-200, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(all_burden$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(all_burden$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-200, c(11,14), pos=4, 
     c(paste0("\u2265", med_ROB, " STROBE-SIIS rating"),
       paste0("<", med_ROB, " STROBE-SIIS rating"))
)



### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

all_burden_above <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (quality == "above"), data = all_injury_bur, method ="ML", slab= reference)

addpoly(all_burden_above, row= 2, cex=0.8, transf = exp, mlab="", digits = 0)


dev.off()


# TOTAL INJURY SEVERITY -----------------------------------------------------------------------------------


png(filename= "figures/Quality_forest7.png", width=8.27, height=10, units ="in", res = 400)


all_sev <-
  all_injury %>%
  filter(total_days_missed > 0) %>%
  mutate(mean_days_missed = (total_days_missed / injury_count),
         mean_days_missed = round(mean_days_missed, 0),
         exposure = exposure / 1000, 
         days_missed_lcl = mean_days_missed / (exp(1.96 / sqrt(injury_count))),
         days_missed_ucl = mean_days_missed * (exp(1.96 / sqrt(injury_count))),
         days_missed_se = (days_missed_ucl - days_missed_lcl)/3.92)


### Model

days_missed_model <-
  rma.mv(
    mean_days_missed,
    days_missed_se,
    random = ~ 1 | reference,
    data = all_sev,
    slab = paste(reference, sport, injury_count, sep = " | ")
  )

summary(days_missed_model)

forest(
  days_missed_model,
  order = order(all_sev$quality),
  cex = 0.8,
  xlab = "Mean days missed",
  digits = 0,
  mlab = "",
  xlim = c(-40, 40),
  ylim=c(-2, 16),
  rows=c(2:9, 12),
  psize = 1,
  refline = days_missed_model$beta,
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Mean days missed [95% CI]")
)



### fit meta-regression model to test for subgroup differences
days_missed_model <-
  rma.mv(
    mean_days_missed,
    mods = ~ quality,
    days_missed_se,
    random = ~ 1 | reference,
    data = all_sev,
    slab = paste(reference, sport, sep = " | ")
  )
summary(days_missed_model)
# 
# ### add text for the test of subgroup differences
text(-40, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
# 
text(-40, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(days_missed_model$QM, digits=2, format="f")), ", df = ", .(days_missed_model$p - 1),
                                             ", p = ", .(formatC(days_missed_model$QMp, digits=4, format="f")))))
# 

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-40, c(10,13), pos=4, 
     c(paste0("\u2265", med_ROB, " STROBE-SIIS rating"),
       paste0("<", med_ROB, " STROBE-SIIS rating"))
)

## switch to bold font
par(font=2)

### set par back to the original settings
par(op)



## fit random-effects model in soccer subgroups
days_missed_soccer <- rma.mv(
  mean_days_missed,
  days_missed_se,
  subset = (quality == "above"),
  random = ~ 1 | reference,
  data = all_sev,
  slab = paste(reference, sport, sep = " | ")
)

addpoly(days_missed_soccer, row=1, cex = 0.8, mlab="", digits = 0)


dev.off()



# AGE GROUP / RANGE AS MODERATOR -------------------------------------------------------------


# MATCH INJURY INCIDENCE ---------------------------------------------------------------------

match_data <-
  df1 %>%
  filter(activity == "match") %>%
  filter(!is.na(exposure)) %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)


overall_match <- 
  match_data %>%
  group_by(reference, age_group, sport, activity) %>%
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


# OVERALL MATCH INCIDENCE --------------------------------------------------------------------------

png(filename= "figures/Age_Forest1.png", width=8.27, height=11.69, units ="in", res = 400)


match_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_match,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


summary(match_incidence)
coef(match_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  match_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-70, 110),
  xlab = "Match incidence rate (events per 1,000 match h)",
  mlab = "",
  order = order(overall_match$age_group),
  ylim=c(-2, 33),
  rows=c(3:20, 23:25, 28:29),
  psize = 1,
  refline = exp(match_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Match incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences


match_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ age_group,
    data = overall_match,
    method = "ML", # random effects
    slab = reference
  )

summary(match_incidence)


### add text for the test of subgroup differences
text(-70, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
text(-70, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(match_incidence$QM, digits=2, format="f")), ", df = ", .(match_incidence$p - 1),
                                             ", p = ", .(formatC(match_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-70, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(match_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(match_incidence$tau2, digits=2, format="f")))))


### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-70, c(21,26,30), pos=4, cex = 1, 
     c("Adolescent (13-19 yrs)",
       "Child (6-12 yrs)",
       "Child & Adolescent (6-19 yrs)"))


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
match_incidence_ad <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (age_group == "adolescent"), data = overall_match, method ="ML", slab= reference)
match_incidence_child <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (age_group == "child"), data = overall_match, method ="ML", slab= reference)
match_incidence_child_ad <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (age_group == "child & adolescent"), data = overall_match, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(match_incidence_ad, row=2, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(match_incidence_child, row= 22, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(match_incidence_child_ad, row= 27, cex=0.8, transf = exp, mlab="", digits = 1)


dev.off()


# MATCH INJURY BURDEN ------------------------------------------------------------------------------

# OVERALL MATCH BURDEN ------------------------------------------------------------------------------

png(filename= "figures/Age_Forest2.png", width=8.27, height=11.69, units ="in", res = 400)

overall_match_bur <-
  overall_match %>% filter(
    total_days_missed > 0 
  )

overall_match_bur %>% 
  ungroup() %>%
  summarise(total_exposure = sum(exposure) * 1000,
            total_count = sum(total_days_missed))

match_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    data = overall_match_bur,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, total_days_missed, sep = " | ")
  )


summary(match_burden)
coef(match_burden)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  match_burden,
  cex = 0.8,
  transf = exp,
  digits = 0,
  xlim=c(-2700, 4000),
  xlab = "Match burden rate (days missed per 1,000 match h)",
  mlab = "",
  order = order(overall_match_bur$age_group),
  ylim=c(-2, 21),
  rows=c(3:9, 12:13, 16:17),
  psize = 1,
  refline = exp(match_burden$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total days missed", "Match burden rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences

match_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    mods = ~ age_group,
    data = overall_match_bur,
    method = "ML",
    slab = reference
  )
summary(match_burden)

### add text for the test of subgroup differences
text(-2700, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
text(-2700, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(match_burden$QM, digits=2, format="f")), ", df = ", .(match_burden$p - 1),
                                               ", p = ", .(formatC(match_burden$QMp, digits=4, format="f")))))

# Heterogeneity
text(-2700, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(match_burden$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(match_burden$tau2, digits=2, format="f")))))




### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-2700, c(10,14,18), pos=4, 
     c("Adolescent (13-19 yrs)",
       "Child (6-12 yrs)",
       "Child & Adolescent (6-19 yrs)"))

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups

match_burden_ad <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (age_group == "adolescent"), data = overall_match_bur, method ="ML", slab= reference)
match_burden_child <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (age_group == "child"), data = overall_match_bur, method ="ML", slab= reference)
match_burden_ad_child <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (age_group == "child & adolescent"), data = overall_match_bur, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(match_burden_ad, row= 2, cex=0.8, transf = exp, mlab="", digits = 0)
addpoly(match_burden_child, row= 11, cex=0.8, transf = exp, mlab="", digits = 0)
addpoly(match_burden_ad_child, row= 15, cex=0.8, transf = exp, mlab="", digits = 0)

dev.off()


# Match SEVERITY --------------------------------------------------------------------------


png(filename= "figures/Age_Forest3.png", width=8.27, height=11.69, units ="in", res = 400)


match_sev <-
  overall_match %>%
  filter(total_days_missed > 0) %>%
  mutate(mean_days_missed = (total_days_missed / injury_count),
         mean_days_missed = round(mean_days_missed, 0),
         exposure = exposure / 1000, 
         days_missed_lcl = mean_days_missed / (exp(1.96 / sqrt(injury_count))),
         days_missed_ucl = mean_days_missed * (exp(1.96 / sqrt(injury_count))),
         days_missed_se = (days_missed_ucl - days_missed_lcl)/3.92)


days_missed_model <-
  rma.mv(
    mean_days_missed,
    days_missed_se,
    random = ~ 1 | reference,
    data = match_sev,
    slab = paste(reference, sport, injury_count, sep = " | ")
  )

summary(days_missed_model)

forest(
  days_missed_model,
  order = order(match_sev$age_group),
  cex = 0.8,
  xlab = "Mean days missed",
  digits = 0,
  mlab = "",
  xlim = c(-50, 80),
  ylim=c(-2, 21),
  rows=c(3:9, 12:13, 16:17),
  psize = 1,
  refline = days_missed_model$beta,
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Mean days missed [95% CI]")
)

### fit meta-regression model to test for subgroup differences
days_missed_model <-
  rma.mv(
    mean_days_missed,
    mods = ~ age_group,
    days_missed_se,
    random = ~ 1 | reference,
    data = match_sev,
    slab = paste(reference, sport, sep = " | ")
  )
summary(days_missed_model)

### add text for the test of subgroup differences
text(-50, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
text(-50, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(days_missed_model$QM, digits=2, format="f")), ", df = ", .(days_missed_model$p - 1),
                                             ", p = ", .(formatC(days_missed_model$QMp, digits=4, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups
text(-50, c(10,14,18), pos=4, 
     c("Adolescent (13-19 yrs)",
       "Child (6-12 yrs)",
       "Child & Adolescent (6-19 yrs)"))

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

## fit random-effects model in soccer subgroups
days_missed_ad <- rma.mv(mean_days_missed, days_missed_se, subset = (age_group == "adolescent"),random = ~ 1 | reference,data = match_sev, slab = paste(reference, sport, sep = " | "))
days_missed_child <- rma.mv(mean_days_missed, days_missed_se, subset = (age_group == "child"),random = ~ 1 | reference,data = match_sev, slab = paste(reference, sport, sep = " | "))
days_missed_child_ad <- rma.mv(mean_days_missed, days_missed_se, subset = (age_group == "child & adolescent"),random = ~ 1 | reference,data = match_sev, slab = paste(reference, sport, sep = " | "))

# add ploy
addpoly(days_missed_ad, row=2, cex = 0.8, mlab="", digits = 0)
addpoly(days_missed_child, row=11, cex = 0.8, mlab="", digits = 0)
addpoly(days_missed_child_ad, row=15, cex = 0.8, mlab="", digits = 0)


dev.off()


# TRAINING DATA ------------------------------------------------------------------------------------------



# OVERALL TRAINING INCIDENCE ------------------------------------------------------------------------------

png(filename= "figures/Age_Forest4.png", width=8.27, height=11.69, units ="in", res = 400)

# TRAINING DATA

training_data <-
  df1 %>%
  filter(activity == "training") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

overall_train <- 
  training_data %>%
  group_by(reference, age_group, sport) %>%
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data



training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_train,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


summary(training_incidence)
coef(training_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  training_incidence,
  cex = 1,
  transf = exp,
  digits = 1,
  xlim=c(-25, 25),
  xlab = "Training incidence rate (events per 1,000 training h)",
  mlab = "",
  order = order(overall_train$age_group),
  ylim=c(-2, 27),
  rows=c(3:15, 18:19, 22:23),
  psize = 1,
  refline = exp(training_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Training incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences

training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ age_group,
    data = overall_train,
    method = "ML",
    slab = reference
  )
summary(training_incidence)

### add text for the test of subgroup differences
text(-25, -0.8, pos=4, cex=1, bquote(paste("Test for Subgroup Differences: ")))
text(-25, -1.8, pos=4, cex=1, bquote(paste(Q[M], " = ", .(formatC(training_incidence$QM, digits=2, format="f")), ", df = ", .(training_incidence$p - 1),
                                           ", p = ", .(formatC(training_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-25, 0.5, pos=4, cex=0.95, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(training_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(training_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.95, font=2)

### add text for the subgroups
text(-25, c(16,20,24), pos=4, 
     c("Adolescent (13-19 yrs)",
       "Child (6-12 yrs)",
       "Child & Adolescent (6-19 yrs)"))

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
train_incidence_ad <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (age_group == "adolescent"), 
                               data = overall_train, method ="ML", slab= reference)

train_incidence_child <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (age_group == "child"), 
                                  data = overall_train, method ="ML", slab= reference)

train_incidence_child_ad <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (age_group == "child & adolescent"), 
                                     data = overall_train, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(train_incidence_ad, row=2, cex = 1, mlab="", digits = 1)
addpoly(train_incidence_child, row=17, cex = 1, mlab="", digits = 1)
addpoly(train_incidence_child_ad, row=21, cex = 1, mlab="", digits = 1)


dev.off()



# OVERALL TRAINING BURDEN ---------------------------------------------------------------------------------

png(filename= "figures/Age_Forest5.png", width=8.27, height=8, units ="in", res = 400)

overall_train_bur <-
  overall_train %>% filter(
    total_days_missed > 0 
  )

train_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    data = overall_train_bur,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, total_days_missed, sep = " | ")
  )


summary(train_burden)
coef(train_burden)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  train_burden,
  cex = 0.8,
  transf = exp,
  digits = 0,
  xlim=c(-120, 200),
  xlab = "Training burden rate (days missed per 1,000 training h)",
  mlab = "",
  order = order(overall_train_bur$age_group),
  ylim=c(-1, 18),
  rows=c(3:6, 9:10, 13:14),
  psize = 1,
  refline = exp(train_burden$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total days missed", "Training burden rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences
train_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    mods = ~ age_group,
    data = overall_train_bur,
    method = "ML",
    slab = reference
  )
summary(train_burden)
# 
# ### add text for the test of subgroup differences
text(-120, -0.8, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ")))
# 
text(-120, -1.2, pos=4, cex=0.75, bquote(paste(Q[M], " = ", .(formatC(train_burden$QM, digits=2, format="f")), ", df = ", .(train_burden$p - 1),
                                             ", p = ", .(formatC(train_burden$QMp, digits=4, format="f")))))

# Heterogeneity
text(-120, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(train_burden$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(train_burden$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-120, c(7, 11, 15), pos=4, 
     c("Adolescent (13-19 yrs)",
       "Child (6-12 yrs)",
       "Child & Adolescent (6-19 yrs)"))

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
train_burden_ad <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (age_group == "adolescent"), data = overall_train_bur, method ="ML", slab= reference)
train_burden_child <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (age_group == "child"), data = overall_train_bur, method ="ML", slab= reference)
train_burden_child_ad <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (age_group == "child & adolescent"), data = overall_train_bur, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(train_burden_ad, row= 2, cex=0.8, transf = exp, mlab="", digits = 0)
addpoly(train_burden_child, row= 8, cex=0.8, transf = exp, mlab="", digits = 0)
addpoly(train_burden_child_ad, row= 12, cex=0.8, transf = exp, mlab="", digits = 0)


dev.off()

# OVERALL TRAINING SEVERITY ----------------------------------------------------------------------------------------------


png(filename= "figures/Age_Forest6.png", width=8.27, height=8, units ="in", res = 400)


train_sev <-
  overall_train %>%
  filter(total_days_missed > 0) %>%
  mutate(mean_days_missed = (total_days_missed / injury_count),
         mean_days_missed = round(mean_days_missed, 0),
         exposure = exposure / 1000, 
         days_missed_lcl = mean_days_missed / (exp(1.96 / sqrt(injury_count))),
         days_missed_ucl = mean_days_missed * (exp(1.96 / sqrt(injury_count))),
         days_missed_se = (days_missed_ucl - days_missed_lcl)/3.92)


### Model

days_missed_model <-
  rma.mv(
    mean_days_missed,
    days_missed_se,
    random = ~ 1 | reference,
    data = train_sev,
    slab = paste(reference, sport, injury_count, sep = " | ")
  )

summary(days_missed_model)

forest(
  days_missed_model,
  order = order(train_sev$age_group),
  cex = 1,
  xlab = "Mean days missed",
  digits = 0,
  mlab = "",
  xlim = c(-30, 40),
  ylim=c(-1, 18),
  rows=c(3:6, 9:10, 13:14),
  psize = 1,
  refline = days_missed_model$beta,
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Mean days missed [95% CI]")
)

### fit meta-regression model to test for subgroup differences
days_missed_model <-
  rma.mv(
    mean_days_missed,
    mods = ~ age_group,
    days_missed_se,
    random = ~ 1 | reference,
    data = train_sev,
    slab = paste(reference, sport, sep = " | ")
  )
summary(days_missed_model)
# 
# ### add text for the test of subgroup differences
text(-30, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

text(-30, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(days_missed_model$QM, digits=2, format="f")), ", df = ", .(days_missed_model$p - 1),
                                             ", p = ", .(formatC(days_missed_model$QMp, digits=4, format="f")))))


### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.95, font=2)

### add text for the subgroups

text(-30, c(7, 11, 15), pos=4, 
     c("Adolescent (13-19 yrs)",
       "Child (6-12 yrs)",
       "Child & Adolescent (6-19 yrs)"))


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

# fit random-effects model in soccer subgroups
days_missed_ad <- rma.mv(mean_days_missed,days_missed_se,subset = (age_group == "adolescent"),random = ~ 1 | reference,data = train_sev, slab = paste(reference, sport, sep = " | "))
days_missed_child <- rma.mv(mean_days_missed,days_missed_se,subset = (age_group == "child"),random = ~ 1 | reference,data = train_sev, slab = paste(reference, sport, sep = " | "))
days_missed_child_ad <- rma.mv(mean_days_missed,days_missed_se,subset = (age_group == "child & adolescent"),random = ~ 1 | reference,data = train_sev, slab = paste(reference, sport, sep = " | "))

# add ploy
addpoly(days_missed_ad, row=2, cex = 1, mlab="", digits = 0)
addpoly(days_missed_child, row=8, cex = 1, mlab="", digits = 0)
addpoly(days_missed_child_ad, row=12, cex = 1, mlab="", digits = 0)


dev.off()



# TOTAL INJURY BY AGE GROUP



# TOTAL INJURY INCIDENCE ---------------------------------------------------------------------------------------


all_injury <-
  df1 %>%
  filter(activity == "overall") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

all_injury <- 
  all_injury %>%
  group_by(reference, age_group, sport) %>%
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


png(filename= "figures/Age_Forest7.png", width=8.27, height=11.69, units ="in", res = 400)


all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = all_injury,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


summary(all_incidence)
coef(all_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  all_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-30, 40),
  xlab = "Total incidence rate (events per 1,000 h)",
  mlab = "",
  order = order(all_injury$age_group),
  ylim=c(-2, 37),
  rows=c(3:19, 22:23, 26:32),
  psize = 1,
  refline = exp(all_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Total incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences
all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ age_group,
    data = all_injury,
    method = "ML",
    slab = reference
  )

summary(all_incidence)

### add text for the test of subgroup differences
text(-30, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

text(-30, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(all_incidence$QM, digits=2, format="f")), ", df = ", .(all_incidence$p - 1),
                                             ", p = ", .(formatC(all_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-30, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(all_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(all_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-30, c(20, 24, 33), pos=4, cex = 1, 
     c("Adolescent (13-19 yrs)",
       "Child (6-12 yrs)",
       "Child & Adolescent (6-19 yrs)"))



### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
all_incidence_ad <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (age_group == "adolescent"),
                                   data = all_injury, method ="ML", slab= reference)

all_incidence_child <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (age_group == "child"),
                                 data = all_injury, method ="ML", slab= reference)

all_incidence_child_ad <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (age_group == "child & adolescent"),
                                 data = all_injury, method ="ML", slab= reference)



### add summary polygons for the three subgroups
addpoly(all_incidence_ad, row=2, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(all_incidence_child, row= 21, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(all_incidence_child_ad, row= 25, cex=0.8, transf = exp, mlab="", digits = 1)


dev.off()


# TOTAL INJURY BURDEN -----------------------------------------------------------------------------------------

png(filename= "figures/Age_Forest8.png", width=8.27, height=10, units ="in", res = 400)

all_injury_bur <-
  all_injury %>% filter(
    total_days_missed > 0)

all_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    data = all_injury_bur,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, total_days_missed, sep = " | ")
  )


summary(all_burden)
coef(all_burden)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  all_burden,
  cex = 0.8,
  transf = exp,
  digits = 0,
  xlim=c(-220, 350),
  xlab = "Total burden rate (days missed per 1,000 h)",
  mlab = "",
  order = order(all_injury_bur$age_group),
  ylim=c(-2, 22),
  rows=c(3:10, 13:14, 17:18),
  psize = 1,
  refline = exp(all_burden$beta),
  addcred = T,
  header = c("Study Reference | Sport| Total days missed", "Total burden rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences

all_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    mods = ~ age_group,
    data = all_injury_bur,
    method = "ML",
    slab = reference
  )
summary(all_burden)
# 
### add text for the test of subgroup differences
text(-220, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
# 
text(-220, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(all_burden$QM, digits=2, format="f")), ", df = ", .(all_burden$p - 1),
                                              ", p = ", .(formatC(all_burden$QMp, digits=4, format="f")))))

# Heterogeneity
text(-220, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(all_burden$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(all_burden$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-220, c(11, 15, 19), pos=4, 
     c("Adolescent (13-19 yrs)",
       "Child (6-12 yrs)",
       "Child & Adolescent (6-19 yrs)"))


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

all_burden_ad <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (age_group == "adolescent"), data = all_injury_bur, method ="ML", slab= reference)
all_burden_child <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (age_group == "child"), data = all_injury_bur, method ="ML", slab= reference)
all_burden_child_ad <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (age_group == "child & adolescent"), data = all_injury_bur, method ="ML", slab= reference)

addpoly(all_burden_ad, row= 2, cex=0.8, transf = exp, mlab="", digits = 0)
addpoly(all_burden_child, row= 12, cex=0.8, transf = exp, mlab="", digits = 0)
addpoly(all_burden_child_ad, row= 16, cex=0.8, transf = exp, mlab="", digits = 0)


dev.off()


# TOTAL INJURY SEVERITY -----------------------------------------------------------------------------------------

png(filename= "figures/Age_Forest9.png", width=8.27, height=10, units ="in", res = 400)


all_sev <-
  all_injury %>%
  filter(total_days_missed > 0) %>%
  mutate(mean_days_missed = (total_days_missed / injury_count),
         mean_days_missed = round(mean_days_missed, 0),
         exposure = exposure / 1000, 
         days_missed_lcl = mean_days_missed / (exp(1.96 / sqrt(injury_count))),
         days_missed_ucl = mean_days_missed * (exp(1.96 / sqrt(injury_count))),
         days_missed_se = (days_missed_ucl - days_missed_lcl)/3.92)


### Model

days_missed_model <-
  rma.mv(
    mean_days_missed,
    days_missed_se,
    random = ~ 1 | reference,
    data = all_sev,
    slab = paste(reference, sport, injury_count, sep = " | ")
  )

summary(days_missed_model)

forest(
  days_missed_model,
  order = order(all_sev$age_group),
  cex = 0.8,
  xlab = "Mean days missed",
  digits = 0,
  mlab = "",
  xlim = c(-40, 40),
  ylim=c(-2, 22),
  rows=c(3:10, 13:14, 17:18),
  psize = 1,
  refline = days_missed_model$beta,
  addcred = T,
  header = c("Study Reference | Sport| Total injury count", "Mean days missed [95% CI]")
)

### fit meta-regression model to test for subgroup differences
days_missed_model <-
  rma.mv(
    mean_days_missed,
    mods = ~ age_group,
    days_missed_se,
    random = ~ 1 | reference,
    data = all_sev,
    slab = paste(reference, sport, sep = " | ")
  )
summary(days_missed_model)
# 
# ### add text for the test of subgroup differences
text(-40, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
# 
text(-40, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(days_missed_model$QM, digits=2, format="f")), ", df = ", .(days_missed_model$p - 1),
                                             ", p = ", .(formatC(days_missed_model$QMp, digits=4, format="f")))))


### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-40, c(11, 15, 19), pos=4, 
     c("Adolescent (13-19 yrs)",
       "Child (6-12 yrs)",
       "Child & Adolescent (6-19 yrs)"))

## switch to bold font
par(font=2)

### set par back to the original settings
par(op)

## fit random-effects model in soccer subgroups
days_missed_ad <- rma.mv(mean_days_missed, days_missed_se, subset = (age_group == "adolescent"), random = ~ 1 | reference, data = all_sev, slab = paste(reference, sport, sep = " | "))
days_missed_child <- rma.mv(mean_days_missed, days_missed_se, subset = (age_group == "child"), random = ~ 1 | reference, data = all_sev, slab = paste(reference, sport, sep = " | "))
days_missed_child_ad <- rma.mv(mean_days_missed, days_missed_se, subset = (age_group == "child & adolescent"), random = ~ 1 | reference, data = all_sev, slab = paste(reference, sport, sep = " | "))

addpoly(days_missed_ad, row=2, cex = 0.8, mlab="", digits = 0)
addpoly(days_missed_child, row=12, cex = 0.8, mlab="", digits = 0)
addpoly(days_missed_child_ad, row=16, cex = 0.8, mlab="", digits = 0)


dev.off()



# MODELS ASSESSING STUDY FORMAT / TYPE --------------------------------------------------------

# OVERALL MATCH INCIDENCE ---------------------------------------------------------------------


match_data <-
  df1 %>%
  filter(activity == "match") %>%
  filter(!is.na(exposure)) %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

overall_match <- 
  match_data %>%
  group_by(reference, format, sport) %>% # setting removed - not needed in this plot
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


png(filename= "figures/Format_Forest1.png", width=8.27, height=11.69, units ="in", res = 400)


match_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_match,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


# FOREST

forest(
  match_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-60, 100),
  xlab = "Match incidence rate (events per 1,000 match h)",
  mlab = "",
  order = order(overall_match$format),
  ylim=c(-2, 28),
  rows=c(3:16, 19:23),
  psize = 1,
  refline = exp(match_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Match incidence rate [95% CI]")
)


match_qual <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ format,
    data = overall_match,
    method = "ML", # random effects
    slab = reference
  )

summary(match_qual)



### add text for the test of subgroup differences
text(-60, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

text(-60, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(match_qual$QM, digits=2, format="f")), ", df = ", .(match_qual$p - 1),
                                             ", p = ", .(formatC(match_qual$QMp, digits=4, format="f")))))

# Heterogeneity
text(-60, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(match_qual$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(match_qual$tau2, digits=2, format="f")))))


### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-60, c(17,24), pos=4, cex = 1, 
     c("Season",
       "Tournament")
)


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
match_incidence_season <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (format == "season"), data = overall_match, method ="ML", slab= reference)

match_incidence_tournament <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (format == "tournament"), data = overall_match, method ="ML", slab= reference)


addpoly(match_incidence_season, row= 2, cex=0.8, transf = exp, mlab="", digits = 1)

addpoly(match_incidence_tournament, row= 18, cex=0.8, transf = exp, mlab="", digits = 1)


dev.off()

# OVERALL MATCH BURDEN --------------------------------------------------------------


png(filename= "figures/Format_Forest2.png", width=8.27, height=8, units ="in", res = 400)

overall_match_bur <-
  overall_match %>% filter(
    total_days_missed > 0 
  )


match_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    data = overall_match_bur,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, total_days_missed, sep = " | ")
  )


summary(match_burden)
coef(match_burden)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  match_burden,
  cex = 0.8,
  transf = exp,
  digits = 0,
  xlim=c(-2500, 4000),
  xlab = "Match burden rate (days missed per 1,000 match h)",
  mlab = "",
  order = order(overall_match_bur$format),
  ylim=c(-2, 16),
  rows=c(3:9, 12),
  psize = 1,
  refline = exp(match_burden$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total days missed", "Match burden rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences

match_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    mods = ~ format,
    data = overall_match_bur,
    method = "ML",
    slab = reference
  )
summary(match_burden)

### add text for the test of subgroup differences
text(-2500, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
text(-2500, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(match_burden$QM, digits=2, format="f")), ", df = ", .(match_burden$p - 1),
                                               ", p = ", .(formatC(match_burden$QMp, digits=4, format="f")))))

# Heterogeneity
text(-2500, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(match_burden$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(match_burden$tau2, digits=2, format="f")))))




### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-2500, c(10,13), pos=4, 
     c("Season",
       "Tournament")
)

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups

match_burden_season <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (format == "season"), 
                                data = overall_match_bur, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(match_burden_season, row= 2, cex=0.8, transf = exp, mlab="", digits = 0)

dev.off()



# OVERALL MATCH SEVERITY --------------------------------------------

png(filename= "figures/Format_Forest3.png", width=8.27, height=8, units ="in", res = 400)


match_sev <-
  overall_match %>%
  filter(total_days_missed > 0) %>%
  mutate(mean_days_missed = (total_days_missed / injury_count),
         mean_days_missed = round(mean_days_missed, 0),
         exposure = exposure / 1000, 
         days_missed_lcl = mean_days_missed / (exp(1.96 / sqrt(injury_count))),
         days_missed_ucl = mean_days_missed * (exp(1.96 / sqrt(injury_count))),
         days_missed_se = (days_missed_ucl - days_missed_lcl)/3.92)


days_missed_model <-
  rma.mv(
    mean_days_missed,
    days_missed_se,
    random = ~ 1 | reference,
    data = match_sev,
    slab = paste(reference, sport, injury_count, sep = " | ")
  )

summary(days_missed_model)

forest(
  days_missed_model,
  order = order(match_sev$format),
  cex = 0.8,
  xlab = "Mean days missed",
  digits = 0,
  mlab = "",
  xlim = c(-50, 80),
  ylim=c(-2, 16),
  rows=c(3:9, 12),
  psize = 1,
  refline = days_missed_model$beta,
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Mean days missed [95% CI]")
)

### fit meta-regression model to test for subgroup differences
days_missed_model <-
  rma.mv(
    mean_days_missed,
    mods = ~ format,
    days_missed_se,
    random = ~ 1 | reference,
    data = match_sev,
    slab = paste(reference, sport, sep = " | ")
  )
summary(days_missed_model)

### add text for the test of subgroup differences
text(-50, -0.8, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ")))
text(-50, -1.2, pos=4, cex=0.75, bquote(paste(Q[M], " = ", .(formatC(days_missed_model$QM, digits=2, format="f")), ", df = ", .(days_missed_model$p - 1),
                                              ", p = ", .(formatC(days_missed_model$QMp, digits=4, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups
text(-50, c(10,13), pos=4, 
     c("Season",
       "Tournament")
)

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

## fit random-effects model in soccer subgroups
days_missed_above <- rma.mv(mean_days_missed, days_missed_se, subset = (format == "season"), 
                            random = ~ 1 | reference, data = match_sev, slab = paste(reference, sport, sep = " | "))

addpoly(days_missed_above, row=2, cex = 0.8, mlab="", digits = 0)


dev.off()

# OVERALL TRAINING INCIDENCE ----------------------------------------------------------------------


training_data <-
  df1 %>%
  filter(activity == "training") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

overall_train <- 
  training_data %>%
  group_by(reference, format, sport) %>% # setting removed - not needed in this plot
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


png(filename= "figures/Format_Forest4.png", width=8.27, height=11.69, units ="in", res = 400)



training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_train,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


summary(training_incidence)
coef(training_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  training_incidence,
  cex = 1,
  transf = exp,
  digits = 1,
  xlim=c(-25, 18),
  xlab = "Training incidence rate (events per 1,000 training h)",
  mlab = "",
  order = order(overall_train$format),
  ylim=c(-2, 23),
  rows=c(3:14, 17:18),
  psize = 1,
  refline = exp(training_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Training incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences

training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ format,
    data = overall_train,
    method = "ML",
    slab = reference
  )
summary(training_incidence)

### add text for the test of subgroup differences
text(-25, -0.8, pos=4, cex=1, bquote(paste("Test for Subgroup Differences: ")))
text(-25, -1.8, pos=4, cex=1, bquote(paste(Q[M], " = ", .(formatC(training_incidence$QM, digits=2, format="f")), ", df = ", .(training_incidence$p - 1),
                                           ", p = ", .(formatC(training_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-25, 0.5, pos=4, cex=0.95, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(training_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(training_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.95, font=2)

### add text for the subgroups
text(-25, c(15,19), pos=4, 
     c("Season",
       "Tournament")
)



### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
train_incidence_season <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (format == "season"), data = overall_train, method ="ML", slab= reference)
train_incidence_tournament <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (format == "tournament"), data = overall_train, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(train_incidence_season, row=2, cex=1, transf = exp, mlab="", digits = 1)
addpoly(train_incidence_tournament, row= 16, cex=1, transf = exp, mlab="", digits = 1)


dev.off()



# OVERALL TRAINING BURDEN and Severity ---------------------------------------------------------------------------------

# Not possible to produce these due to no studies with tournament format




# TOTAL INJURY INCIDENCE -------------------------------------------------------------------------

all_injury <-
  df1 %>%
  filter(activity == "overall") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

all_injury <- 
  all_injury %>%
  group_by(reference, format, sport) %>%
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


png(filename= "figures/Format_Forest5.png", width=8.27, height=11.69, units ="in", res = 400)


all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = all_injury,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


summary(all_incidence)
coef(all_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  all_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-30, 40),
  xlab = "Total incidence rate (events per 1,000 h)",
  mlab = "",
  order = order(all_injury$format),
  ylim=c(-2, 32),
  rows=c(3:23, 26:27),
  psize = 1,
  refline = exp(all_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Total incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences
all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ format,
    data = all_injury,
    method = "ML",
    slab = reference
  )

summary(all_incidence)

### add text for the test of subgroup differences
text(-30, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

text(-30, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(all_incidence$QM, digits=2, format="f")), ", df = ", .(all_incidence$p - 1),
                                             ", p = ", .(formatC(all_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-30, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(all_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(all_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-30, c(24,28), pos=4, 
     c("Season",
       "Tournament")
)


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
all_incidence_season <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (format == "season"),
                                data = all_injury, method ="ML", slab= reference)

all_incidence_tournament<- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (format == "tournament"),
                                data = all_injury, method ="ML", slab= reference)



### add summary polygons for the three subgroups
addpoly(all_incidence_season, row=2, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(all_incidence_tournament, row= 25, cex=0.8, transf = exp, mlab="", digits = 1)


dev.off()


# TOTAL INJURY BURDEN ------------------------------------------------------------------------------------

png(filename= "figures/Format_Forest6.png", width=8.27, height=10, units ="in", res = 400)

all_injury_bur <-
  all_injury %>% filter(
    total_days_missed > 0)


all_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    data = all_injury_bur,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, total_days_missed, sep = " | ")
  )


summary(all_burden)
coef(all_burden)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  all_burden,
  cex = 0.8,
  transf = exp,
  digits = 0,
  xlim=c(-200, 300),
  xlab = "Total burden rate (days missed per 1,000 h)",
  mlab = "",
  order = order(all_injury_bur$format),
  ylim=c(-2, 17),
  rows=c(3:10, 13),
  psize = 1,
  refline = exp(all_burden$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total days missed", "Total burden rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences

all_burden <-
  rma.glmm(
    measure = "IRLN",
    xi = total_days_missed,
    ti = exposure,
    mods = ~ format,
    data = all_injury_bur,
    method = "ML",
    slab = reference
  )
summary(all_burden)
# 
### add text for the test of subgroup differences
text(-200, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
# 
text(-200, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(all_burden$QM, digits=2, format="f")), ", df = ", .(all_burden$p - 1),
                                              ", p = ", .(formatC(all_burden$QMp, digits=4, format="f")))))

# Heterogeneity
text(-200, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(all_burden$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(all_burden$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-200, c(11,14), pos=4, 
     c("Season",
       "Tournament")
)



### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

all_burden_above <- rma.glmm(measure= "IRLN", xi= total_days_missed, ti= exposure, subset = (format == "season"), data = all_injury_bur, method ="ML", slab= reference)

addpoly(all_burden_above, row= 2, cex=0.8, transf = exp, mlab="", digits = 0)


dev.off()


# TOTAL INJURY SEVERITY -----------------------------------------------------------------------------------


png(filename= "figures/Format_forest7.png", width=8.27, height=10, units ="in", res = 400)


all_sev <-
  all_injury %>%
  filter(total_days_missed > 0) %>%
  mutate(mean_days_missed = (total_days_missed / injury_count),
         mean_days_missed = round(mean_days_missed, 0),
         exposure = exposure / 1000, 
         days_missed_lcl = mean_days_missed / (exp(1.96 / sqrt(injury_count))),
         days_missed_ucl = mean_days_missed * (exp(1.96 / sqrt(injury_count))),
         days_missed_se = (days_missed_ucl - days_missed_lcl)/3.92)


### Model

days_missed_model <-
  rma.mv(
    mean_days_missed,
    days_missed_se,
    random = ~ 1 | reference,
    data = all_sev,
    slab = paste(reference, sport, injury_count, sep = " | ")
  )

summary(days_missed_model)

forest(
  days_missed_model,
  order = order(all_sev$format),
  cex = 0.8,
  xlab = "Mean days missed",
  digits = 0,
  mlab = "",
  xlim = c(-40, 40),
  ylim=c(-2, 17),
  rows=c(3:10, 13),
  psize = 1,
  refline = days_missed_model$beta,
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Mean days missed [95% CI]")
)

### fit meta-regression model to test for subgroup differences
days_missed_model <-
  rma.mv(
    mean_days_missed,
    mods = ~ format,
    days_missed_se,
    random = ~ 1 | reference,
    data = all_sev,
    slab = paste(reference, sport, sep = " | ")
  )
summary(days_missed_model)
# 
# ### add text for the test of subgroup differences
text(-40, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))
# 
text(-40, -1.2, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(days_missed_model$QM, digits=2, format="f")), ", df = ", .(days_missed_model$p - 1),
                                             ", p = ", .(formatC(days_missed_model$QMp, digits=4, format="f")))))
# 

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-40, c(11,14), pos=4, 
     c("Season",
       "Tournament")
)

## switch to bold font
par(font=2)

### set par back to the original settings
par(op)



## fit random-effects model in soccer subgroups
days_missed_soccer <- rma.mv(
  mean_days_missed,
  days_missed_se,
  subset = (format == "season"),
  random = ~ 1 | reference,
  data = all_sev,
  slab = paste(reference, sport, sep = " | ")
)

addpoly(days_missed_soccer, row=2, cex = 0.8, mlab="", digits = 0)


dev.off()



# MODELS ASSESSING RECORDING METHOD --------------------------------------------------------


# TOTAL INJURY INCIDENCE -------------------------------------------------------------------------

all_injury <-
  df1 %>%
  filter(activity == "overall") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

all_injury <- 
  all_injury %>%
  group_by(reference, reporting_method, sport) %>%
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


png(filename= "figures/Method_all_inc.png", width=8.27, height=11.69, units ="in", res = 400)


all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = all_injury,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


summary(all_incidence)
coef(all_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  all_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-30, 40),
  xlab = "Total incidence rate (events per 1,000 h)",
  mlab = "",
  order = order(all_injury$reporting_method),
  ylim=c(-2, 36),
  rows=c(3:10, 13:15, 18:21, 24:31),
  psize = 1,
  refline = exp(all_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Total incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences
all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ reporting_method,
    data = all_injury,
    method = "ML",
    slab = reference
  )

summary(all_incidence)

### add text for the test of subgroup differences
text(-30, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

text(-30, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(all_incidence$QM, digits=2, format="f")), ", df = ", .(all_incidence$p - 1),
                                             ", p = ", .(formatC(all_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-30, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(all_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(all_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-30, c(11,16,22,32), pos=4, 
     c("Medical",
       "Mixed",
       "Non-Medical",
       "Self-Reported")
)

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
medical <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (reporting_method == "medical"),
                    data = all_injury, method ="ML", slab= reference)

mixed <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (reporting_method == "mixed"),
                  data = all_injury, method ="ML", slab= reference)

non_medical <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (reporting_method == "non-medical"),
                        data = all_injury, method ="ML", slab= reference)

self_rep <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (reporting_method == "self-reported"),
                     data = all_injury, method ="ML", slab= reference)


### add summary polygons for the three subgroups
addpoly(medical, row=2, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(mixed, row= 12, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(non_medical, row= 17, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(self_rep, row= 23, cex=0.8, transf = exp, mlab="", digits = 1)


dev.off()



# OVERALL MATCH INCIDENCE ---------------------------------------------------------------------


match_data <-
  df1 %>%
  filter(activity == "match") %>%
  filter(!is.na(exposure)) %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

overall_match <- 
  match_data %>%
  group_by(reference, reporting_method, sport) %>% # setting removed - not needed in this plot
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


png(filename= "figures/Method_match_inc.png", width=8.27, height=11.69, units ="in", res = 400)


match_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_match,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


# FOREST

forest(
  match_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-60, 100),
  xlab = "Match incidence rate (events per 1,000 match h)",
  mlab = "",
  order = order(overall_match$reporting_method),
  ylim=c(-2, 32),
  rows=c(3:13, 16, 19:22, 25:27),
  psize = 1,
  refline = exp(match_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Match incidence rate [95% CI]")
)


match_rec <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ reporting_method,
    data = overall_match,
    method = "ML", # random effects
    slab = reference
  )

summary(match_rec)



### add text for the test of subgroup differences
text(-60, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

text(-60, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(match_rec$QM, digits=2, format="f")), ", df = ", .(match_rec$p - 1),
                                             ", p = ", .(formatC(match_rec$QMp, digits=4, format="f")))))

# Heterogeneity
text(-60, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(match_rec$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(match_rec$tau2, digits=2, format="f")))))


### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-60, c(14,17,23,28), pos=4, cex = 1, 
     c("Medical",
       "Mixed",
       "Non-Medical",
       "Self-Reported")
)


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
medical <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (reporting_method == "medical"), data = overall_match, method ="ML", slab= reference)

non_medical <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (reporting_method == "non-medical"), data = overall_match, method ="ML", slab= reference)

self_reported <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (reporting_method == "self-reported"), data = overall_match, method ="ML", slab= reference)

addpoly(medical, row= 2, cex=0.8, transf = exp, mlab="", digits = 1)

addpoly(non_medical, row= 18, cex=0.8, transf = exp, mlab="", digits = 1)

addpoly(self_reported, row= 24, cex=0.8, transf = exp, mlab="", digits = 1)

dev.off()



# OVERALL TRAINING INCIDENCE ----------------------------------------------------------------------


training_data <-
  df1 %>%
  filter(activity == "training") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

overall_train <- 
  training_data %>%
  group_by(reference, reporting_method, sport) %>% # setting removed - not needed in this plot
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


png(filename= "figures/Method_train_inc.png", width=8.27, height=11.69, units ="in", res = 400)



training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_train,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


summary(training_incidence)
coef(training_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  training_incidence,
  cex = 1,
  transf = exp,
  digits = 1,
  xlim=c(-25, 18),
  xlab = "Training incidence rate (events per 1,000 training h)",
  mlab = "",
  order = order(overall_train$reporting_method),
  ylim=c(-2, 27),
  rows=c(3:7, 10, 13:16, 19:22),
  psize = 1,
  refline = exp(training_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Training incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences

training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ reporting_method,
    data = overall_train,
    method = "ML",
    slab = reference
  )
summary(training_incidence)

### add text for the test of subgroup differences
text(-25, -0.8, pos=4, cex=1, bquote(paste("Test for Subgroup Differences: ")))
text(-25, -1.8, pos=4, cex=1, bquote(paste(Q[M], " = ", .(formatC(training_incidence$QM, digits=2, format="f")), ", df = ", .(training_incidence$p - 1),
                                           ", p = ", .(formatC(training_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-25, 0.5, pos=4, cex=0.95, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(training_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(training_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.95, font=2)

### add text for the subgroups
text(-25, c(8,11,17,23), pos=4, 
     c("Medical",
       "Mixed",
       "Non-Medical",
       "Self-Reported")
)

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
medical <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (reporting_method == "medical"), data = overall_train, method ="ML", slab= reference)
non_medical <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (reporting_method == "non-medical"), data = overall_train, method ="ML", slab= reference)
self_rep <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (reporting_method == "self-reported"), data = overall_train, method ="ML", slab= reference)


### add summary polygons for the three subgroups
addpoly(medical, row=2, cex=1, transf = exp, mlab="", digits = 1)
addpoly(non_medical, row= 12, cex=1, transf = exp, mlab="", digits = 1)
addpoly(self_rep, row= 18, cex=1, transf = exp, mlab="", digits = 1)


dev.off()





# MODELS ASSESSING COMPETITIVE LEVEL --------------------------------------------------------


# TOTAL INJURY INCIDENCE -------------------------------------------------------------------------

all_injury <-
  df1 %>%
  filter(activity == "overall") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

all_injury <- 
  all_injury %>%
  group_by(reference, level2, sport) %>%
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


png(filename= "figures/Supplementary Fig. 2 .png", width=8.27, height=11.69, units ="in", res = 800)


all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = all_injury,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


summary(all_incidence)
coef(all_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  all_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-30, 40),
  xlab = "Total incidence rate (events per 1,000 h)",
  mlab = "",
  order = order(all_injury$level2),
  ylim=c(-2, 32),
  rows=c(3:12, 15:28),
  psize = 1,
  refline = exp(all_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Total incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic

### fit meta-regression model to test for subgroup differences
all_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ level2,
    data = all_injury,
    method = "ML",
    slab = reference
  )

summary(all_incidence)

### add text for the test of subgroup differences
text(-30, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

text(-30, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(all_incidence$QM, digits=2, format="f")), ", df = ", .(all_incidence$p - 1),
                                             ", p = ", .(formatC(all_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-30, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(all_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(all_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-30, c(13,29), pos=4, 
     c("Elite",
       "Non-Elite")
)

### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
elite <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (level2 == "elite"),
                    data = all_injury, method ="ML", slab= reference)

non_elite <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (level2 == "non-elite"),
                  data = all_injury, method ="ML", slab= reference)



### add summary polygons for the three subgroups
addpoly(elite, row=2, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(non_elite, row= 14, cex=0.8, transf = exp, mlab="", digits = 1)


dev.off()



# OVERALL MATCH INCIDENCE ---------------------------------------------------------------------


match_data <-
  df1 %>%
  filter(activity == "match") %>%
  filter(!is.na(exposure)) %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

overall_match <- 
  match_data %>%
  group_by(reference, level2, sport) %>% # setting removed - not needed in this plot
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


png(filename= "figures/Supplementary Fig. 3.png", width=8.27, height=11.69, units ="in", res = 800)


match_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_match,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


# FOREST

forest(
  match_incidence,
  cex = 0.8,
  transf = exp,
  digits = 1,
  xlim=c(-60, 100),
  xlab = "Match incidence rate (events per 1,000 match h)",
  mlab = "",
  order = order(overall_match$level2),
  ylim=c(-2, 28),
  rows=c(3:11, 14:24),
  psize = 1,
  refline = exp(match_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Match incidence rate [95% CI]")
)


match_rec <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ level2,
    data = overall_match,
    method = "ML", # random effects
    slab = reference
  )

summary(match_rec)



### add text for the test of subgroup differences
text(-60, -0.8, pos=4, cex=0.8, bquote(paste("Test for Subgroup Differences: ")))

text(-60, -1.8, pos=4, cex=0.8, bquote(paste(Q[M], " = ", .(formatC(match_rec$QM, digits=2, format="f")), ", df = ", .(match_rec$p - 1),
                                             ", p = ", .(formatC(match_rec$QMp, digits=4, format="f")))))

# Heterogeneity
text(-60, 0.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(match_rec$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(match_rec$tau2, digits=2, format="f")))))


### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=2)

### add text for the subgroups

text(-60, c(12,25), pos=4, cex = 1, 
     c("Elite",
       "Non-Elite")
)


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
pol1 <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (level2 == "elite"), data = overall_match, method ="ML", slab= reference)
pol2 <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (level2 == "non-elite"), data = overall_match, method ="ML", slab= reference)


addpoly(pol1, row= 2, cex=0.8, transf = exp, mlab="", digits = 1)
addpoly(pol2, row= 13, cex=0.8, transf = exp, mlab="", digits = 1)

dev.off()



# OVERALL TRAINING INCIDENCE ----------------------------------------------------------------------


training_data <-
  df1 %>%
  filter(activity == "training") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(injury_count = round(injury_count, 0)) %>% 
  mutate(exposure = exposure / 1000) %>%
  filter(injury_count > 0 &
           exposure > 0)

overall_train <- 
  training_data %>%
  group_by(reference, level2, sport) %>% # setting removed - not needed in this plot
  dplyr::summarise(injury_count = sum(injury_count),
                   exposure = sum(exposure),
                   total_days_missed = sum(total_days_missed))    # this is to remove studies with multiple rows of data


png(filename= "figures/Supplementary Fig. 4.png", width=8.27, height=11.69, units ="in", res = 800)



training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    data = overall_train,
    method = "ML",
    model = "UM.RS",
    slab = paste(reference, sport, injury_count, sep = " | ")
  )


summary(training_incidence)
coef(training_incidence)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)

forest(
  training_incidence,
  cex = 1,
  transf = exp,
  digits = 1,
  xlim=c(-25, 18),
  xlab = "Training incidence rate (events per 1,000 training h)",
  mlab = "",
  order = order(overall_train$level2),
  ylim=c(-2, 23),
  rows=c(3:10, 13:19),
  psize = 1,
  refline = exp(training_incidence$beta),
  addcred = T,
  header = c("Study Reference | Sport | Total injury count", "Training incidence rate [95% CI]")
)

### add text with Q-value, dfs, p-value, and I^2 statistic
### fit meta-regression model to test for subgroup differences

training_incidence <-
  rma.glmm(
    measure = "IRLN",
    xi = injury_count,
    ti = exposure,
    mods = ~ level2,
    data = overall_train,
    method = "ML",
    slab = reference
  )
summary(training_incidence)

### add text for the test of subgroup differences
text(-25, -0.8, pos=4, cex=1, bquote(paste("Test for Subgroup Differences: ")))
text(-25, -1.8, pos=4, cex=1, bquote(paste(Q[M], " = ", .(formatC(training_incidence$QM, digits=2, format="f")), ", df = ", .(training_incidence$p - 1),
                                           ", p = ", .(formatC(training_incidence$QMp, digits=4, format="f")))))

# Heterogeneity
text(-25, 0.5, pos=4, cex=0.95, bquote(
  paste("RE Model for All Studies: ", I^2, " = ",
        .(formatC(training_incidence$I2, digits=1, format="f")), 
        "%", "; ", tau^2 == 
          .(formatC(training_incidence$tau2, digits=2, format="f")))))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.95, font=2)

### add text for the subgroups
text(-25, c(11,20), pos=4, 
     c("Elite",
       "Non-Elite")
)


### switch to bold font
par(font=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
pol1 <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (level2 == "elite"), data = overall_train, method ="ML", slab= reference)
pol2 <- rma.glmm(measure= "IRLN", xi= injury_count, ti= exposure, subset = (level2 == "non-elite"), data = overall_train, method ="ML", slab= reference)

### add summary polygons for the three subgroups
addpoly(pol1, row=2, cex=1, transf = exp, mlab="", digits = 1)
addpoly(pol2, row= 12, cex=1, transf = exp, mlab="", digits = 1)


dev.off()
