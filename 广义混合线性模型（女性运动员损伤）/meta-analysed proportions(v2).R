


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



# Injury Categories -------------------------------------------------------
#Load data

############################################################################
# INJURY Prevelance
############################################################################


injury_prevelance <- read_excel("data/proportions.xlsx", sheet = "Injury Prevelance")


df <-
  injury_prevelance %>%
  mutate(prop = ((injured_athletes / injury_count) * 100),
         prop_lcl = prop / (exp(1.96 / sqrt(injury_count))),
         prop_ucl = prop * (exp(1.96 / sqrt(injury_count))),
         prop_se = (prop_ucl - prop_lcl)/3.92) %>%
  mutate_if(is.character, as.factor) %>%
  filter(injured_athletes > 0)



(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df))



prop <- tidy((model1), conf.int = TRUE) %>%
  select(3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df$reference))


dat <-
  df %>%
  mutate(
    study_no = study_number) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count),
    total_injured_athletes = sum(injured_athletes)) %>%
  mutate_if(is.numeric, round, digits = 0)


injury_prev <- cbind.data.frame(dat, prop)



############################################################################
# INJURY LOCATION
############################################################################





injury_location <- read_excel("data/proportions.xlsx", sheet = "Injury Location")


df <-
  injury_location %>%
  mutate(prop = ((injury_count / total_count) * 100),
         prop_lcl = prop / (exp(1.96 / sqrt(total_count))),
         prop_ucl = prop * (exp(1.96 / sqrt(total_count))),
         prop_se = (prop_ucl - prop_lcl)/3.92) %>%
  mutate_if(is.character, as.factor) %>%
  filter(injury_count > 0)


# Head & Neck ------

rep_neck <- 
  c("cervical spine" = "neck")

HN <-
  df %>%
  filter(location == "head & neck") %>%
  dplyr::select(1:3, 5, 4, 6:10)
  

HN2 <-
  df %>%
  filter(reference != "Barden et al., 2021",
         reference != "Beech et al., 2022") %>%
  mutate(
    location = str_replace_all(location, rep_neck)) %>%
  filter(location == "head" | location == "neck") %>%
  mutate(location = "head & neck") %>%
  group_by(reference, sport, activity, location) %>%
  dplyr::summarise(total_count = mean(total_count),
                   injury_count = sum(injury_count)) %>%
  mutate(prop = ((injury_count / total_count) * 100),
         prop_lcl = prop / (exp(1.96 / sqrt(total_count))),
         prop_ucl = prop * (exp(1.96 / sqrt(total_count))),
         prop_se = (prop_ucl - prop_lcl)/3.92) %>%
  mutate_if(is.character, as.factor) %>%
  filter(injury_count > 0)



df1 <- rbind.data.frame(HN, HN2)


(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))



prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "head & neck") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


head_neck <- left_join(dat, prop, by = c("location"))




# Head/face -------------

df1 <-
  df %>%
  filter(location == "head")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "head") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


head <- left_join(dat, prop, by = c("location"))




# Neck -------------

df1 <-
  df %>%
  mutate(
    location = str_replace_all(location, rep_neck)) %>%
  filter(location == "neck")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "neck") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


neck <- left_join(dat, prop, by = c("location"))


# Upper limb ----


rep_upper <- 
  c("arm" = "upper limb",
    "hand" = "upper limb",
    "elbow" = "upper limb",
    "wrist" = "upper limb",
    "shoulder" = "upper limb",
    "shoulder/clavicle" = "upper limb")

UL <-
  df %>%
  filter(location == "upper limb") %>%
  dplyr::select(1:3, 5, 4, 6:10)


UL2 <-
  df %>%
  filter(reference != "Barden et al., 2021",
         reference != "Beech et al., 2022",
         reference != "Hjelm et al., 2012") %>%
  mutate(
    location = str_replace_all(location, rep_upper)) %>%
   filter(location == "upper limb") %>%
   group_by(reference, sport, activity, location) %>%
   dplyr::summarise(total_count = mean(total_count),
                    injury_count = sum(injury_count)) %>%
   mutate(prop = ((injury_count / total_count) * 100),
          prop_lcl = prop / (exp(1.96 / sqrt(total_count))),
          prop_ucl = prop * (exp(1.96 / sqrt(total_count))),
          prop_se = (prop_ucl - prop_lcl)/3.92) %>%
   mutate_if(is.character, as.factor) %>%
   filter(injury_count > 0)
 


df1 <- rbind.data.frame(UL, UL2)


(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))


prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "upper limb") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


upper_limb <- left_join(dat, prop, by = c("location"))


# Shoulder Injuries -------------

rep_shoulder <- 
  c("shoulder/clavicle" = "shoulder")

df1 <-
  df %>%
  mutate(
  location = str_replace_all(location, rep_shoulder)) %>%
  filter(location == "shoulder")
  

(model1 <-
    rma.mv(
      prop,
      prop_se,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "shoulder") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


shoulder <- left_join(dat, prop, by = c("location"))



# Elbow Injuries -------------


df1 <-
  df %>%
  filter(location == "elbow" | location == "upper arm, elbow") %>%
  mutate(location = "elbow")


(model1 <-
    rma.mv(
      prop,
      prop_se,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "elbow") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


elbow <- left_join(dat, prop, by = c("location"))


# Wrist Injuries -------------


df1 <-
  df %>%
  filter(location == "wrist")


(model1 <-
    rma.mv(
      prop,
      prop_se,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "wrist") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


wrist <- left_join(dat, prop, by = c("location"))

# Hand Injuries -------------


df1 <-
  df %>%
  filter(location == "hand")


(model1 <-
    rma.mv(
      prop,
      prop_se,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "hand") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)

study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


hand <- left_join(dat, prop, by = c("location"))

# Trunk -----

t <-
  df %>%
  filter(location == "trunk") %>%
dplyr::select(1:3, 5, 4, 6:10)


t2 <-
  df %>%
  filter(reference != "Barden et al., 2021",
         reference != "Beech et al., 2022",
         reference != "Hjelm et al., 2012",
         reference != "Mann et al., 2021",
         reference != "Le Gall et a., 2008") %>%
  filter(location == "upper back" | 
           location == "upper back/neck" |
           location == "thoracic spine" |
           location == "chest" |
           location == "sternum/ribs/upper back" |
           location == "lower back" | 
           location == "lower back/pelvis/sacrum" |
           location == "lower back/sacrum" |
           location == "pelvis" |
           location == "spine" |
           location == "lumbar, pelvis, sacrum" |
           location == "lumbar spine" |
           location == "lumbosacral" |
           location == "abdomen") %>%
  mutate(location = "trunk") %>%
  group_by(reference, sport, activity, location) %>%
  dplyr::summarise(total_count = mean(total_count),
                   injury_count = sum(injury_count)) %>%
  mutate(prop = ((injury_count / total_count) * 100),
         prop_lcl = prop / (exp(1.96 / sqrt(total_count))),
         prop_ucl = prop * (exp(1.96 / sqrt(total_count))),
         prop_se = (prop_ucl - prop_lcl)/3.92) %>%
  mutate_if(is.character, as.factor) %>%
  filter(injury_count > 0)



df1 <- rbind.data.frame(t, t2)


(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))


prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "trunk") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)

study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


trunk <- left_join(dat, prop, by = c("location"))




# Chest/upper back ----


df1 <-
  df %>%
  filter(location == "upper back" | 
           location == "upper back/neck" |
           location == "thoracic spine" |
           location == "chest" |
           location == "sternum/ribs/upper back") %>%
  mutate(location = "upper back/chest")


(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))


prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "upper back/chest") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


upper_back <- left_join(dat, prop, by = c("location"))


# Lower back ----


df1 <-
  df %>%
  filter(location == "lower back" | 
           location == "lower back/pelvis/sacrum" |
           location == "lower back/sacrum" |
           location == "lumbar, pelvis, sacrum" |
           location == "pelvis" |
           location == "lumbar spine" |
           location == "lumbosacral") %>%
  mutate(location = "lower back")


(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))


prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "lower back") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


lower_back <- left_join(dat, prop, by = c("location"))




# Abdomen Injuries -------------

df1 <-
  df %>%
  filter(location == "abdomen")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "abdomen") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


abdomen <- left_join(dat, prop, by = c("location"))



# Lower limb ----

low_limb <-
  df %>%
  filter(location == "lower limb") %>%
  dplyr::select(1:3, 5, 4, 6:10 )# papers that have region present

  
  
low_limb2 <- # remove references that have region present
  df %>%
  filter(reference != "Barden et al., 2021",
         reference != "Beech et al., 2022",
         reference != "Hjelm et al., 2012",
         reference != "Mann et al., 2021",
         reference != "Steffen et al., 2008",
         )


low_limb2$location <- gsub('\\s+', '', low_limb2$location)

rep_thigh <- 
  c("posteriorthigh" = "thigh",
    "anteriorthigh" = "thigh")

rep_low_limb <-
  c("hip/groin" = "lower limb",
    "thigh" = "lower limb",
    "knee" = "lower limb",
    "hip,groin,thigh" = "lower limb",
    "knee,lowerleg," = "lower limb",
    "achillestendon,ankle,foot/toe" = "lower limb",
    "leg" = "lower limb",
    "lowerleg" = "lower limb",
    "ankle" = "lower limb",
    "foot" = "lower limb")

low_limb2 <-
  low_limb2 %>%
  mutate(
    location = str_replace_all(location, rep_thigh),
     location = str_replace_all(location, rep_low_limb)) %>%
  filter(location == "lower limb") %>%
  group_by(reference, sport, activity, location) %>%
  dplyr::summarise(total_count = mean(total_count),
                  injury_count = sum(injury_count)) %>%
  mutate(prop = ((injury_count / total_count) * 100),
         prop_lcl = prop / (exp(1.96 / sqrt(total_count))),
         prop_ucl = prop * (exp(1.96 / sqrt(total_count))),
         prop_se = (prop_ucl - prop_lcl)/3.92) %>%
  mutate_if(is.character, as.factor) %>%
  filter(injury_count > 0)

df1 <- rbind.data.frame(low_limb, low_limb2)


(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))


prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "lower limb") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)

low_limb_data <- left_join(dat, prop, by = c("location"))


# Hip/Groin ----

df1 <-
  df %>%
  filter(location == "hip/groin")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "hip/groin") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)

study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


hip_groin <- left_join(dat, prop, by = c("location"))





# Anterior Thigh ----

df1 <-
  df %>%
  filter(location == "anterior thigh")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "anterior thigh") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)

study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


ant_thigh <- left_join(dat, prop, by = c("location"))


# Posterior Thigh ----

df1 <-
  df %>%
  filter(location == "posterior thigh")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "posterior thigh") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


post_thigh <- left_join(dat, prop, by = c("location"))

# Thigh --------------------


rep_thigh <- 
  c("posterior thigh" = "thigh",
    "anterior thigh" = "thigh")


df1 <-
  df %>%
  mutate(
    location = str_replace_all(location, rep_thigh)) %>%
   filter(location == "thigh") %>%
  group_by(reference, location) %>%
  dplyr::summarise(total_count = mean(total_count),
                   injury_count = sum(injury_count))


df1 <-
  df1 %>%
  mutate(prop = ((injury_count / total_count) * 100),
         prop_lcl = prop / (exp(1.96 / sqrt(total_count))),
         prop_ucl = prop * (exp(1.96 / sqrt(total_count))),
         prop_se = (prop_ucl - prop_lcl)/3.92)


(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "thigh") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)

study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


thigh <- left_join(dat, prop, by = c("location"))


# Knee Injuries -------------

df1 <-
  df %>%
  filter(location == "knee")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "knee") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


knee <- left_join(dat, prop, by = c("location"))


# Lower leg Injuries -------------

df1 <-
  df %>%
  filter(location == "lower leg")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "lower leg") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


lower_leg <- left_join(dat, prop, by = c("location"))

# Ankle Injuries -------------

df1 <-
  df %>%
  filter(location == "ankle")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "ankle") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


ankle <- left_join(dat, prop, by = c("location"))



# Foot -------------

df1 <-
  df %>%
  filter(location == "foot")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(location = "foot") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(location) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


foot <- left_join(dat, prop, by = c("location"))


# rbind df--------------------------

prop_data <- rbind.data.frame(head_neck,
                              head,
                              neck,
                              upper_limb,
                              shoulder,
                              elbow,
                              wrist,
                              hand,
                              trunk,
                              upper_back,
                              lower_back,
                              abdomen,
                              low_limb_data,
                              hip_groin,
                              thigh,
                              ant_thigh,
                              post_thigh,
                              knee,
                              lower_leg,
                              ankle,
                              foot)




region <-
  c("head & neck",
    "upper limb",
    "trunk",
    "lower limb")



prop_data_raw <-
  prop_data %>%
  mutate(group = case_when(location %in% region ~ "region",
                            TRUE ~"location"))


prop_data_raw$variable <- paste0(prop_data_raw$location, " | ", prop_data_raw$total_injury_no, " (",prop_data_raw$study_no, ")")

prop_data_raw$variable<-str_to_title(prop_data_raw$variable)

region_location_data <-
  prop_data_raw %>%
  select(7,8,4:6)


prop_data$prop_CI <- paste0(prop_data$estimate, " (", prop_data$conf.low, " - ",prop_data$conf.high, ")")


prop_data$location<-str_to_title(prop_data$location)

loc_data <- 
  prop_data %>%
  dplyr::select(1,2,3,7) %>%
  dplyr::rename(
    "Injury location" = location,
    "Number of studies" = study_no,
    "Total injury count" = total_injury_no,
    "Meta-analysed proportion (95% CI)" = prop_CI)


############################################################################
# INJURY TYPE
############################################################################

type <- read_excel("data/proportions.xlsx", sheet = "Injury Type")

type2 <-
  type %>%
  filter(injury_count > 0)


type2$type <-tolower(type2$type)

# TISSUE TYPE GROUPS ----

muscle_tendon <- 
  c(
    "muscle/tendon",
    "muscle/tendon strain",
    "strain",
    "strains",
    "tendinopathy",
    "tendinitis",
    "tendinosis",
    "muscle-tendon strain, incomplete tear",
    "muscle / tendon strain",
    "tendonitis/tenosynovitis",
    "labral tear",
    "muscle strain",
    "tendonitis/tenosynovitis",
    "impingement/ synovitis",
    "tendon",
    "muscle and tendon Strain/ spasm/ trigger points")

nervous <-
  c(
    "cns/pns",
    "concussion",
    "concusion")


bone <-
  c(
    "fracture",
    "fractures",
    "apophysitis",
    "fracture - stress",
    "frature - stress",
    "stress fracture",
    "fracture - acute",
    "acute fracture",
    "stress fracture",
    "bone/fracture",
    "other bone injury")

cartilage_synovium_bursa <-
  c(
    "meniscal",
    "meniscus tear",
    "osteochrondoses",
    "chondral")

ligament <-
  c(
    "sprain",
    "sprains",
    "ligament sprains",
    "ligament sprain",
    "ligament sprain, incomplete tear",
    "dislocation",
    "dislocation / subluxation",
    "subluxation",
    "ligament/joints",
    "luxation",
    "sprain/ligament injury")

superficial <-
  c(
    "contusion",
    "haematoma/contusion/bruise",
    "laceration",
    "contusion/laceration/bruising",
    "abrasion")

type2 <- 
  type2 %>% 
  mutate(tissue = case_when(type %in% muscle_tendon ~ "muscle/tendon",
                            type %in% nervous ~ "nervous",
                            type %in% bone ~ "bone",
                            type %in% cartilage_synovium_bursa ~ "cartilage/synovium/bursa",
                            type %in% ligament ~ "ligament/joint capsule",
                            type %in% superficial ~ "superficial tissues/skin",
                            TRUE ~"Not specified / other"))


df <-
  type2 %>%
  group_by(reference, sport, activity, tissue) %>%
  dplyr::summarise(total_count = mean(total_count),
                   injury_count = sum(injury_count)) %>%
  mutate(prop = ((injury_count / total_count) * 100),
         prop_lcl = prop / (exp(1.96 / sqrt(total_count))),
         prop_ucl = prop * (exp(1.96 / sqrt(total_count))),
         prop_se = (prop_ucl - prop_lcl)/3.92) %>%
  mutate_if(is.character, as.factor)

# muscle/tendon ----

df1 <-
  df %>%
  filter(tissue == "muscle/tendon")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(tissue = "muscle/tendon") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(tissue) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


muscle_tendon_prop <- left_join(dat, prop, by = c("tissue"))


# nervous ----

df1 <-
  df %>%
  filter(tissue == "nervous")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(tissue = "nervous") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(tissue) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


nervous_prop <- left_join(dat, prop, by = c("tissue"))



# bone ----

df1 <-
  df %>%
  filter(tissue == "bone")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(tissue = "bone") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(tissue) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


bone_prop <- left_join(dat, prop, by = c("tissue"))


# cartilage/synovium/bursa ----

df1 <-
  df %>%
  filter(tissue == "cartilage/synovium/bursa")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(tissue = "cartilage/synovium/bursa") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(tissue) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


cartilage_prop <- left_join(dat, prop, by = c("tissue"))



# ligament/joint capsule ----

df1 <-
  df %>%
  filter(tissue == "ligament/joint capsule")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(tissue = "ligament/joint capsule") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(tissue) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


ligament_prop <- left_join(dat, prop, by = c("tissue"))


# superficial tissues/skin ----

df1 <-
  df %>%
  filter(tissue == "superficial tissues/skin")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(tissue = "superficial tissues/skin") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(tissue) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


superficial_prop <- left_join(dat, prop, by = c("tissue"))


# Not specified / other----

df1 <-
  df %>%
  filter(tissue == "Not specified / other") %>%
  filter(reference != "Jacobsson et al. 2013", reference != "Pluim 2016")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(tissue = "Not specified / other") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)

study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(tissue) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


other_prop <- left_join(dat, prop, by = c("tissue"))

# rbind df--------------------------

prop_data <- rbind.data.frame(muscle_tendon_prop,
                              nervous_prop,
                              bone_prop,
                              cartilage_prop,
                              ligament_prop,
                              superficial_prop,
                              other_prop)


prop_data_raw <-
  prop_data %>%
  mutate(group = "tissue_type")


prop_data_raw$variable <- paste0(prop_data_raw$tissue, " | ", prop_data_raw$total_injury_no, " (",prop_data_raw$study_no, ")")

prop_data_raw$variable<-str_to_title(prop_data_raw$variable)

tissue_type_data <-
  prop_data_raw %>%
  select(7,8,4:6)


prop_data$prop_CI <- paste0(prop_data$estimate, " (", prop_data$conf.low, " - ",prop_data$conf.high, ")")


prop_data$tissue<-str_to_title(prop_data$tissue)

type_data <- 
  prop_data %>%
  dplyr::select(1,2,3,7) %>%
  dplyr::rename(
    "Injury Type" = tissue,
    "Number of studies" = study_no,
    "Total injury count" = total_injury_no,
    "Meta-analysed proportion (95% CI)" = prop_CI)



############################################################################
# INJURY SEVERITY TIME BINS
############################################################################

sev_cat <- read_excel("data/proportions.xlsx", sheet = "Severity Time Bins")

sev_cat1 <-
  sev_cat %>%
  filter(injury_count > 0)


sev_cat1$severity_time_bin <-tolower(sev_cat1$severity_time_bin)

# DEFINE SEV TIME BINS -----

minor <- 
  c(
    "1 day",
    "1 days",
    "2-3 days",
    "1-3 days",
    "4-7 days",
    "2-7 days",
    "1-7 days")

moderate <-
  c(
    "8-28 days")


severe <-
  c(
    ">28 days",
    "> 28 days")



sev_cat1 <- 
  sev_cat1 %>% 
  mutate(time_bin = case_when(severity_time_bin %in% minor ~ "minor (1-7 days)",
                             severity_time_bin %in% moderate ~ "moderate (8-28 days)",
                             severity_time_bin %in% severe ~ "severe (>28 days)",
                             TRUE ~"incompatible"))


df <-
  sev_cat1 %>%
  group_by(reference, sport, activity, time_bin) %>%
  dplyr::summarise(total_count = mean(total_count),
                   injury_count = sum(injury_count)) %>%
  mutate(prop = ((injury_count / total_count) * 100),
         prop_lcl = prop / (exp(1.96 / sqrt(total_count))),
         prop_ucl = prop * (exp(1.96 / sqrt(total_count))),
         prop_se = (prop_ucl - prop_lcl)/3.92) %>%
  mutate_if(is.character, as.factor)




# MINOR SEVERITY TIME BINS ----


df1 <-
  df %>%
  filter(time_bin == "minor (1-7 days)")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(time_bin = "minor (1-7 days)") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(time_bin) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


minor_prop <- left_join(dat, prop, by = c("time_bin"))


# MODERATE SEVERITY TIME BINS ----

df1 <-
  df %>%
  filter(time_bin == "moderate (8-28 days)")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(time_bin = "moderate (8-28 days)") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)

study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(time_bin) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


moderate_prop <- left_join(dat, prop, by = c("time_bin"))

# SEVERE SEVERITY TIME BINS ----


df1 <-
  df %>%
  filter(time_bin == "severe (>28 days)")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(time_bin = "severe (>28 days)") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(time_bin) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


severe_prop <- left_join(dat, prop, by = c("time_bin"))

# rbind df--------------------------


prop_data <- rbind.data.frame(minor_prop,
                              moderate_prop,
                              severe_prop)

prop_data_raw <-
  prop_data %>%
  mutate(group = "severity_time_bins")


prop_data_raw$variable <- paste0(prop_data$time_bin, " | ", prop_data_raw$total_injury_no, " (",prop_data_raw$study_no, ")")

prop_data_raw$variable<-str_to_title(prop_data_raw$variable)

severity_cat_data <-
  prop_data_raw %>%
  select(7,8,4:6)


prop_data$prop_CI <- paste0(prop_data$estimate, " (", prop_data$conf.low, " - ",prop_data$conf.high, ")")


prop_data$time_bin<-str_to_title(prop_data$time_bin)

time_bin_data <- 
  prop_data %>%
  dplyr::select(1,2,3,7) %>%
  dplyr::rename(
    "Injury Severity Time Bins" = time_bin,
    "Number of studies" = study_no,
    "Total injury count" = total_injury_no,
    "Meta-analysed proportion (95% CI)" = prop_CI)




############################################################################
# INJURY ONSET
############################################################################

inj_onset <- read_excel("data/proportions.xlsx", sheet = "Injury Onset")

inj_onset1 <-
  inj_onset %>%
  filter(injury_count > 0)


inj_onset1$injury_onset <-tolower(inj_onset1$injury_onset)


# Onset Cats

acute_onset <-
  c("traumatic",
    "acute",
    "impact/traumatic",
    "sudden onset no trauma",
    "sudden onset",
    "traumatic injury")


overuse_onset <-
  c("overuse",
    "gradual",
    "gradual onset")



inj_onset1 <- 
  inj_onset1 %>% 
  mutate(onset = case_when(injury_onset%in% acute_onset ~ "acute",
                              injury_onset %in% overuse_onset ~ "overuse",
                              TRUE ~"incompatible"))


df <-
  inj_onset1 %>%
  group_by(reference, sport, activity, onset) %>%
  dplyr::summarise(total_count = mean(total_count),
                   injury_count = sum(injury_count)) %>%
  mutate(prop = ((injury_count / total_count) * 100),
         prop_lcl = prop / (exp(1.96 / sqrt(total_count))),
         prop_ucl = prop * (exp(1.96 / sqrt(total_count))),
         prop_se = (prop_ucl - prop_lcl)/3.92) %>%
  mutate_if(is.character, as.factor)



# ACUTE INJURY ----


df1 <-
  df %>%
  filter(onset == "acute")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(onset = "acute") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(onset) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


acute_prop <- left_join(dat, prop, by = c("onset"))



# Overuse INJURY ----


df1 <-
  df %>%
  filter(onset == "overuse")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(onset = "overuse") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(onset) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


overuse_prop <- left_join(dat, prop, by = c("onset"))



# rbind df--------------------------

prop_data <- rbind.data.frame(acute_prop,
                              overuse_prop)



prop_data_raw <-
  prop_data %>%
  mutate(group = "injury_onset")


prop_data_raw$variable <- paste0(prop_data_raw$onset, " | ", prop_data_raw$total_injury_no, " (",prop_data_raw$study_no, ")")

prop_data_raw$variable<-str_to_title(prop_data_raw$variable)

onset_cat_data <-
  prop_data_raw %>%
  select(7,8,4:6)


prop_data$prop_CI <- paste0(prop_data$estimate, " (", prop_data$conf.low, " - ",prop_data$conf.high, ")")


prop_data$onset<-str_to_title(prop_data$onset)

onset_data <- 
  prop_data %>%
  dplyr::select(1,2,3,7) %>%
  dplyr::rename(
    "Injury Onset" = onset,
    "Number of studies" = study_no,
    "Total injury count" = total_injury_no,
    "Meta-analysed proportion (95% CI)" = prop_CI)




############################################################################
# INJURY MECHANISM
############################################################################

inj_mechanism <- read_excel("data/proportions.xlsx", sheet = "Injury Mechanism")

inj_mech <-
  inj_mechanism %>%
  filter(injury_count > 0)


inj_mech$mechanism <-tolower(inj_mech$mechanism)


# Mechnism Cats

contact_cat <-
  c("player contact",
    "other contact",
    "contact",
    "other contact")


non_contact_cat <-
  c("non-contact",
    "cumulative")



inj_mech <- 
  inj_mech %>% 
  mutate(mech_cat = case_when(mechanism%in% contact_cat ~ "contact",
                           mechanism %in% non_contact_cat ~ "non contact",
                           TRUE ~"other"))


df <-
  inj_mech %>%
  group_by(reference, sport, activity, mech_cat) %>%
  dplyr::summarise(total_count = mean(total_count),
                   injury_count = sum(injury_count)) %>%
  mutate(prop = ((injury_count / total_count) * 100),
         prop_lcl = prop / (exp(1.96 / sqrt(total_count))),
         prop_ucl = prop * (exp(1.96 / sqrt(total_count))),
         prop_se = (prop_ucl - prop_lcl)/3.92) %>%
  mutate_if(is.character, as.factor)



# CONTACT INJURY ----


df1 <-
  df %>%
  filter(mech_cat == "contact")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(mech_cat = "contact") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(mech_cat) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


contact_prop <- left_join(dat, prop, by = c("mech_cat"))



# NON - CONTACT INJURY ----


df1 <-
  df %>%
  filter(mech_cat == "non contact")

(model1 <-
    rma.mv(
      prop,
      prop_se,
      random = ~ 1 | reference,
      data = df1))

prop <- tidy((model1), conf.int = TRUE) %>%
  mutate(mech_cat = "non contact") %>%
  select(9,3,7,8) %>%
  mutate_if(is.numeric, round, digits = 1)


study_number <- length(unique(df1$reference))


dat <-
  df1 %>%
  mutate(
    study_no = study_number) %>%
  group_by(mech_cat) %>%
  dplyr::summarise(
    study_no = mean(study_no),
    total_injury_no = sum(injury_count)) %>%
  mutate_if(is.numeric, round, digits = 0)


non_contact_prop <- left_join(dat, prop, by = c("mech_cat"))


# rbind df--------------------------

prop_data <- rbind.data.frame(contact_prop,
                              non_contact_prop)


prop_data_raw <-
  prop_data %>%
  mutate(group = "injury_mechanism")


prop_data_raw$variable <- paste0(prop_data$mech_cat, " | ", prop_data_raw$total_injury_no, " (",prop_data_raw$study_no, ")")

prop_data_raw$variable<-str_to_title(prop_data_raw$variable)

mech_cat_data <-
  prop_data_raw %>%
  select(7,8,4:6)


prop_data$prop_CI <- paste0(prop_data$estimate, " (", prop_data$conf.low, " - ",prop_data$conf.high, ")")


prop_data$mech_cat<-str_to_title(prop_data$mech_cat)

mechanism_data <- 
  prop_data %>%
  dplyr::select(1,2,3,7) %>%
  dplyr::rename(
    "Injury Mechanism" = mech_cat,
    "Number of studies" = study_no,
    "Total injury count" = total_injury_no,
    "Meta-analysed proportion (95% CI)" = prop_CI)



# WRITE TO EXCEL -----

library(openxlsx)


list_of_datasets <- list("location proportions" = loc_data,
                         "type proportions" = type_data,
                         "time bin proportions" = time_bin_data,
                         "injury onset proportions" = onset_data,
                         "injury mechanism proportions" = mechanism_data)
write.xlsx(list_of_datasets, file = "results/meta_analysed_proportions.xlsx", 
           colNames = TRUE, rowNames = FALSE, append = FALSE)




# Plot data ----------------



plot_data <- rbind.data.frame(region_location_data,
                              tissue_type_data,
                              severity_cat_data,
                              onset_cat_data,
                              mech_cat_data)



list_of_datasets <- list("plot proportions" = plot_data)
write.xlsx(list_of_datasets, file = "results/plot_proportions_data.xlsx", 
           colNames = TRUE, rowNames = FALSE, append = FALSE)






