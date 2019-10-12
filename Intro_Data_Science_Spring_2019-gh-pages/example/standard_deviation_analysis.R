###############################
# STANDARD DEVIATION ANALYSIS #
###############################

#Name: 
#Date: 
#Summary: This assignment 
## 1. practices summarizing data with tidyverse
## 2. demonstrate stratification

install.packages("tidyverse")
library("tidyverse")

#cf. install R/RStudio https://www.ics.uci.edu/~sternh/courses/210/InstallingRandRStudio.pdf
#cf. textbook https://r4ds.had.co.nz/index.html (not required)

#Download NYPD SQF Data
## cf. https://www1.nyc.gov/site/nypd/stats/reports-analysis/stopfrisk.page

column_names <- c(
  #subject information    
  "year", "sex", "race", "dob", "age", "ht_feet", "ht_inch",  
  "weight", "haircolr", "eyecolor", "build",
  #basis of search Y/N
  "sb_hdobj", "sb_outln", "sb_admis", "sb_other",
  #reason for stop Y/N
  "cs_objcs", "cs_descr", "cs_casng", "cs_lkout", "cs_cloth", 
  "cs_drgtr", "cs_furtv", "cs_vcrim", "cs_bulge", "cs_other",  
  #reason for frisk Y/N
  "rf_vcrim", "rf_othsw", "rf_attir", "rf_vcact", "rf_rfcmp", 
  "rf_verbl", "rf_knowl", "rf_furt", "rf_bulg",
  #additional circumstances Y/N
  "ac_rept",  "ac_inves", "ac_proxm", "ac_evasv", "ac_assoc", 
  "ac_cgdir", "ac_incid", "ac_time", "ac_stsnd", "ac_other", 
  #use of force Y/N
  "pf_hands", "pf_wall", "pf_grnd", "pf_drwep", "pf_ptwep", 
  "pf_baton", "pf_hcuff", "pf_pepsp", "pf_other",
  #weapons found Y/N
  "pistol",   "riflshot", "asltweap", "knifcuti", "machgun", 
  "othrweap",
  #contraband found Y/N
  "contrabn")

sqf_url <- "https://www1.nyc.gov/assets/nypd/downloads/zip/analysis_and_planning/stop-question-frisk/sqf-2003-csv.zip"
download.file(url = sqf_url, destfile = "sqf.zip")
sqf_df <- read_csv(file = "sqf.zip", 
                   col_types = cols(.default = col_character())) %>%
  select(column_names)

for(year in 2004:2012) {
  sqf_url <- str_c(
    "https://www1.nyc.gov/assets/nypd/downloads/zip/analysis_and_planning/stop-question-frisk/sqf-",
    year,
    "-csv.zip")
  download.file(url = sqf_url, destfile = "sqf.zip")
  sqf_df <-  bind_rows(sqf_df, 
                       read_csv(file = "sqf.zip",
                                col_types = cols(.default = col_character())) %>%
                         select(column_names)
  )
  file.remove("sqf.zip")
}

#An analysis ready for publication
#Disparate Impact Criteria: statistically significant AND 80% difference  

sqf_df %>%
  mutate(eth = ifelse(race == "Q" | race == "P", "H", "NH"),
         contraband_found    = contrabn == "Y") %>%
  filter(cs_bulge == "Y",
         sex == "M",
         eth == "NH",
         race %in% c("B", "W")) %>%
  group_by(race) %>%
  summarize(num              = n(),
            prob             = mean(contraband_found, na.rm = TRUE),
            sd               = sqrt(prob * (1 - prob)),
            se_prob          = sd / sqrt(num),
            `lower_prob (%)` = 100 * prob - 2 * se_prob,
            `upper_prob (%)` = 100 * prob + 2 * se_prob)


#How did we get there?
## group and summarize

sqf_df %>%
  summarize(n = n())

sqf_df %>%
  group_by(year) %>%
  summarize(n = n())

sqf_df %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  mutate(prop = n / sum(n))

sqf_df %>%
  group_by(year, race == "B") %>%
  tally() %>%
  na.omit() %>%
  mutate(prop = n / sum(n)) 

sqf_df %>%
  group_by(year, race == "B") %>%
  tally() %>%
  na.omit() %>%
  ungroup() %>%
  mutate(prop = n / sum(n))

###race: B is black, W is white, Q | P is Hispanic
sqf_df %>% 
  mutate(eth = ifelse(race == "Q" | race == "P", "H", "NH")) %>%
  select(eth, race)

sqf_df %>%
  mutate(eth = ifelse(race == "Q" | race == "P", "H", "NH")) %>%
  filter(cs_descr == "Y",
         sex == "M",
         eth == "NH",
         race %in% c("B", "W")) %>%
  group_by(race, contrabn) %>%
  tally()


#is use of force predictive of weapons?
sqf_df %>%
  mutate(eth = ifelse(race == "Q" | race == "P", "H", "NH"),
         pf =
           pf_hands == "Y" |
           pf_wall == "Y" |
           pf_grnd == "Y" |
           pf_drwep == "Y" |
           pf_ptwep == "Y" |
           pf_baton == "Y" |
           pf_hcuff == "Y" |
           pf_pepsp == "Y" |
           pf_other == "Y" ,
         wep =
           pistol == "Y" |
           riflshot == "Y" |
           asltweap == "Y" |
           knifcuti == "Y" |
           machgun == "Y" |
           othrweap == "Y") %>%
  filter(sb_outln == "Y",
         sex == "M",
         eth == "NH",
         race %in% c("B", "W")) %>%
  group_by(pf) %>%
  summarize(num              = n(),
            prob             = mean(wep, na.rm = TRUE),
            sd               = sqrt(prob * (1 - prob)),
            se_prob          = sd / sqrt(num),
            `lower_prob (%)` = 100 * prob - 2 * se_prob,
            `upper_prob (%)` = 100 * prob + 2 * se_prob)

#is use of force predictive of weapons, and therefore (sometimes) justified?
sqf_df %>%
  mutate(eth = ifelse(race == "Q" | race == "P", "H", "NH"),
         pf =
           pf_hands == "Y" |
           pf_wall == "Y" |
           pf_grnd == "Y" |
           pf_drwep == "Y" |
           pf_ptwep == "Y" |
           pf_baton == "Y" |
           pf_hcuff == "Y" |
           pf_pepsp == "Y" |
           pf_other == "Y" ,
         wep =
           pistol == "Y" |
           riflshot == "Y" |
           asltweap == "Y" |
           knifcuti == "Y" |
           machgun == "Y" |
           othrweap == "Y") %>%
  filter(sb_outln == "Y",
         sex == "M",
         eth == "NH",
         race %in% c("B", "W")) %>%
  group_by(race, pf) %>%
  summarize(num              = n(),
            prob             = mean(wep, na.rm = TRUE),
            sd               = sqrt(prob * (1 - prob)),
            se_prob          = sd / sqrt(num),
            `lower_prob (%)` = 100 * prob - 2 * se_prob,
            `upper_prob (%)` = 100 * prob + 2 * se_prob)

