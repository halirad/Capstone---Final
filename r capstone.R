library(tidyverse)
library(janitor)
library(ggplot2)
install.packages("reshape2") 
library(dplyr)

Pov_Data <- read.csv("/Users/haliradecker/Documents/MSPP/Data Studio/PROJECT/table_1.csv")

##Looking at Column names
colnames(Pov_Data)


##Drop Unnecessary Variables
pov_clean <- select(Pov_Data, par_pctile, count_pooled, count_aian_pooled, count_asian_pooled, count_black_pooled,
count_hisp_pooled, count_white_pooled, kfr_black_pooled, kfr_hisp_pooled, kfr_white_pooled, kid_college_black_female, 
kid_college_black_male, kid_college_white_female, kid_college_white_male, kid_pos_hours_white_male, kid_wage_rank_black_female, 
kid_wage_rank_black_male, kid_wage_rank_white_female, kid_wage_rank_white_male, kir_black_female, kir_black_male, kir_black_pooled, 
kir_white_female, kir_white_male, kir_white_pooled, kir_1par_black_male, kir_1par_white_male, kir_2par_black_male, 
kir_2par_white_male, kir_par_nohome_black_male, kir_par_nohome_white_male)



##Create the Income Quintile Data
  ##Convert the data set from 100 individual percentile rows, to 5 quintiles to distinguish 5 income levels

pov_data_grp_qntl <- pov_clean %>% mutate(Household_Quintile = cut(par_pctile, c(0,20,40,60,80,100)))

income_quintile <- pov_data_grp_qntl %>% group_by(Household_Quintile) %>% 
  summarise(sum_pooled = sum(count_pooled),
            sum_aian_pooled = sum(count_aian_pooled),
            sum_asian_pooled = sum(count_asian_pooled),
            sum_black_pooled = sum(count_black_pooled),
            sum_hisp_pooled = sum(count_hisp_pooled),
            sum_white_pooled = sum(count_white_pooled),
            avg_kid_wage_rank_black_female = mean(kid_wage_rank_black_female),
            avg_kid_wage_rank_black_male = mean(kid_wage_rank_black_male),
            avg_kid_wage_rank_white_female = mean(kid_wage_rank_white_female),
            avg_kid_wage_rank_white_male = mean(kid_wage_rank_white_male),
            avg_kid_college_black_female = mean(kid_college_black_female, na.rm = TRUE),
            avg_kid_college_black_male = mean(kid_college_black_male),
            avg_kid_college_white_female = mean(kid_college_white_female),
            avg_kid_college_white_male = mean(kid_college_white_male))


##Create Lowest Income Quintile by Ethnicity Data
low_income_quintile_raw <- pov_data_grp_qntl %>% group_by(Household_Quintile) %>% 
  summarise(sum_aian_pooled = sum(count_aian_pooled),
            sum_asian_pooled = sum(count_asian_pooled),
            sum_black_pooled = sum(count_black_pooled),
            sum_hisp_pooled = sum(count_hisp_pooled),
            sum_white_pooled = sum(count_white_pooled)) 
low_income_quintile <- low_income_quintile_raw %>% 
  filter(row_number()==1)

##Create Table with Lowest Income Totals by Ethnicity and Total Number of Participants by Ethnicity
low_income_quintile_totals <- low_income_quintile_raw %>% 
  adorn_totals("row") %>% 
  filter(row_number()==1 | row_number()==n())


##Export CSV
write_csv(low_income_quintile_totals, "Lowest Income by Ethnicity.csv")



##Create Lowest Income Quintile by Ethnicity
low_income_quintile_raw_2 <- pov_data_grp_qntl %>% group_by(Household_Quintile) %>% 
  summarise(avg_kid_wage_rank_black_female = mean(kid_wage_rank_black_female),
            avg_kid_wage_rank_black_male = mean(kid_wage_rank_black_male),
            avg_kid_wage_rank_white_female = mean(kid_wage_rank_white_female),
            avg_kid_wage_rank_white_male = mean(kid_wage_rank_white_male))
low_income_quintile_kir <- low_income_quintile_raw_2 %>% 
  filter(row_number()==1)


##Export CSV
write_csv(low_income_quintile_kir, "Lowest Income Kid Income Rank.csv")

##Create College Data by Quintile
College_income_quintile <- pov_data_grp_qntl %>% group_by(Household_Quintile) %>% 
  summarise(avg_kid_college_black_female = mean(kid_college_black_female, na.rm = TRUE),
            avg_kid_college_black_male = mean(kid_college_black_male),
            avg_kid_college_white_female = mean(kid_college_white_female),
            avg_kid_college_white_male = mean(kid_college_white_male))

##Export csv
  write_csv(College_income_quintile, "College by Quintile.csv")
  

##Create Child Income Rank by Quintile
kid_rank_by_quintile <- pov_data_grp_qntl %>% group_by(Household_Quintile) %>% 
    summarise(avg_kid_wage_rank_black_female = mean(kid_wage_rank_black_female),
              avg_kid_wage_rank_black_male = mean(kid_wage_rank_black_male),
              avg_kid_wage_rank_white_female = mean(kid_wage_rank_white_female),
              avg_kid_wage_rank_white_male = mean(kid_wage_rank_white_male))

## Group together Child Income Rank and Child College Attendance
kid_college_rank_by_quintile <- pov_data_grp_qntl %>% group_by(Household_Quintile) %>% 
  summarise(avg_kid_college_black_female = mean(kid_college_black_female, na.rm = TRUE),
            avg_kid_wage_rank_black_female = mean(kid_wage_rank_black_female),
            avg_kid_college_black_male = mean(kid_college_black_male),
            avg_kid_wage_rank_black_male = mean(kid_wage_rank_black_male),
            avg_kid_college_white_female = mean(kid_college_white_female),
            avg_kid_wage_rank_white_female = mean(kid_wage_rank_white_female),
            avg_kid_college_white_male = mean(kid_college_white_male),
            avg_kid_wage_rank_white_male = mean(kid_wage_rank_white_male))

write_csv(kid_college_rank_by_quintile, "College Attendance Income Rank.csv")

##Export CSV
write_csv(kid_rank_by_quintile, "Child Income.csv")


##Look at Effects of Residential Stability on Income

income_rank_homeownership <- pov_data_grp_qntl %>% group_by(Household_Quintile) %>% 
  summarise(avg_kir_par_nohome_black_male = mean(kir_par_nohome_black_male),
            avg_kir_par_nohome_white_male = mean(kir_par_nohome_white_male))

write_csv(income_rank_homeownership, "Homeownership Income Rank Quintile.csv")


##Look at 1 vs 2 parental household
number_parents <- pov_data_grp_qntl %>% group_by(Household_Quintile) %>% 
  summarise(avg_kir_1par_black_male = mean(kir_1par_black_male),
            avg_kir_1par_white_male = mean(kir_1par_white_male),
            avg_kir_2par_black_male = mean(kir_2par_black_male),
            avg_kir_2par_white_male = mean(kir_2par_white_male))

write_csv(number_parents, "number of parents.csv")            
            


##Line chart comparing density of children born into different household income levels
ggplot(Pov_Data) + geom_smooth(aes(x=par_pctile, y=density_aian_pooled), size=1, color = "#7df7c5") + 
  geom_smooth(aes(x=par_pctile ,y=density_asian_pooled), size=1, color = "#d8e896") + 
  geom_smooth(aes(x=par_pctile,y=density_black_pooled) ,size=1, color = "#09bb9f") + 
  geom_smooth(aes(x=par_pctile,y=density_hisp_pooled) , size=1, color = "#fee45f") + 
  geom_smooth(aes(x=par_pctile,y=density_white_pooled), size=1, color= "#c4c4c4") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

##Line chart comparing the future income ranks of the two given participant group depending on their childhood household income level
ggplot(pov_clean) + geom_smooth(aes(x=par_pctile, y=kir_black_female), size=1, color = "#7df7c5") + 
  geom_smooth(aes(x=par_pctile ,y=kir_black_male), size=1, color = "#d8e896") + 
  geom_smooth(aes(x=par_pctile,y=kir_white_female) ,size=1, color = "#09bb9f") + 
  geom_smooth(aes(x=par_pctile,y=kir_white_male) , size=1, color = "#fee45f") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

##Line Chart comparing the future wage ranks of the given participant groups depending on their childhood household income level
ggplot(pov_clean) + geom_smooth(aes(x=par_pctile, y=kid_wage_rank_black_female), size=1, color = "#7df7c5") + 
  geom_smooth(aes(x=par_pctile ,y=kid_wage_rank_black_male), size=1, color = "#d8e896") + 
  geom_smooth(aes(x=par_pctile,y=kid_wage_rank_white_female) ,size=1, color = "#09bb9f") + 
  geom_smooth(aes(x=par_pctile,y=kid_wage_rank_white_male) , size=1, color = "#fee45f") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


  


