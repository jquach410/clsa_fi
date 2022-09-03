library(dplyr)

# script for cleaning normative data

normsfi62 <- 
  read.csv("data/normsfi62.csv")

male <- 
  normsfi62 %>% 
  slice(1:41) %>% 
  pivot_longer(cols = X1:X99, names_to = "percentile", values_to = "FI") %>% 
  mutate(Sex = 1) %>% 
  rename(Age = Men)

female <- 
  normsfi62 %>% 
  slice(46:86) %>% 
  pivot_longer(cols = X1:X99, names_to = "percentile", values_to = "FI") %>% 
  mutate(Sex = 2) %>% 
  rename(Age = Men)

normsfi62_clean <-  
 bind_rows(male, female) %>% 
  mutate(percentile = gsub("X", "", percentile),
         FI = round(FI, digits = 5)) 

write.csv(normsfi62_clean, "data/normsfi62_clean.csv")



# testing

dataframetest <- 
  tibble(
    fi = 0.5,
    sex = 1,
    age = 45
  )

 normsfi62_clean <- 
      read.csv("data/normsfi62_clean.csv") %>% 
      select(-X) %>% 
      rename(Age_Norm = Age,
             Sex_Norm = Sex,
             FI_Norm = FI) %>% 
      mutate_all(as.numeric) %>% 
   mutate(fi = dataframetest$fi,
          age = dataframetest$age,
          sex = dataframetest$sex) %>% 
   mutate(
     fi_dist = FI_Norm - fi,
     fi_dist = if_else(fi_dist < 0, abs(fi_dist - 1), fi_dist)
     ) %>% 
   filter(age == Age_Norm & sex == Sex_Norm & fi_dist >=0) %>% 
   slice(which.min(fi_dist)) %>% View
   
   
