

'''
library(dplyr)

study_names <- c("Barton_2018", "ChengpingW_2017", "FengQ_2015", "Ghosh_2020", "JieZ_2017")
print(study_names)
markers <- c("AST", "ALT", "Triglyceride", "ALP", "HDL", "LDL", "Cholesterol", "HSCRP")

for(i in study_names) {
  bootstrap_list[[i]] <- final_table1[final_table1$study_name==i, c(1:9, 16:30)]
  if(i == "JeiZ_2017"){
    break
  } 
} 

bootstrap_df <- bind_rows(bootstrap_list)

bootstrap_df[is.na(bootstrap_df)] <- 0

unique(is.na(bootstrap_df$study_name))

num_studies <- length(bootstrap_list)

library(randomForest)

for (i in study_names) {
  for(j in markers){
    data
    rf_model <- randomForest(j ~ ., data=bootstrap_df)
    rf_model_list[[paste("rf_model", i)]] <- rf_model
    
    
  }
  
  
}


list_with_na <- list()
markers <- c("Age","BMI","Glucose","HSCRP","Insulin","Creatinine","AST","ALP","ALT","Triglyceride","HDL","LDL","Cholesterol","Systolic","Diastolic", "HBA1C")


names_studies <- c("Wilmanski_2022", "Barton_2018",  "ChengpingW_2017",  "Tian_et_al_2023",  "QinJ_2012",  "LiJ_2014",  "Ghosh_2020",  "He_at_al_2019",  "QinN_2014",  "HMP_2019_t2d",  "MetaCardis_2022",  "CuestaZuluaga_2017",  "Cronin_2018",  "KarlssonFH_2013",  "JieZ_2017",  "Song_2023",  "FengQ_2015",  "YuJ_2015",  "keohaneD_2020",  "ZhuF_2020")
for(i in names_studies){
  for(j in markers){
    has_na <- all(is.na(sorted_list[[i]][[j]]))
    if(has_na){
      list_with_na[[paste(i,j, sep='_')]] <- has_na
      
    }
  }
  
}


#code to figure out which study has all the rows with na in triglyceride column of every study
names_studies <- c("Wilmanski_2022", "Barton_2018",  "ChengpingW_2017",  "Tian_et_al_2023",  "QinJ_2012",  "LiJ_2014",  "Ghosh_2020",  "He_at_al_2019",  "QinN_2014",  "HMP_2019_t2d",  "MetaCardis_2022",  "CuestaZuluaga_2017",  "Cronin_2018",  "KarlssonFH_2013",  "JieZ_2017",  "Song_2023",  "FengQ_2015",  "YuJ_2015",  "keohaneD_2020",  "ZhuF_2020")
markers <- c("Age","BMI","Glucose","HSCRP","Insulin","Creatinine","AST","ALP","ALT","Triglyceride","HDL","LDL","Cholesterol","Systolic","Diastolic", "HBA1C")
library(dplyr)
for(i in names_studies){
  for(j in markers){
  list_with_na_[j] <- list()
  dataset <- list()
  has_na <- all(is.na(sorted_list[[i]][[j]]))
  print(has_na)
    if(has_na){
      list_with_na_j[[paste(i, sep='_')]] <- has_na
      study_with_all_na <- names(list_with_na_j)
      for(k in study_with_all_na){
        tg_dataset[[k]] <- sorted_list[[k]]
      [j]_dataset_df <- bind_rows(j_dataset)  
      }
    }
}
  
}

tg_dataset <- bind_rows(sorted_list[[]])
'''

user_input1 <- as.integer(readline(prompt = "Enter a number: "))
user_input2 <- as.integer(readline(prompt = "Enter a number: "))
user_input3 <- (readline(prompt = "Enter the arithmetic operation: "))


for(input_1 in user_input1){
  for(input_2 in user_input2){
    for(input_3 in user_input3){
      if(input_3 == "+"){
        A <- input_1 + input_2
        print(A)
        
      } else if(input_3== "-"){
        B <- input_1 - input_2
        print(B)
    } else if(input_3=="*"){
        C <- input_1 * input_2
        print(C)
        
      }else if(input_3=="/"){
        D <- input_1/input_2
        print(D)
      }else if(input_3=="sqrt"){
        E <- sqrt(input_1)
        print(E)
  }
}
        
}
      
}  
    
 
  







