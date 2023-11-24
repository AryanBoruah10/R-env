folder_path= "D:\\metadata\\"
library(dplyr)
install.packages("purrr")
library(purrr)
file_names= list.files(folder_path)
metadata_list= list()

for(files in file_names)
{
  file_path= paste0(folder_path,files)
  metadata_list[[sub("_Metadata.*","",files)]]= read_xlsx(file_path,col_names = TRUE)
}

#sub names from a data frame 
names(metadata_list) <- sub(".xlsx","", names(metadata_list))

# #change column names
colnames(my_list_converted[["Wilmanski_2022"]][8]) <- "sample_ID"


# for (i in names(metadata_list)) {
#   metadata_list[i] <- as.data.frame(metadata_list[i])
# }

# Function to convert lists and tibbles to data frames
to_data_frame <- function(x) {
  if (inherits(x, "tbl_df")) {
    return(as.data.frame(x))
  } else if (is.list(x)) {
    return(lapply(x, to_data_frame))
  } else {
    return(x)
  }
}

# Apply the to_data_frame function to all elements in the list
my_list_converted <- lapply(metadata_list, to_data_frame)
Wilmanski<- my_list_converted$Wilmanski_2022
colnames(Wilmanski)<-Wilmanski[1,]
Wilmanski<-Wilmanski[-1,]



#change the column name of dataframes in a list
new_column_name <- "sample_ID"

my_list_converted <- lapply(my_list_converted, function(df) {
  if ("Sample ID" %in% names(df)) {
    names(df)[names(df) == "Sample ID"] <- new_column_name
  }
  return(df)


colnames(arr2)[1] <- "sample_ID"
Wil <- merge(Wilmanski,arr2, by = "sample_ID")
Wilmanski$sample_ID<- as.integer(Wilmanski$sample_ID)


delete_point <- c(12,13,20)
my_list_converted[-delete_point]
my_list_converted <- c(my_list_converted, list(KeohaneD_merg))
Jeiz_merg <- merge(my_list_converted$Jeiz_2017, my_list_converted$Jiez_sample_ID, by = "sample_ID")


colnames(Jeiz_merg)[17] <- "sample_ID"
Jeiz_merg$sample_ID <- NULL


my_list_converted$KeohaneD_2020CMD3 <- NULL

my_list_converted$Wilmanski_2022<- Wil

merged_metadata <- do.call(rbind,my_list_converted)
merged_metadata <- bind_rows(my_list_converted)


# Function to convert column types and merge dataframes
merge_metadata <- function(final_table1, final_table2) {
  common_columns <- intersect(names(final_table1), names(final_table2))
  converted_columns <- setdiff(common_columns, "study_name")
  
  # Convert columns to compatible types
  for (col in converted_columns) {
    if (is.character(final_table1[[col]])) {
      final_table2[[col]] <- as.character(final_table2[[col]])
    } else if (is.character(final_table2[[col]])) {
      final_table1[[col]] <- as.character(final_table1[[col]])
    }
  }
  
  # Merge dataframes using full_join
  tryCatch(
    {
      merged_final_table <- full_join(final_table1, final_table2, by = common_columns)
    },
    error = function(e) {
      cat("Error merging dataframes:", deparse(substitute(final_table1)), "and", deparse(substitute(final_table2)), "\n")
    }
  )
  
  # Return merged dataframe or NULL if error occurred
  if (exists("merged_final_table")) {
    return(merged_final_table)
  } else {
    return(NULL)
  }
}
Merged_Metadata <- reduce(my_list_converted, merge_metadata)
##########################################################################################

#omit a column from a dataframe 

Merged_Metadata$sample_id <- NULL





#code to load R file in R 
load("D:\\Cardiogut_metaanalysis\\Code and Environment\\merged_species_table (1).RData")


#CODE TO CREATE SUMMARY TABLE OF MERGED METADATA

# Step 1: Extract the metadata columns (from 2 to 21) from the final_table
metadata_cols <- Merged_Metadata[, 3:16]

# Step 2: Create an empty summary table with unique study names as rows and metadata columns as columns
summary_table <- data.frame(study_name = unique(Merged_Metadata$study_name), stringsAsFactors = FALSE)

# Step 3: Loop through each metadata column and check for each unique study name
for (col in names(metadata_cols)) {
  for (study in unique(Merged_Metadata$study_name)) {
    rows_for_study <- Merged_Metadata$study_name == study
    if (any(!is.na(metadata_cols[rows_for_study, col]))) {
      summary_table[summary_table$study_name == study, col] <- 1
    } else {
      summary_table[summary_table$study_name == study, col] <- 0
    }
  }
}
# Step 4: Add a column to count the number of samples for each study
summary_table$sample_count <- sapply(unique(Merged_Metadata$study_name), function(study) {
  sum(Merged_Metadata$study_name == study)
})
# Step 5: Print the summary table
print(summary_table)
write.table(summary_table,"summary_table.txt",sep="\t",row.names = TRUE)



install.packages("openxlsx")
library(openxlsx)


write.xlsx(summary_table, "D:\\Cardiogut_metaanalysis\\Code and Environment\\summary_table.xlsx", sheetName = "Sheet1")


difference_metadf <- setdiff(Merged_Metadata, final_table)




karlsson_corr <- cor(Merged_Metadata$study_name=="KarlssonFH_2013", final_table$study_name=="KarlssonFH_2013", method = "pearson")
unique(Merged_Metadata$study_name)




values_df1 <- QinJ_mmt[["Cholesterol"]]
values_df2 <- QinJft[["Cholesterol"]]



scaling_factor <- (values_df2/values_df1)

cat("Scaling Factor:", scaling_factor, "\n")







# Sample Data Frame
df <- data.frame(ID = c("C", "A", "B"),
                 Value = c(30, 10, 20))

# Set the "ID" column as row names
rownames(df) <- df$ID

# Arrange the data frame based on row names
KarlssonFH_mmt <- KarlssonFH_mmt[rownames(KarlssonFH_mmt), ]

# Drop the row names column (optional)
rownames(df) <- NULL

# Print the result
print(KarlssonFH_mmt)

QinJ_mmt <- QinJ_mmt[order(QinJ_mmt$sample_ID), ]
rownames(QinJ_mmt) <- QinJ_mmt$sample_ID 
Jiez_2017_mmt$Triglyceride <- as.numeric(Jiez_2017_mmt$Triglyceride)



combined_row_names <- c(rownames(QinJft), rownames(QinJ_mmt))
new_df[is.na(new_df)] <- NA
new_df2 <- data.frame(QinJft = c(rownames(QinJft)), QinJ_mmt = c(rownames(QinJ_mmt)))


QinJft <- final_table[final_table$study_name=="QinJ_2012", ]
QinJ_mmt <- Merged_Metadata[Merged_Metadata$study_name=="QinJ_2012", ]



List_of_Metadata <- list()
Names <- c("Karlsson", "Metacardis", "HMP", "QinJ")
names(List_of_Metadata) <- c("Karlsson", "Metacardis", "HMP", "QinJ")

for (name in names(List_of_Metadata)) {
  List_of_Metadata[[name]] <- data.frame()
}                    



#values_df1 <- KarlssonFH_mmt[["Cholesterol"]]
#values_df2 <- KarlssonFH_2013ft[["Cholesterol"]]

#scaling_factor <- (values_df2/values_df1)
#cat("Scaling Factor:", scaling_factor, "\n")
#List_of_Metadata$Karlsson <- as.data.frame(List_of_Metadata$Karlsson)


KarlssonFH_mdsf <- list()
QinJ_mdsf <- list()
Metacardis_mdsf <- list()
HMP_mdsf <- list()

metacardis_mmt<- Merged_Metadata[Merged_Metadata$study_name=="MetaCardis_2022", 1:19]
HMP_ft <- final_table[final_table$study_name=="HMP_2019_t2d", 1:15]


#order_names <- order(names(KarlssonFH_mmt))
#df1 <- KarlssonFH_mmt[, order_names]
#df2 <- KarlssonFH_2013ft[, order_names]

KarlssonFH_2013ft <- KarlssonFH_2013ft[, names(KarlssonFH_mmt)]

common_columns <- intersect(names(QinJ_mmt), names(QinJft))

# Reorder columns in df1 based on the column sequence of df2
QinJ_mmt <- QinJ_mmt[, common_columns]

# Reorder columns in df2 based on the column sequence of df1
QinJft <- QinJft[, common_columns]
#remove duplicate values
non_unique_values <- duplicated(metacardis_mmt$sample_ID) | duplicated(metacardis_mmt$sample_ID, fromLast = TRUE)
metacardis_mmt <- metacardis_mmt[!non_unique_values, ]

non_unique_values <- Metac$sample_ID[duplicated(Metac$sample_ID) | duplicated(Metac$sample_ID, fromLast = TRUE)]
print(non_unique_values)
Jiez_mmt$HSCRP<-NULL
Jiez_mmt$ALT<-NULL
Jiez_mmt$AST<-NULL
Jiez_mmt$Age<-NULL
Jiez_mmt$BMI<-NULL
Jiez_mmt$VLDL<-NULL
Jiez_mmt$study_name<-NULL
KarlssonFH_mmt<-KarlssonFH_mmt[-145, ]
KarlssonFH_mmt$AST <- NULL
KarlssonFH_mmt$ALT <- NULL
KarlssonFH_mmt$ALP <- NULL
KarlssonFH_mmt$Systolic <- NULL
KarlssonFH_mmt$Diastolic <- NULL
KarlssonFH_mmt$BMI <- NULL
KarlssonFH_mmt$Age <- NULL



library(dplyr)
#############
Merged_Metadata <- Merged_Metadata %>% distinct(sample_ID, .keep_all = TRUE)
###############


metacardis_mmt<- apply(metacardis_mmt, 2, as.numeric)
as.numeric(KarlssonFH_mmt$Glucose)
KarlssonFH_mmt$HBA1C<-as.numeric(KarlssonFH_mmt$HBA1C)

#replace NA with 0
Jiez_2017ft[is.na(Jiez_2017ft)] <- 0


#creating list for scaling factor of karlsson for each metadata column
QinJ_scaling_factor <- list()
for (i in colnames(QinJ_mmt)) {
  index_df1 <- match(common_rownames, rownames(QinJ_mmt))
  index_df2 <- match(common_rownames, rownames(QinJft))
  
  scaling_factor <- QinJft[index_df2, i] / QinJ_mmt[index_df1, i]
  
   QinJ_scaling_factor[[i]] <- scaling_factor
}

#To change character to numeric 
QinJ_mmt <- as.data.frame(sapply(QinJ_mmt, as.numeric))
#Find common rownames between two dataframes by intersect function
common_rownames <- intersect(rownames(QinJ_mmt), rownames(QinJft))


Merged_Metadata$sample_ID[Merged_Metadata$study_name== "MetaCardis_2022"] <- paste0("M0", Merged_Metadata$sample_ID[Merged_Metadata$study_name== "MetaCardis_2022"])
matching_row_Metacardis<- intersect(Metacardis_ft$sample_ID, metacardis_mmt$sample_ID)

Merged_Metadata <- merge(metacardis_mmt, Metacardis_ft[, c("LDL")], by = "sample_ID", all.x = TRUE)




matching_row_metacardis <- intersect(rownames(final_table[final_table$study_name== "MetaCardis_2020_a",]),Merged_Metadata$sample_ID[Merged_Metadata$study_name== "MetaCardis_2022"])


for (i in matching_row_metacardis) 
{

  Merged_Metadata[i,"HSCRP"] <- final_table[i, "HSCRP"]
  
}


matching_row_metacardis <- intersect(rownames(final_table[final_table$study_name== "MetaCardis_2020_a",]),Merged_Metadata$sample_ID[Merged_Metadata$study_name== "MetaCardis_2022"])


for (i in matching_row_metacardis) 
{
  
  Merged_Metadata[i,"HSCRP"] <- final_table[i, ""]
  
}


Merged_Metadata[Merged_Metadata$sample_ID=="M0x14MCx2088", ]



Merged_Metadata$sample_ID[Merged_Metadata$study_name== "MetaCardis_2022"] <- paste0("M0", Merged_Metadata$sample_ID[Merged_Metadata$study_name== "MetaCardis_2022"])

rownames(Merged_Metadata[Merged_Metadata$study_name=="MetaCardis_2022", ]) <- Merged_Metadata[Merged_Metadata$study_name=="MetaCardis_2022", "sample_ID"]







Merged_Metadata <- Merged_Metadata %>% distinct(sample_ID, .keep_all = TRUE)
is.na(Merged_Metadata$sample_ID)
unique(is.na(Merged_Metadata$sample_ID))

rownames(Merged_Metadata) <- Merged_Metadata$sample_ID

rownames(Merged_Metadata[Merged_Metadata$study_name=="MetaCardis_2022", ]) <- Merged_Metadata[Merged_Metadata$study_name=="MetaCardis_2022", "sample_ID"]
a <- c(1:1087)
for(i in a ){
  
}


Merged_Metadata<-  Merged_Metadata[, -c(14,15,19)]

old_summary_table_aligned <- old_summary_table[rownames(summary_table), colnames(summary_table)]
summary_table_aligned <- summary_table

# Check for matching elements and create a resulting dataframe
result_df <- ifelse(old_summary_table_aligned == summary_table_aligned, "F", ifelse(old_summary_table_aligned == 1, "Missing_in_new", "Missing_in_old"))

# Convert the result to a dataframe
result_df <- as.data.frame(result_df)
Merged_Metadata$study_name<- NULL

Merged_Metadata$sample_ID<- NULL

#find the range of metadata values
range_df <- summary_table
rownames(range_df)<-rownames(summary_table)
colnames(range_df) <- colnames(summary_table)

for (i in rownames(range_df)) {
  for (j in colnames(range_df)){
    row_index <-which(Merged_Metadata$study_name==i)
    temp_vec <- Merged_Metadata[row_index,j]
    
    temp_range <- range(temp_vec)
  }
}


Merged_Metadata[Merged_Metadata$study_name=="Barton_2018",]
which([Merged_Metadata$study_name=="Barton_2018",)], arr.ind = FALSE)
