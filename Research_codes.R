
#code to calculate correlation values and then create LOOCV(Leave one out cross validation) matrix 

markers <- c("Age", "ALP", "ALT", "AST", "BMI", "Cholesterol", "Creatinine", "Diastolic", "Glucose", "HBA1C", "HDL",
             "HSCRP", "Insulin", "LDL", "Systolic", "Triglyceride")
names_studies <- c("reduced_list_Age","reduced_list_ALP" ,"reduced_list_ALT","reduced_list_AST","reduced_list_BMI",         
"reduced_list_cholesterol","reduced_list_Creatinine","reduced_list_Diastolic","reduced_list_Glucose",     
"reduced_list_HBA1C","reduced_list_HDL","reduced_list_HSCRP","reduced_list_Insulin",     
"reduced_list_LDL","reduced_list_Systolic","reduced_list_triglyceride")

load("C:/Users/aryan/Desktop\rf_tg_env.RData") # set the path accordingly(differ individually)
library(randomForest)
library(caret)
library(dplyr)
rfmodel10 <- list()
num_iterations <- 10


for (study_names in names_studies){
  current_study <- get(study_names)
  print(study_names)
  for(marker in markers){
    print(marker)
  
    matrix_name <- paste("CorrMat_", marker, sep = "")
    x <- matrix(ncol = length(current_study), nrow = num_iterations)
    colnames(x) <- names(current_study)
    assign(matrix_name, x)
    
  for (study_num in 1:length(current_study)){ 
    print(paste("List: ", study_names, sep = ""))
    
    for (i in 1:num_iterations) {
      print(paste("Iteration:", i))
      # Extract data for all other studies except the current study
      leave_out_data <- do.call(rbind, current_study[-study_num])
      # Train Random Forest model
      formula <- as.formula(paste(current_study[[study_num]][i], "~ ."))
      rf_model <- randomForest(formula , data = leave_out_data, ntree = 10, mtry = 10)
      print(paste("rfmodel:", i , "complete"))
      # Store the Random Forest model in the list
      rfmodel5[[names(current_study[study_num])]][[paste("rf_model", i)]] <- rf_model
      print(paste("rfmodel:", i , "for", marker, names(current_study[study_num]) , "stored in the list"))
      
      predictions <- predict(rfmodel5[[names(current_study[study_num])]][[i]], newdata=current_study[[study_num]])
      
      corr_value <- cor(predictions, current_study[[study_num]][[i]])
      print(corr_value)
      
      z <- get(matrix_name)
      z[i, names(current_study[study_num])] <- corr_value
      print(paste("correlation value:", i , "stored in the matrix"))
      
    }
    Dynamic_file_path <- paste("C:/Users/aryan/Desktop/boot_df/list_", marker , ".RData", sep="")
    save.image(file = Dynamic_file_path)
    print(paste("Dynamic file saved for Study",marker))
    
  }
    
}
}  



# code to create out of the box correlation matrix 

CorrMatrix <- matrix(ncol = 14, nrow = 14)
colnames(CorrMatrix) <- names(Triglyceride_datasets)
rownames(CorrMatrix) <- names(Triglyceride_datasets)
rfmodel_same <- list()
rfmodel_diff <- list()
load(...............)


for(i in names(Triglyceride_datasets)){
  for (j in names(Triglyceride_datasets)){
    if(i==j){
      current_data <- Triglyceride_datasets[[i]]
      rf_model <- randomForest(Triglyceride ~ ., data = current_data, ntree = 20, mtry = 10)
      rfmodel_same[["Barton_2018"]] <- rf_model
      print("rfmodel stored")
      
      corr_value <- cor(rfmodel_same[[i]]$y, rfmodel_same[[i]]$predicted)
      print(paste("CorrValue:", i,"&", j,  corr_value, sep = ""))
      CorrMatrix[i,j] <- corr_value
      print("CorrValue added in the matrix")
    }else if(i !=j){
      
      
      train_data <- Triglyceride_datasets[[i]]
      test_data <- Triglyceride_datasets[[j]]
      
      rfmodel <- randomForest(Triglyceride ~ ., data = train_data, ntree = 50, mtry = 10)
      rfmodel_diff[[paste(i, "&", j, sep = "_")]] <- rfmodel
      print("rfmodel")
      predictions <- predict(rfmodel_diff[[paste(i, "&", j, sep = "_")]], newdata = test_data)
      corr_value <- cor(predictions, test_data$Triglyceride)
      print(paste("CorrValue:", i, "&", j, corr_value))
      CorrMatrix[i,j] <- corr_value
      print("CorrValue added in the matrix")
    }
    
    
  }
  if(i=="keohaneD_2020" & j == "keohaneD_2020"){
    file_path <- "........."
    save.image(file = file_path)
    print("file saved")
    
  }
  
}

#code to subset common columns from multiple data frames in a list and create another list with those subset columns

columns_to_subset <- c("Triglyceride",rownames(filtered_BatchSpecies_REM_Triglyceride[filtered_BatchSpecies_REM_Triglyceride$dir %in% c(2, 3, -2, -3), ]))
Triglyceride_datasets_boot <- Triglyceride_datasets
for(i in 1:14)
{   
`Triglyceride datasets`[[i]] <- `Triglyceride datasets`[[i]][, c("Age", "BMI", columns_to_subset)]
}


#code to remove n number columns from n number of data frames in a list 
remove_columns <- function(df) {
  df %>%
    select(-Age, -BMI)
}
Triglyceride_datasets <- lapply(`Triglyceride datasets`, remove_columns)

#Replace all the NAs with 0 from all the data frame in a list 

for (i in 1:length(`Triglyceride datasets`)) {
  for (k in order_list_datasets) {
    for (j in 1:length(datasets[[i]])) {
      na_indices <- is.na(datasets[[i]][[j]][[k]])
      
      if (any(na_indices)) {
        print(paste("NA values detected in", k, "for", j, "in", i))
        print(which(na_indices))
        datasets[[i]][[j]][[k]][na_indices] <- 0
        print(paste("NA values removed in", k, "for", j, "in", i))
      }
    }
  }
}


#code to create summary table of metadata

metadata_cols <- final_table[, 2:17]

# Step 2: Create an empty summary table with unique study names as rows and metadata columns as columns
summary_table <- data.frame(study_name = unique(final_table$study_name), stringsAsFactors = FALSE)

# Step 3: Loop through each metadata column and check for each unique study name
for (col in names(metadata_cols)) {
  for (study in unique(final_table$study_name)) {
    rows_for_study <- final_table$study_name == study
    if (any(!is.na(metadata_cols[rows_for_study, col]))) {
      summary_table[summary_table$study_name == study, col] <- 1
    } else {
      summary_table[summary_table$study_name == study, col] <- 0
    }
  }
}
# Step 4: Add a column to count the number of samples for each study
summary_table$sample_count <- sapply(unique(final_table$study_name), function(study) {
  sum(final_table$study_name == study)
})
# Step 5: Print the summary table
print(summary_table)
write.table(summary_table,"summary_table.txt",sep="\t",row.names = TRUE)

#Code to create a web application to plot data of Penguin(R inbuilt database)

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)

penguins_csv <- "https://raw.githubusercontent.com/jcheng5/simplepenguins.R/main/penguins.csv"


df <- readr::read_csv(penguins_csv)
# Find subset of columns that are suitable for scatter plot
df_num <- df |> select(where(is.numeric), -Year)

ui <- page_sidebar(
  sidebar = sidebar(
    varSelectInput("xvar", "X variable", df_num, selected = "Bill Length (mm)"),
    varSelectInput("yvar", "Y variable", df_num, selected = "Bill Depth (mm)"),
    checkboxGroupInput(
      "species", "Filter by species",
      choices = unique(df$Species), 
      selected = unique(df$Species)
    ),
    hr(), # Add a horizontal rule
    checkboxInput("by_species", "Show species", TRUE),
    checkboxInput("show_margins", "Show marginal plots", TRUE),
    checkboxInput("smooth", "Add smoother"),
  ),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$species)
    df |> filter(Species %in% input$species)
  })
  
  output$scatter <- renderPlot({
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(
      theme(legend.position = "bottom"),
      if (input$by_species) aes(color = Species),
      geom_point(),
      if (input$smooth) geom_smooth()
    )
    
    if (input$show_margins) {
      margin_type <- if (input$by_species) "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",read
                               size = 8, groupColour = input$by_species, groupFill = input$by_species)
    }
    
    p
  }, res = 100)
}

shinyApp(ui, server)



library(shiny)
ui <- fluidPage(
  "Hello, world!"
)
server <- function(input, output, session) {
}
shinyApp(ui, server)



