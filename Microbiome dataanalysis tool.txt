# Step 1
load_microbiome_data <- function() {
  species_file <- read.table("C:/Users/aryan/Desktop/New folder/FilteredSpecies.txt")
  metadata <- read.table("C:/Users/aryan/Desktop/New folder/FilteredMetadata.txt")
  
  
  microbiome_data <- list(species = species_file, metadata = metadata)
  
  return(microbiome_data)
}

# Step 2
calculate_mean_std <- function(microbiome_data) {
  species_files <- microbiome_data$species
  for (species_name in colnames(species_files)) {
    species_values <- species_files[, species_name]
    species_mean <- mean(species_values)
    species_std <- sd(species_values)
    
    cat("Species Name:", species_name, "\n")
    cat("Mean:", species_mean, "\n")
    cat("Standard Deviation:", species_std, "\n\n")
  }
}

# Step 3
create_histogram <- function(microbiome_data, species_name) {
  species_data <- microbiome_data$species
  species_values <- species_data[, species_name]
  
  hist(species_values, main = paste("Abundance Histogram for", species_name), xlab = "Abundance")
}

#step 4
filter_by_group <- function(microbiome_data, study_name) {
  metadata <- microbiome_data$metadata
  filtered_data <- metadata[metadata$Study_name == study_name, ]
  return(filtered_data)
}

# Step 5
subset_species_data <- function(microbiome_data, species_name) {
  species_file <- microbiome_data$species
  species_subset <- species_file[, c("SampleID", species_name)]
  return(species_subset)
}

# Step 6
MicrobiomeCalc <- function() {
  microbiome_data <- load_microbiome_data()
  
  while (TRUE) {
    cat("Microbiome Data Analysis Tool\n")
    cat("1. Display the data structure\n")
    cat("2. Calculate the mean and standard deviation for species\n")
    cat("3. Create a histogram for a specific species\n")
    cat("4. Filter data by a study name\n")
    cat("5. Extract data for a specific species\n")
    cat("0. Exit the program\n")
    
    choice <- as.numeric(readline("Enter your choice: "))
    
    if (choice == 0) {
      cat("Exiting the program.\n")
      break
    } else if (choice == 1) {
      str(microbiome_data)
    } else if (choice == 2) {
      calculate_mean_std(microbiome_data)
    } else if (choice == 3) {
      species_name <- readline("Enter the species name for the histogram: ")
      create_histogram(microbiome_data, species_name)
    } else if (choice == 4) {
      study_name <- readline("Enter the study name to filter data: ")
      filtered_data <- filter_by_group(microbiome_data, study_name)
      print(filtered_data)
    } else if (choice == 5) {
      species_name <- readline("Enter the species name to extract data: ")
      species_data <- subset_species_data(microbiome_data, species_name)
      print(species_data)
    } else {
      cat("Invalid choice. Please select a valid option.\n")
    }
  }
}

# Step 7: Run the MicrobiomeCalc Tool
MicrobiomeCalc()
calculate_mean_std(microbiome_data)
