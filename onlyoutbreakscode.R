library(dplyr)

# Read the original Salmonella data
salmonella_data <- read.csv("Salmonella_May.csv", stringsAsFactors = FALSE)

# Filter to keep only samples with valid outbreak codes (not NA and not "N/A")
outbreak_only <- salmonella_data %>%
  filter(!is.na(Outbreak) & Outbreak != "N/A")

# Write the filtered data to a new CSV file
write.csv(outbreak_only, "Salmonella_Outbreaks_Only.csv", row.names = FALSE)

# Print summary information
cat("Original dataset:", nrow(salmonella_data), "records\n")
cat("Outbreak-only dataset:", nrow(outbreak_only), "records\n")
cat("Outbreak codes included:", length(unique(outbreak_only$Outbreak)), "unique codes\n")
cat("Filtered dataset saved as: Salmonella_Outbreaks_Only.csv\n")

# Print the first few outbreak codes as a sample
outbreak_codes <- unique(outbreak_only$Outbreak)
cat("Sample outbreak codes:\n")
cat(paste(outbreak_codes[1:min(10, length(outbreak_codes))], collapse = ", "), "\n")