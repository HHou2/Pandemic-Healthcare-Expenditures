# Harry Hou
# May 2023
# Pandemic Healthcare Expenditures

# Set Up File Paths
library(tidyverse)

base_path <- "C:/Users/h4rry/Documents/SCHOOL/SSQL Social Sciences Quantitative Laboratory/Pandemic Healthcare Expenditures/mtbi files"

files_for_import <- c("mtbi181x.csv",
                      "mtbi182.csv",
                      "mtbi183.csv",
                      "mtbi184.csv",
                      "mtbi191.csv",
                      "mtbi192.csv",
                      "mtbi193.csv",
                      "mtbi194.csv",
                      "mtbi201.csv",
                      "mtbi202.csv",
                      "mtbi203.csv",
                      "mtbi204.csv",
                      "mtbi211.csv",
                      "mtbi212.csv",
                      "mtbi213.csv",
                      "mtbi214.csv",
                      "mtbi221.csv",
                      "mtbi222.csv",
                      "mtbi223.csv") # names of files

# files_for_import <- c("mtbi223.csv") # FOR TESTING SMALLER DATA

# Function That Reads Files for Specific Information
files <- c()

for(file in files_for_import) {
  files <- append(files, file)
}

expenditure_data <- tibble()

specific_columns <- c("NEWID", "REF_MO", "REF_YR", "UCC", "COST")

for(file in files) {
  expenditure_data <- expenditure_data %>%
    bind_rows(read_csv(file.path(base_path, file), col_types = cols(
      NEWID = col_character(), 
      REF_MO = col_character(), 
      REF_YR = col_double(), 
      UCC = col_character(), 
      COST = col_double()
      )) %>%
        select(all_of(specific_columns)))
}

# # ALL UCC Code Targets and Targets Labels
# UCC_targets <- c("540000", "550110", "550320", "550330", "550340",
#                  "560110", "560210", "560310", "560330", "560410", 
#                  "560420", "570111", "570220", "570230", "570901", 
#                  "570903")
# 
# UCC_targets_labels <- c("Prescription Drugs", "Eyeglasses / Contacts", "Medical Equip. (General Use)",
#                         "Supportive Med. Equip.", "Hearing Aids", "Physicians Services", 
#                         "Dental Services", "Eyecare Services", "Lab Tests, X-Rays", 
#                         "Non Physcns Servc Inside Home", "Non Physcns Servc Outside Home", "Hospital Rooms / Services", 
#                         "Convl / Nursing Home Care", "Other Med. Care Service", "Med./Surgical Equip Rental", 
#                         "Supportive/Conval Equip Rental")

# Selected UCC Code Targets and Targets Labels
UCC_targets <- c("560330", "540000", "560310")

UCC_targets_labels <- c("Lab Tests, X-Rays", "Prescription Drugs", "Eyecare Services")

# Filter expenditure_data based on UCC Targets
filter_by_UCC <- function(expenditure_data, UCC_targets) {
  expenditure_data %>%
    filter(UCC %in% UCC_targets)
}

expenditure_data <- expenditure_data %>%
  filter_by_UCC(UCC_targets)

# Extract Month and Year
month <- expenditure_data$REF_MO
month <- formatC(month, width = 2, flag = "0") # ensures values from 1 to 9 are 01 to 09
year <- expenditure_data$REF_YR
day <- "01"

month <- factor(month, levels = c("01", "02", "03", "04", "05", 
                                  "06", "07", "08", "09", "10", 
                                  "11", "12"))

# Create Date Variable Convert Format
date_str <- paste(year, month, day, sep = "-")
formatted_date <- as.Date(date_str, format = "%Y-%m-%d")

# Add Formatted Date Column
expenditure_data$DATE <- formatted_date

# Compute Monthly Costs
expenditure_data <- expenditure_data %>%
  group_by(DATE, UCC) %>%
  summarise(avg_cost = mean(COST), med_cost = median(COST)) %>% # average and median costs computed
  mutate(UCC = factor(UCC, levels = UCC_targets, labels = UCC_targets_labels))

# Plot the Monthly Costs
ggplot(data = expenditure_data, aes(x = DATE, y = avg_cost, fill = UCC)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Time", 
       y = "Cost", 
       title = "Average Monthly Healthcare Expenditures", 
       fill = "Expenditure") +
  scale_fill_viridis_d(option = "viridis", direction = -1, begin = 0, end = 1) +
  theme_classic()

ggplot(data = expenditure_data, aes(x = DATE, y = avg_cost, fill = UCC)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Time", 
       y = "Cost", 
       title = "Average Monthly Healthcare Expenditures", 
       fill = "Expenditure") +
  scale_fill_viridis_d(option = "viridis", direction = -1, begin = 0, end = 1) +
  theme_classic()

ggplot(data = expenditure_data, aes(x = DATE, y = avg_cost, color = UCC)) +
  geom_line() +
  labs(x = "Time", 
       y = "Cost", 
       title = "Average Monthly Healthcare Expenditures", 
       color = "Expenditure") +
  scale_color_viridis_d(option = "viridis", direction = -1, begin = 0, end = 1) + 
  theme_classic()
