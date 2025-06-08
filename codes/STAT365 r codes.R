setwd("C:/Users/tagii/Downloads")
getwd()

install.packages("readxl")
data <- read_excel("caffein.xlsx")
library(readxl)
library(dplyr)
head(data)
str(data)
unique_values <- lapply(data, unique); unique_values
unique_dep <- unique(data$`What is your department?
`); unique_dep
data$`What is your department?
`<- tolower(data$`What is your department?
  `);lower_case_dep
unique(data$`What is your department?
         `)
data$dep <- tolower(data$`What is your department?
`)
data$dep
dep_map <- c(
  "food engineering" ="fde",
  "physics"               ="phys",
  "foreign language education"  ="fle",
  "mechanical engineering"           ="me",
  "elemantary mathematics educational"     ="eme",
  "elementary mathematic education"               ="eme",
  "elementary mathematics education"                = "eme",
  "elemantry math education"                         ="eme",
  "politics"                                         ="padm",
  "eee"                                              ="eee",
  "early childhood education"                        ="ece",
  "elementry science education"                      ="ese",
  "political science and public administration"      ="padm",
  "economics"                                        ="econ",
  "psychology"                                       ="psy",
  "petroleum and natural gas engineering"            ="pete",
  "chemical engineering"                             ="che",
  "biology"                                          ="bio",
  "political science and public adm"                 ="padm",
  "metallurgical and materials engineering"          ="mete",
  "ie(industrial engineering)"                       ="ie",
  "international relations"                          ="ir",
  "molecular biology and genetics"                   ="mole",
  "civil engineering"                                ="ce",
  "endustriyel tasarim"                              ="id",
  "computer engineering"                             ="ceng",
  "business administration"                          ="ba",
  "philosophy"                                       ="phil",
  "statistics"                                       ="stat",
  "aee"                                              ="aee",
  "fle"                                              ="fle",
  "architecture"                                     ="arch",
  "math"                                             ="math",
  "chem"                                             ="chem",
  "uluslararasi iliskiler"                           ="ir",
  "political science"                                ="padm",
  "global international affairs"                     ="gia",
  "business administration suny"                     ="ba",
  "ceng"                                             ="ceng",
  "industrial design"                                ="id",
  "ched"                                             ="ched",
  "computer education and instructional technologies"="ceit",
  "industrial engineering"                           ="ie",
  "fde"                                              ="fde",
  "mimarlD1k"                                         ="arch",
  "che"                                              ="che",
  "mining engineering"                               ="mine",
  "architecturr"                                     ="arch",
  "electric and electronics engineerD1ng"             ="eee",
  "fizik"                                            ="phys",
  "metallD1rgical and material enginering"            ="mete",
  "electrical and electronics engineering"           ="eee",
  "electric electronics engineering"                 ="eee",
  "electical and electronics engineering"            ="eee",
  "me"                                               ="me",
  "industrial enginnering"                           ="ie",
  "mathematics"                                      ="math",
  "D1ndustrial design"                                ="id",
  "computer education and instructional technology"  ="ceit",
  "chemistry"                                        ="chem",
  "ie"                                               ="ie",
  "environmental engineering"                        ="enve",
  "stat"                                             ="stat",
  "ee"                                               ="eee",
  "elementary math education"                        ="eme",
  "biol"                                             ="bio",
  "electrics and electronics engineering"            ="eee",
  "istatistik"                                       ="stat",
  "mechanical engineer"                              ="me",
  "metalurji ve malzeme mC<hendislio?=\u009fi"          ="mete",
  "bilgisayar mC<hendislio?=\u009fi"                          ="ceng",
  "geoe"                                             ="geoe",
  "history"                                          ="hist",
  "geological engineering"                           ="geoe",
  "elemantary mathematics education"                 ="eme",
  "mechanical enginering"                            ="me",
  "bussines administration"                          ="ba",
  "computer engineer"                                ="ceng",
  "aerospace engineering"                            ="ae",
  "adm"                                              ="adm",
  "ce"                                               ="ce",
  "sosyoloji"                                        ="soc",
  "eme"                                              ="eme",
  "elementary science education"                     ="ese",
  "city and regional planning"                       ="crp",
  "elektronik"                                       ="eee",
  "sociology"                                        ="soc",
  "civil engineerin"                                 ="ce",
  "aerospace"                                        ="ae",
  "pete"                                             ="pete",
  "phil"                                             ="phil",
  "aerospace engineer"                               ="ae",
  "metallurgical and material engineering"           ="mete",
  "mhed"                                             ="eme",
  "havacD1lD1k ve uzay mC<hendislio?=\u009fi"        ="ae",
  "havacD1lD1k ve uzay mC<hendisliD i" = "ae",
  "Bilgisayar mC<hendisliD i" = "ceng",
  "Metalurji ve Malzeme MC<hendisliD i"= "mete"
)
data$dep <- dep_map[match(data$dep, names(dep_map))]
data
unique(data$dep)
dep_names <- data[, c("What is your department?\n", "dep")]
dep_names
colnames(data)
print(dep_names, n=322)
#rm(data)
na_rows_dep <- data[is.na(data$dep), ]
print(na_rows_dep)
View(na_rows_dep)


data[172,]$dep <- "mete"
View(data[172,])
data[174,]$dep <- "ceng"
View(data)
install.packages("writexl")
library(writexl)

# Temizlenmiş veriyi kaydet
write_xlsx(data, "cleaned_data.xlsx")
write_xlsx(data, "C:/Users/tagii/Downloads/cleaned_data.xlsx")

df <- read_excel('cleaned_data.xlsx')
View(df)
unique(df$`What is your main source of caffeine?`)
df$`What is your main source of caffeine?` =='cikolata falan'
df[65,]
print(df[65,])
rm(df)
df$`Which side effects do you experience after consuming caffeine?`
data_with_insomnia <- df[grepl("Insomnia", df$`Which side effects do you experience after consuming caffeine?`), ]
data_with_insomnia

str(df)









calculate_total_caffeine <- function(coffee, tea, matcha, energy_drinks, chocolate = 0) {
  # Define caffeine values per unit (mg)
  caffeine_values <- list(
    coffee = 95,
    tea = 47,
    matcha = 70,
    energy_drinks = 80,  # Adjust as needed
    chocolate = 10
  )
  
  # Calculate total caffeine intake
  total_caffeine <- (coffee * caffeine_values$coffee) +
    (tea * caffeine_values$tea) +
    (matcha * caffeine_values$matcha) +
    (energy_drinks * caffeine_values$energy_drinks) +
    (chocolate * caffeine_values$chocolate)
  return(total_caffeine)
}
library(dplyr)


df <- df %>%
  mutate(
    total_caffeine = calculate_total_caffeine(
      `Please rate your daily caffeine consumption for each category. [Coffee]`,
      `Please rate your daily caffeine consumption for each category. [Tea]`,
      `Please rate your daily caffeine consumption for each category. [Matcha/Green tea]`,
      `Please rate your daily caffeine consumption for each category. [Energy drinks]`
    )
  )


# Load necessary libraries
library(dplyr)

# Load the dataset (update the file path with your dataset location)
file_path <- "cleaned_data.xlsx"  # Replace with your file path
library(readxl)
df <- read_excel(file_path)

# Rename columns for easier handling
df <- df %>%
  rename(
    Coffee = `Please rate your daily caffeine consumption for each category. [Coffee]`,
    Tea = `Please rate your daily caffeine consumption for each category. [Tea]`,
    Green_tea = `Please rate your daily caffeine consumption for each category. [Matcha/Green tea]`,
    Energy_Drinks = `Please rate your daily caffeine consumption for each category. [Energy drinks]`
  )

# Define caffeine content (mg) per unit for each category
caffeine_content <- c(
  Coffee = 202.5,
  Tea = 55,
  Green_tea = 32,
  Energy_Drinks = 80,
  Coke=34
)

# Calculate total caffeine consumption for each participant
df<- df %>%
  mutate(
    Total_Caffeine = (Coffee * caffeine_content["Coffee"]) +
      (Tea * caffeine_content["Tea"]) +
      (Green_tea * caffeine_content["Green_tea"]) +
      (Energy_Drinks * caffeine_content["Energy_Drinks"])
  )

# View the first few rows of the updated dataset
print(df %>% select(Coffee, Tea, Green_tea, Energy_Drinks, Total_Caffeine))
colnames(df)

write_xlsx(df, "cleaned_data3.xlsx")
write_xlsx(df, "C:/Users/tagii/Downloads/cleaned_data3.xlsx")
sort(table(df$dep))
table(df$dep)
head(df)



# Create a data frame with departments and their counts
departments <- data.frame(
  Department = c( "aee", "arch", "ba", "bio", "ce", "ceit", "ceng", 
                 "che", "ched", "chem", "crp", "ece", "econ", "eee", "eme", "enve", 
                 "ese", "fde", "fle", "geoe", "hist", "id", "ie", "ir", 
                 "math", "me", "mete", "mine", "mole", "padm", "pete", "phil", 
                 "phys", "psy", "soc", "stat"),
  Count=(c(5,21,5,11,25,6,20,15,1,8,1,5,12,21,13,3,2,6,9,5,1,8,11,4,7,15,7,2,2,15,5,4,6,9,2,32))
)

# Define the faculty mapping
faculty_mapping <- list(
  Engineering = c("aee", "ceng", "che", "enve", "geoe", "ie", "me", "mete", "mine", "pete", "fde",'ce','eee'),
  Arts_and_Sciences = c("chem", "hist", "math", "phil", "phys", "psy", "soc", "stat", "bio", "mole"),
  Education = c("ceit", "ece", "ese", "fle",'ched','eme'),
  Architecture = c("id", "arch", "crp"),
  Economics_and_Administrative_Sciences=c('padm','ba','econ','ir')
)

# Assign faculties based on the mapping
departments$Faculty <- sapply(departments$Department, function(dept) {
  if (dept %in% faculty_mapping$Engineering) {
    return("Engineering")
  } else if (dept %in% faculty_mapping$Arts_and_Sciences) {
    return("Arts and Sciences")
  } else if (dept %in% faculty_mapping$Education) {
    return("Education")
  } else if (dept %in% faculty_mapping$Architecture) {
    return("Architecture")
  } else if (dept %in% faculty_mapping$Economics_and_Administrative_Sciences){
    return("Economics and Administrative Sciences")
  } else  {
    return(NA)  # Assign NA if the department doesn't match any faculty
  }
})

# View the updated data frame with faculties
print(departments)
df<- df %>%
  mutate(
    Faculty
  )

# Optional: Save the updated data frame to an Excel file
library(writexl)
colnames(df)
df<-read_excel('cleaned_data4.xlsx')
write_xlsx(data, "cleaned_data4.xlsx")
