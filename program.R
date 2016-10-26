library(data.table)
library(dplyr)


############################################ Reading files
train_data <- fread(input = "C:\\Utilisateurs\\a578009\\Documents\\Kaggle\\Titanic_study\\Tatinic_exploration\\data\\train.csv", sep = ",")
test_data  <- fread(input = "C:\\Utilisateurs\\a578009\\Documents\\Kaggle\\Titanic_study\\Tatinic_exploration\\data\\test.csv", sep = ",")

############################################ data exploration
total_data <- bind_rows(train_data, test_data) %>% data.table()

# Getting title from name
# substitute with "" several characters until a "," and from a "." until serveral dots (.)
total_data$Title <- gsub('(.*, )|(\\..*)', '', total_data$Name)

table(total_data$Title)

# standarizing title variable
total_data[grepl("Capt", Title), Title := "Mr"]
total_data[Title == "Don", Title := "Mr"]
total_data[Title == "Dona", Title := "Mrs"]
total_data[Title == "Dr" & Sex == "male", Title := "Mr"]
total_data[Title == "Dr" & Sex == "female", Title := "Mrs"]
total_data[Title == "Lady", Title := "Mrs"]
total_data[Title == "Mme", Title := "Mrs"]
total_data[Title == "Sir", Title := "Mr"]
total_data[Title == "Jonkheer", Title := "Mr"]
total_data[Title %in% c("Col", "Major", "Rev", "the Countess"), Title := "Other"]
total_data[Title %in% c("Mlle", "Ms"), Title := "Miss"]

# Getting surname from name
total_data$Surname <- sapply(total_data$Name, function(x) strsplit(x, ",")[[1]][1], USE.NAMES = F)


