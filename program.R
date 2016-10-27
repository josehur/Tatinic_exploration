library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes) # visualization
library(scales) # visualization
library(mice) #imputation
library(randomForest) #imputation


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

# Create a family size variable including the passenger themselves
total_data$Family_size <- total_data$SibSp + total_data$Parch + 1

# family variable
total_data$Family <- paste(total_data$Surname, total_data$Family_size, sep='_')

# relationship between family size and survivals
ggplot(total_data[!is.na(Survived)], aes(x = Family_size, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:13)) +
  labs(x = 'Family Size') 

# Discretize family size
total_data$Family_size_d[total_data$Family_size == 1] <- 'singleton'
total_data$Family_size_d[total_data$Family_size < 5 & total_data$Family_size > 1] <- 'small'
total_data$Family_size_d[total_data$Family_size > 4] <- 'large'

total_data$PassengerId <- as.character(total_data$PassengerId)
total_data$Survived <- as.factor(total_data$Survived)
total_data$Pclass <- as.factor(total_data$Pclass)
total_data$Sex <- as.factor(total_data$Sex)
total_data$Embarked <- as.factor(total_data$Embarked)
total_data$Family_size_d <- as.factor(total_data$Family_size_d)

summary(total_data)

# correcion of embarque variable "" (passangers 62 and 830)
total_data[Embarked == ""]

# Get rid of our missing passenger IDs
embark_fare <- total_data %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2)

total_data[PassengerId == "62" | PassengerId == "830", Embarked := "C"]

# estimating fare dor passanger 1044
total_data[PassengerId == "1044"]

ggplot(total_data[total_data$Pclass == '3' & total_data$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1)

total_data[PassengerId == "1044",]$Fare <- total_data[total_data$Pclass == '3' & total_data$Embarked == 'S', ]$Fare %>% median(., na.rm = T)

# Age imputation
str(total_data)

total_data$Title <- as.factor(total_data$Title)
total_data$Surname <- as.factor(total_data$Surname)
total_data$Family <- as.factor(total_data$Family)
total_data$Family_size_d <- as.factor(total_data$Family_size_d)


# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(total_data[, !names(total_data) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived'), with = F],
                 method='rf') 


mice_mod$imp$Age

# Save the complete output 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(total_data$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))


# par(mfrow=c(1,2))
# hist(total_data$Age, freq=T, main='Age: Original Data', 
#      col='darkgreen')
# hist(mice_output$Age, freq=T, main='Age: MICE Output', 
#      col='lightgreen', )

# Replace Age variable from the mice model.
total_data$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(total_data$Age))

# First we'll look at the relationship between age & survival
ggplot(total_data[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

# Create the column child, and indicate whether child or adult
total_data$Child[total_data$Age < 18] <- 'Child'
total_data$Child[total_data$Age >= 18] <- 'Adult'

# Show counts
table(total_data$Child, total_data$Survived)


# Adding Mother variable
total_data$Mother <- 'Not Mother'
total_data$Mother[total_data$Sex == 'female' & total_data$Parch > 0 & total_data$Age > 18 & total_data$Title != 'Miss'] <- 'Mother'

# Show counts
table(total_data$Mother, total_data$Survived)


# Finish by factorizing our two new factor variables
total_data$Child  <- factor(total_data$Child)
total_data$Mother <- factor(total_data$Mother)

md.pattern(total_data)

######################################## Prediction

# Split the data back into a train set and a test set
train <- total_data[1:891,]
test <- total_data[892:1309,]


set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           Family_size_d + Child + Mother,
                         data = train)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
