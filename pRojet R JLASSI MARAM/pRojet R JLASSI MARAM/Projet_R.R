#1.loading data
library(readr)
top_six_economies <- read_csv("C:\\Users\\wiemk\\OneDrive\\Bureau\\Projet_R\\top_six_economies.csv")
View(top_six_economies)

print(colnames(top_six_economies))
# Rename columns
names(top_six_economies)[names(top_six_economies) == 'GDP (current US$)'] <- 'GDP( $Bn )'
names(top_six_economies)[names(top_six_economies) == 'GDP, PPP (current international $)'] <- 'GDP, PPP ($Bn)'
names(top_six_economies)[names(top_six_economies) == 'Country Name'] <- 'Country'
names(top_six_economies)[names(top_six_economies) == 'Imports of goods and services (% of GDP)'] <- 'Imports ($Bn)'
names(top_six_economies)[names(top_six_economies) == 'Exports of goods and services (% of GDP)'] <- 'Exports ($Bn)'
names(top_six_economies)[names(top_six_economies) == 'Total reserves (includes gold, current US$)'] <- 'Total Reserves ($Bn)'
names(top_six_economies)[names(top_six_economies) == 'Unemployment, total (% of total labor force) (modeled ILO estimate)'] <- 'Unemployment'
names(top_six_economies)[names(top_six_economies) == 'Life expectancy at birth, total (years)'] <- 'Life expectancy (years)'
names(top_six_economies)[names(top_six_economies) == 'Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)'] <- 'Poverty'
names(top_six_economies)[names(top_six_economies) == 'Personal remittances, received (% of GDP)'] <- 'Remittances (% of GDP)'
names(top_six_economies)[names(top_six_economies) == 'Inflation,consumer prices (annual %)'] <- 'Inflation'
names(top_six_economies)[names(top_six_economies) == 'Central government debt, total (% of GDP)'] <- 'Debt'
print(colnames(top_six_economies))

#2.data overview
dim(top_six_economies)
head(top_six_economies)
str(top_six_economies)

#3.cleaning data 

#find null values
null_counts <- colSums(is.na(top_six_economies))
print(null_counts[null_counts != 0])

#Filling of Missing data using ffill and bfill (first with forward fill and then with backward fill)
# Load the 'zoo' package for na.locf() function
library(zoo)
#Fill missing values in 'Debt' column
top_six_economies$`Debt` <- na.locf(top_six_economies$`Debt`, fromLast = FALSE, na.rm = FALSE)
top_six_economies$`Debt` <- na.locf(top_six_economies$`Debt`, fromLast = TRUE, na.rm = FALSE)

# Fill missing values in 'Poverty' column
top_six_economies$Poverty <- na.locf(top_six_economies$Poverty, fromLast = FALSE, na.rm = FALSE)
top_six_economies$Poverty <- na.locf(top_six_economies$Poverty, fromLast = TRUE, na.rm = FALSE)
# Adjusting label values to countable form
top_six_economies$`GDP( $Bn )` <- top_six_economies$`GDP( $Bn )` / 1000000000
top_six_economies$`GDP, PPP ($Bn)` <- top_six_economies$`GDP, PPP ($Bn)` / 1000000000
top_six_economies$`Total Reserves ($Bn)` <- top_six_economies$`Total Reserves ($Bn)` / 1000000000
#Résumé des données
summary(top_six_economies)
str(top_six_economies)
#visualisation des boites à moustache 
boxplot(top_six_economies$Year)
boxplot(top_six_economies$`GDP( $Bn )`) 
boxplot(top_six_economies$`GDP, PPP ($Bn)`)
boxplot(top_six_economies$`GDP per capita (current US$)`)
boxplot(top_six_economies$`GDP growth (annual %)`)
boxplot(top_six_economies$`Imports ($Bn)`)
boxplot(top_six_economies$`Exports ($Bn)`)
boxplot(top_six_economies$Debt)
boxplot(top_six_economies$`Total Reserves ($Bn)`)
boxplot(top_six_economies$Unemployment)
boxplot(top_six_economies$`Inflation, consumer prices (annual %)`)
boxplot(top_six_economies$`Remittances (% of GDP)`)
boxplot(top_six_economies$`Population, total`)
boxplot(top_six_economies$`Population growth (annual %)`)
boxplot(top_six_economies$`Life expectancy (years)`)
boxplot(top_six_economies$Poverty)

#Visualisation et analyse

library(ggplot2)

#Mean GDP
# Calculate the mean GDP of countries in Billion Dollars
GDP <- aggregate(`GDP( $Bn )` ~ Country, data = top_six_economies, FUN = mean)
# Print the mean GDP of countries
print(GDP)
# Create a bar plot of the mean GDP
barplot(GDP$`GDP( $Bn )`, names.arg = GDP$Country, xlab = "Country", ylab = "Mean GDP ($Bn)", main = "Mean GDP of Countries")

#Mean GDP Per Capita
# Calculate the mean Per Capita Income (PCI) of countries in dollars
PCI <- aggregate(`GDP per capita (current US$)` ~ Country, data = top_six_economies, FUN = mean)
# Print the mean PCI of countries
print(PCI)
# Create a bar plot of the mean PCI
barplot(PCI$`GDP per capita (current US$)`, names.arg = PCI$Country, xlab = "Country", ylab = "Mean PCI (USD)", main = "Mean Per Capita Income of Countries")

#Mean GDP/PPP
# Calculate the mean GDP/PPP in Billion Dollars
GDP_PPP <- aggregate(`GDP, PPP ($Bn)` ~ Country, data = top_six_economies, FUN = mean)
# Print the mean GDP/PPP in Billion Dollars for each country
print(GDP_PPP)
# Create a bar plot of the mean GDP/PPP
barplot(GDP_PPP$`GDP, PPP ($Bn)`, names.arg = GDP_PPP$Country, xlab = "Country", ylab = "Mean GDP/PPP ($Bn)", main = "Mean GDP/PPP of Countries")

#Imports by decade for all countries

# Getting List of Countries, Start Year, and Last Year
Countries <- unique(top_six_economies$Country)
Start_year <- min(top_six_economies$Year)
Last_year <- max(top_six_economies$Year)
# Load required libraries
library(dplyr)
library(ggplot2)
# Initialize an empty list to store dataframes
Tab <- list()
# Initialize a counter for colors
y <- 1
# Define a vector of colors
colors <- c("red", "green", "blue", "black", "yellow", "magenta")
# Loop through each country
for (i in Countries) {
  # Filter data for the current country
  x <- top_six_economies %>%
    filter(Country == i) 
  # Calculate mean imports for each decade
  y1 <- mean(x$`Imports ($Bn)`[x$Year <= (Start_year + 9)])
  y2 <- mean(x$`Imports ($Bn)`[x$Year > (Start_year + 9) & x$Year <= (Start_year + 19)])
  y3 <- mean(x$`Imports ($Bn)`[x$Year > (Start_year + 19) & x$Year <= (Start_year + 29)])
  # Create a dataframe for each country and decade
  df <- data.frame(
    Country = i,
    "1991-2000" = y1,
    "2001-2010" = y2,
    "2011-2020" = y3
  )
  # Append the dataframe to the list
  Tab[[y]] <- df
  # Increment the color counter
  y <- y + 1
}
# Combine all dataframes into one
Imports <- do.call(rbind, Tab)
# Plotting
Imports_long <- tidyr::pivot_longer(Imports, -Country)
ggplot(Imports_long, aes(x = name, y = value, color = Country, group = Country)) +
  geom_line() +
  labs(x = "Decade", y = "Mean Imports ($Bn)", title = "Imports by Decade") +
  scale_color_manual(values = colors) +
  theme_minimal()


#Exports by decade for all the countries
# Load required libraries
library(dplyr)
library(ggplot2)
# Define colors
colors <- c("red", "green", "blue", "black", "yellow", "magenta")
# Initialize an empty list to store dataframes
Tab <- list()
# Initialize a counter for colors
y <- 1
# Loop through each country
for (i in Countries) {
  # Filter data for the current country
  x <- top_six_economies %>%
    filter(Country == i)
  # Calculate mean exports for each decade
  y1 <- mean(x$`Exports ($Bn)`[x$Year <= (Start_year + 9)])
  y2 <- mean(x$`Exports ($Bn)`[x$Year > (Start_year + 9) & x$Year <= (Start_year + 19)])
  y3 <- mean(x$`Exports ($Bn)`[x$Year > (Start_year + 19) & x$Year <= (Start_year + 29)])
  # Create a dataframe for each country and decade
  df <- data.frame(
    Country = i,
    "1991-2000" = y1,
    "2001-2010" = y2,
    "2011-2020" = y3
  )
  # Append the dataframe to the list
  Tab[[y]] <- df
  # Increment the color counter
  y <- y + 1
}
# Combine all dataframes into one
Exports <- do.call(rbind, Tab)
# Reshape data from wide to long format
Exports_long <- tidyr::pivot_longer(Exports, cols = -Country)
# Plotting
ggplot(Exports_long, aes(x = name, y = value, color = Country, group = Country)) +
  geom_line() +
  labs(x = "Decade", y = "Mean Exports ($Bn)", title = "Exports by Decade") +
  scale_color_manual(values = colors) +
  theme_minimal()

#years of recession for each country
# Loop through each country
for (C in unique(top_six_economies$Country)) {
  # Find the year and GDP with the minimum value
  x_min <- min(top_six_economies[top_six_economies$Country == C, "GDP( $Bn )"])
  z_min <- top_six_economies[top_six_economies$`GDP( $Bn )` == x_min, "Year"]
  print(paste("The Year of Recession for", C, "was in year", z_min, "with GDP", x_min))
  
  # Find the year and GDP with the maximum value
  x_max <- max(top_six_economies[top_six_economies$Country == C, "GDP( $Bn )"])
  z_max <- top_six_economies[top_six_economies$`GDP( $Bn )` == x_max, "Year"]
  print(paste("The Year with maximum GDP for", C, "was in year", z_max, "with GDP", x_max))
  
  print("******************************")
}

#Correlation
# Select the specific variables for which you want to compute the correlation matrix
selected_variables <- top_six_economies[, c("GDP( $Bn )", "GDP per capita (current US$)", "GDP growth (annual %)", "Imports ($Bn)", "Exports ($Bn)", "Debt", "Total Reserves ($Bn)", "Unemployment", "Inflation, consumer prices (annual %)", "Population, total", "Life expectancy (years)", "Poverty")]
# Compute the correlation matrix
correlation_matrix <- cor(selected_variables)
print(correlation_matrix)
# Load required libraries
library(corrplot)
# Visualize correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper")
# Sélectionner les variables avec une corrélation significative avec la variable cible
significant_variables <- names(which(abs(correlation_matrix["GDP per capita (current US$)", ]) > 0.5))
print(significant_variables)

#After seeing the corr matrix it's interesting to explore further relationship between

# Filter data for China and the United States (we'll take these two as an example)
china_data <- top_six_economies[top_six_economies$Country == "China", ]
us_data <- top_six_economies[top_six_economies$Country == "United States", ]


#A-Imports and exports ( correlation presque 1)

# Plot exports and imports over time for China
ggplot() +
  geom_line(data = china_data, aes(x = Year, y = `Exports ($Bn)`, color = "Exports")) +
  geom_line(data = china_data, aes(x = Year, y = `Imports ($Bn)`, color = "Imports")) +
  labs(x = "Year", y = "Value ($Bn)", color = "Variable") +
  scale_color_manual(values = c("Exports" = "blue", "Imports" = "red")) +
  ggtitle("Exports and Imports Over Time - China")

# Plot exports and imports over time for the United States
ggplot() +
  geom_line(data = us_data, aes(x = Year, y = `Exports ($Bn)`, color = "Exports")) +
  geom_line(data = us_data, aes(x = Year, y = `Imports ($Bn)`, color = "Imports")) +
  labs(x = "Year", y = "Value ($Bn)", color = "Variable") +
  scale_color_manual(values = c("Exports" = "blue", "Imports" = "red")) +
  ggtitle("Exports and Imports Over Time - United States")

#B-Life Expectancy and GDP Per Capita ( high corr pos)

# Load required libraries
library(ggplot2)

# Scatter plot with trend line for China
ggplot(china_data, aes(x = `GDP per capita (current US$)`, y = `Life expectancy (years)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "GDP Per Capita ($)", y = "Life Expectancy (years)",
       title = "Relationship between GDP Per Capita and Life Expectancy (China)")

# Scatter plot with trend line for the United States
ggplot(us_data, aes(x = `GDP per capita (current US$)`, y = `Life expectancy (years)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "GDP Per Capita ($)", y = "Life Expectancy (years)",
       title = "Relationship between GDP Per Capita and Life Expectancy (United States)")

#C-Poverty and GDP Per Capita (high corr neg presque -1)

# Scatter plot with trend line for China
ggplot(china_data, aes(x = `GDP per capita (current US$)`, y = `Poverty`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "GDP Per Capita ($)", y = "`Poverty`",
       title = "Relationship between GDP Per Capita and Poverty (China)")

#D-Poverty and Life expectancy (high corr neg presque -1)
library(ggplot2)
# Scatter plot with trend line for China
ggplot(china_data, aes(x = Poverty , y = `Life expectancy (years)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Poverty (%)", y = "Life Expectancy (years)",
       title = "Relationship between Life Expectancy (years) and Poverty (China)")

#4* Prediction

# Charger les bibliothèques nécessaires
install.packages("forecast")
library(forecast)
library(base)
data_df <- data.frame(Year = top_six_economies$Year+10, GDP = top_six_economies$`GDP( $Bn )`)

# Diviser les données en ensemble d'entraînement et ensemble de test
train_data <- subset(data_df, Year <= 2020)
test_data <- subset(data_df, Year > 2020)

# Ajuster un modèle de régression linéaire
lm_model <- lm(GDP ~ Year, data = train_data)

# Faire des prédictions sur l'ensemble de test
predictions <- predict(lm_model, newdata = test_data)

# Tracer les prédictions
plot(data_df$Year, data_df$GDP, main="prediction pour GDP($Bn)",type = "l", col = "blue", xlab = "Year", ylab = "GDP ($Bn)")
lines(test_data$Year, predictions, col = "red")
legend("topleft", legend = c("Données réelles", "Prédictions"), col = c("blue", "red"), lty = c(1, 1))


library(base)
data_df <- data.frame(Year = top_six_economies$Year+10, inflation = top_six_economies$`Inflation, consumer prices (annual %)`)

# Diviser les données en ensemble d'entraînement et ensemble de test
train_data <- subset(data_df, Year <= 2020)
test_data <- subset(data_df, Year > 2020)

# Ajuster un modèle de régression linéaire
lm_model <- lm(inflation ~ Year, data = train_data)

# Faire des prédictions sur l'ensemble de test
predictions <- predict(lm_model, newdata = test_data)

# Tracer les prédictions
plot(data_df$Year, data_df$inflation, main="prediction pour l'inflation",type = "l", col = "blue", xlab = "Year", ylab = "Inflation, consumer prices (annual %)")
lines(test_data$Year, predictions, col = "red")
legend("topleft", legend = c("Données réelles", "Prédictions"), col = c("blue", "red"), lty = c(1, 1))



library(base)
data_df <- data.frame(Year = top_six_economies$Year+10, Exports = top_six_economies$`Exports ($Bn)`)

# Diviser les données en ensemble d'entraînement et ensemble de test
train_data <- subset(data_df, Year <= 2020)
test_data <- subset(data_df, Year > 2020)

# Ajuster un modèle de régression linéaire
lm_model <- lm(Exports ~ Year, data = train_data)

# Faire des prédictions sur l'ensemble de test
predictions <- predict(lm_model, newdata = test_data)

# Tracer les prédictions
plot(data_df$Year, data_df$Exports, main="prediction pour les exportations",type = "l", col = "blue", xlab = "Year", ylab = "Exports($Bn)")
lines(test_data$Year, predictions, col = "red")
legend("topleft", legend = c("Données réelles", "Prédictions"), col = c("blue", "red"), lty = c(1, 1))




library(base)
data_df <- data.frame(Year = top_six_economies$Year+10, Imports = top_six_economies$`Imports ($Bn)`)

# Diviser les données en ensemble d'entraînement et ensemble de test
train_data <- subset(data_df, Year <= 2020)
test_data <- subset(data_df, Year > 2020)

# Ajuster un modèle de régression linéaire
lm_model <- lm(Imports ~ Year, data = train_data)

# Faire des prédictions sur l'ensemble de test
predictions <- predict(lm_model, newdata = test_data)

# Tracer les prédictions
plot(data_df$Year, data_df$Imports, main="prediction pour les importations",type = "l", col = "blue", xlab = "Year", ylab = "Imports($Bn)")
lines(test_data$Year, predictions, col = "red")
legend("topleft", legend = c("Données réelles", "Prédictions"), col = c("blue", "red"), lty = c(1, 1))

