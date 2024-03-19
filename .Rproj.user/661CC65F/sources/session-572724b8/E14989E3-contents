library(tidyverse)
PatientID <- 1:60
Age <- rpois(60, 45)
Treatment <- rep(c("Control", "Medication X"), each=30)
BloodPressureBefore <- round(130 + rnorm(60, 3*(Age - 45)/sqrt(45), 2), 1)
BloodPressureAfter <- round(rnorm(60, BloodPressureBefore - 7*(Treatment == "Medication X"), 3), 1)
Headache <- c("No", "Yes")[1 + rbinom(60, 1, (Treatment == "Control")*0.1 + (Treatment != "Control")*0.25)]

data <- tibble(`Patient ID` = PatientID,
               Age = Age,
               Treatment = Treatment,
               `Baseline SBP` = BloodPressureBefore,
               `Post Treatment SBP` = BloodPressureAfter,
               Headache = Headache)

write_csv(data, file = "data/HypotheticalData1.csv")
foo = read_csv("data/HypotheticalData1.csv")


tab = table(data$Headache, data$Treatment)

chisq.test(tab)$p.value
fisher.test(tab)$p.value
print("hey")



# Descriptive Statistics
hist(data$Age)
boxplot(data$Age)
summary(data$Age)
table(data$Headache, data$Treatment)

# Make a new column
data$`SBP Diff` = data$`Baseline SBP` - data$`Post Treatment SBP`

boxplot(BloodPressureAfter~Treatment, 
        col=c("orange", "dodgerblue"))

ggplot(data, aes(x = Treatment, y = `SBP Diff`, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Box Plot of Blood Pressure Grouped by Treatment",
       x = "Treatment Group",
       y = "Change in Blood Pressure (mmHg)") 

# Do a statistical test
index_control <- which(data$Treatment == "Control")
index_medx <- which(data$Treatment == "Medication X")

x1 = data$`SBP Diff`[index_control]
x2 = data$`SBP Diff`[index_medx]

t.test(x1, x2)





