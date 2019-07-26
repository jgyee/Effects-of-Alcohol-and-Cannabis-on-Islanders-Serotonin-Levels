

library(dplyr)
library(ggplot2)
experiment <- read.csv("data.101b - sample.csv", header=TRUE)

experiment$Sex[156] <- "Male"

experiment$Sex <- factor(experiment$Sex)

random <- rep(c(1:9), 11)

#Unshuffled Males
experiment[experiment$Sex=="Male",]


set.seed(20119)

#Shuffled Males
shuffled_males <- experiment[experiment$Sex=="Male",][sample(1:99, size = 99),]

shuffled_males <- shuffled_males %>% mutate(Order = as.integer(rownames(shuffled_males)), Group = random)

shuffled_males <- shuffled_males %>% arrange(Order)


#Unshuffled Females
experiment[experiment$Sex=="Female",]

shuffled_females <- experiment[experiment$Sex=="Female",][sample(1:99, size = 99),]

shuffled_females <- shuffled_females %>% mutate(Order = as.integer(rownames(shuffled_females)), Group = random)

shuffled_females <- shuffled_females %>% arrange(Order)


#Recombined Shuffled Data Frame In Correct Order

combined <- full_join(shuffled_males,shuffled_females) %>% arrange(Order)

write.csv(combined, file = "Shuffled_data.csv")

#Collected data

collected <- read.csv("data.101b - Shuffled_data.csv", header = TRUE)

collected <- collected %>% mutate(serotonin_change = a.serotonin - b.serotonin)

collected <- collected %>% mutate(Substance = if_else(collected$Group %in% 1:3, "Alcohol", if_else(collected$Group %in% 4:6, "Cannabis", "Control")))

collected$Substance <- factor(collected$Substance)

collected <- collected %>% mutate(Dosage = if_else(collected$Group %in% c(1,4,7), '1 drink', if_else(collected$Group %in% c(2,5,8), "2 drinks", "3 drinks")))

collected$Dosage <- factor(collected$Dosage)

collected$Group <- factor(collected$Group)


#final.csv <- write.csv(collected, file = "finalproject.csv")

#aov 2 way factorial, 1 blocking factor (sex)
#Randomized Complete Block Design


model <- aov(serotonin_change~ Substance+Dosage+Sex+Substance:Dosage, data = collected)

#summary of model
summary(model)


#Checking model assumptions
par(mfrow=c(2,2))
plot(model)

#interaction plot
par(mfrow=c(1,1))
with(collected, interaction.plot(Substance,Dosage,serotonin_change))


ggplot(collected, aes(x = Substance, color = Dosage, group = Dosage, y = serotonin_change))+
  stat_summary(fun.y = mean, geom="point") + stat_summary(fun.y = mean, geom = "line") + ylab("Serotonin Change")



#Boxplots
ggplot(data = collected, aes(x = Substance, y = serotonin_change)) + geom_boxplot(aes(fill = Substance, alpha = 0.7)) +
  geom_point(stat="summary", position = "dodge", fun.y = "mean")

ggplot(data = collected, aes(x = Dosage, y = serotonin_change, fill = Dosage, alpha = 0.7)) + geom_boxplot() +
geom_point(stat="summary", position = "dodge", fun.y = "mean")

#Dot plot of Means
ggplot(data = collected, aes(x = Substance, y = serotonin_change, color = Substance))+
  geom_point(stat="summary", position = "dodge", fun.y = "mean", size = 3)

ggplot(data = collected, aes(x = Dosage, y = serotonin_change, color = Dosage))+
  geom_point(stat="summary", position = "dodge", fun.y = "mean", size = 3)
