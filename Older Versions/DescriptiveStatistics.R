setwd("/Users/maia/Documents")
data<-read.csv("MaiaData_CardSort.csv")

data$Attention <- ifelse(data$BiasScore > 0.5 | data$BiasScore < -0.5, "Biased", "Neutral")
data$IPS <- ifelse(data$BiasScore > 0, "Verbal", "Visual")

#Reaction times
#Biased attender histograms and descriptive statistics
biased_data<- data[data$Attention == "Biased", ]

#InConRT
biased_incon_rt_mean<- mean(biased_data$InCon_RT)
biased_incon_rt_std<- sd(biased_data$InCon_RT)
biased_incon_rt_min<- min(biased_data$InCon_RT)
biased_incon_rt_max<- max(biased_data$InCon_RT)
hist(biased_data$InCon_RT, breaks=15, main="Histogram of Biased Attender Incongruent Trial Reaction Times", xlab="Time (ms)")

#ConRT
biased_con_rt_mean<- mean(biased_data$Con_RT)
biased_con_rt_std<- sd(biased_data$Con_RT)
biased_con_rt_min<- min(biased_data$Con_RT)
biased_con_rt_max<- max(biased_data$Con_RT)
hist(biased_data$Con_RT, breaks=15, main="Histogram of Biased Attender Congruent Trial Reaction Times", xlab="Time (ms)")

#Neutral attender histograms and descriptive statistics
neutral_data<- data[data$Attention == "Neutral", ]

#InConRT
neutral_incon_rt_mean<- mean(neutral_data$InCon_RT)
neutral_incon_rt_std<- sd(neutral_data$InCon_RT)
neutral_incon_rt_min<- min(neutral_data$InCon_RT)
neutral_incon_rt_max<- max(neutral_data$InCon_RT)
hist(neutral_data$InCon_RT, breaks=15, main="Histogram of Neutral Attender Incongruent Trial Reaction Times", xlab="Time (ms)")

#ConRT
neutral_con_rt_mean<- mean(neutral_data$Con_RT)
neutral_con_rt_std<- sd(neutral_data$Con_RT)
neutral_con_rt_min<- min(neutral_data$Con_RT)
neutral_con_rt_max<- max(neutral_data$Con_RT)
hist(neutral_data$Con_RT, breaks=15, main="Histogram of Neutral Attender Congruent Trial Reaction Times", xlab="Time (ms)")

#Dataframe for descriptive statistics
biased_descriptive <- data.frame(
  Attention = "Biased",  
  Variable = c("InCon_RT", "Con_RT"),
  Mean = c(biased_incon_rt_mean, biased_con_rt_mean),
  StdDev = c(biased_incon_rt_std, biased_con_rt_std),
  Min = c(biased_incon_rt_min, biased_con_rt_min),
  Max = c(biased_incon_rt_max, biased_con_rt_max),
  stringsAsFactors = FALSE
)

neutral_descriptive <- data.frame(
  Attention = "Neutral",  
  Variable = c("InCon_RT", "Con_RT"),
  Mean = c(neutral_incon_rt_mean, neutral_con_rt_mean),
  StdDev = c(neutral_incon_rt_std, neutral_con_rt_std),
  Min = c(neutral_incon_rt_min, neutral_con_rt_min),
  Max = c(neutral_incon_rt_max, neutral_con_rt_max),
  stringsAsFactors = FALSE
)

descriptive_statistics <- rbind(biased_descriptive, neutral_descriptive)
print(descriptive_statistics)

#Bar Plot of reaction times by trial type and attention
combined_data <- rbind(biased_data, neutral_data)

means <- aggregate(list(Con_RT = combined_data$Con_RT, InCon_RT = combined_data$InCon_RT), 
                   by = list(Attention = combined_data$Attention), FUN = mean)

means$Attention <- factor(means$Attention, levels = unique(means$Attention))

means_long <- stack(means[, -1])

barplot(height = means_long$values,
        beside = TRUE,
        main = "Mean Reaction Times by Attention and Trial Type",
        xlab = "Condition",
        ylab = "Mean Reaction Time (ms)",
        col = c("lightblue", "lightgreen"),
        legend.text = unique(means_long$ind),
        names.arg = rep(unique(means$Attention), each = nrow(means)))
        args.legend = list(x = "topright")

#Incongruency Effect
data$IncongruencyEffect<-data$InCon_RT-data$Con_RT
Incongruency_Effect_Data<- data$IncongruencyEffect

biased_data$Incongruency_Effect_Data<-biased_data$InCon_RT-biased_data$Con_RT
neutral_data$Incongruency_Effect_Data<-neutral_data$InCon_RT-neutral_data$Con_RT


#Biased Attender Incongruency Effect Descriptive Statistics
biased_IE_mean<- mean(biased_data$Incongruency_Effect_Data)
biased_IE_std<- sd(biased_data$Incongruency_Effect_Data)
biased_IE_min<- min(biased_data$Incongruency_Effect_Data)
biased_IE_max<- max(biased_data$Incongruency_Effect_Data)

#Data frame for Biased Attender Incongruency Effect Descriptive Statistics
biased_descriptive_IE <- data.frame(
  Attention = "Biased",  
  Variable = "Incongruency Effect",
  Mean = biased_IE_mean,
  StdDev = biased_IE_std,
  Min = biased_IE_min,
  Max = biased_IE_max,
  stringsAsFactors = FALSE
)

#Neutral Attender Incongruency Effect Descriptive Statistics
neutral_IE_mean<- mean(neutral_data$Incongruency_Effect_Data)
neutral_IE_std<- sd(neutral_data$Incongruency_Effect_Data)
neutral_IE_min<- min(neutral_data$Incongruency_Effect_Data)
neutral_IE_max<- max(neutral_data$Incongruency_Effect_Data)

#Data frame for Neutral Attender Incongruency Effect Descriptive Statistics
neutral_descriptive_IE <- data.frame(
  Attention = "Neutral",  
  Variable = "Incongruency Effect",
  Mean = neutral_IE_mean,
  StdDev = neutral_IE_std,
  Min = neutral_IE_min,
  Max = neutral_IE_max,
  stringsAsFactors = FALSE
)

descriptive_statistics_IE <- rbind(biased_descriptive_IE, neutral_descriptive_IE)
print(descriptive_statistics_IE)

# ATTEMPT to create a boxplot
boxplot(Mean ~ Attention, 
        data = descriptive_statistics_IE,
        main = "Mean Incongruency Effect by Attention",
        xlab = "Attention",
        ylab = "Mean Incongruency Effect",
        col = c("lightblue", "lightgreen"),
        notch = TRUE,
        names = c("Biased", "Neutral"),
        outline = TRUE)


#Biased Incongruency Effect Scatterplot
plot(biased_data$Incongruency_Effect_Data,col = "red", 
     pch = 16, 
     main = "Scatter Plot of Incongruency Effect (Biased Attenders)",
     xlab = "Incongruency Effect (ms)",
     ylab = "Condition")

#Neutral Incongruency Effect Scatterplot
plot(neutral_data$Incongruency_Effect_Data,col = "blue", 
     pch = 16, 
     main = "Scatter Plot of Incongruency Effect (Neutral Attenders)",
     xlab = "Incongruency Effect (ms)",
     ylab = "Condition")


#Both Scatterplot
plot(neutral_data$Incongruency_Effect_Data, 
     col = "blue", 
     pch = 16, 
     main = "Scatter Plot of Incongruency Effect",
     xlab = "Incongruency Effect (ms)",
     ylab = "Condition")

points(biased_data$Incongruency_Effect_Data, 
       col = "red", 
       pch = 16)
