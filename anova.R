#EXECUTE THE BELOW STATEMENTS IF YOU DONT HAVE THE BELOW PACKAGES INSTALLED
install.packages("ggpubr")
install.packages("dplyr")

#LOADING DATASET
dataset1 <- read.csv(file.choose())

#GETTING THE MENU GROUPS
levels(dataset1$menu)

#DATA DISTRIBUTION
library(dplyr)
group_by(dataset1, menu) %>%
  summarise(
    count = n(),
    mean = mean(time, na.rm = TRUE),
    sd = sd(time, na.rm = TRUE)
  )

# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(dataset1, x = "menu", y = "time", 
          color = "menu", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#FFA07A"),
          order = c(levels(dataset2$menu)),
          ylab = "time", xlab = "menu")

# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(dataset1, x = "menu", y = "time", 
       add = c("mean_se", "jitter"), 
       order = c(levels(dataset2$menu)),
       ylab = "time", xlab = "menu")

# Compute the analysis of variance
res.aov <- aov(time ~ menu, data = dataset1)
# Summary of the analysis
summary(res.aov)
#plot the analysis of variance
plot(res.aov)


#pairwise t test using Bonferroni
pairwise.t.test(dataset1[,2],dataset1[,3],
                p.adjust.method = "bonferroni")






#IF YOU WANT TO RUN INDIVIDUAL PAIRED T-TEST BETWEEN THE MENU GROUPS THEN EXECUTE THE BELOW SECTION OF CODE
#THE BELOW SECTION WILL PERFORM PAIRED T-TEST AMONGST EACH OF THE GROUP. THIS IS AN ADDITIONAL EXPERIMENT PERFORMED, 
#BUT HAS NOT BEEN MENTIONED IN REPORT.

upd_df <- dataset1[,c("time","menu")]

alpha <- as.list(rep(levels( upd_df$menu )))

row_num = length(which(upd_df$menu == alpha[1]))
col_num = length(alpha)

test = as.data.frame(alpha)
for (i in 1:length(alpha))
{
  colnames(test)[i] <- alpha[i]
}

test <- test[-(1),]

indx <- sapply(test, is.factor)
test[indx] <- lapply(test[indx], function(x) as.numeric(as.character(x)))

for (i in alpha)
{
  cnt = 1
  for (j in 1:nrow(upd_df))
  {
    if (i == upd_df[j,"menu"] )
    {
      test[cnt,i] = upd_df[j,"time"]
      cnt = cnt + 1
    }
  }
}

for (i in 1:length(alpha))
{
  if (i< length(alpha))
  {
    for (j in (i+1):length(alpha))
    {
      print('individual pairwise t test for the below factors')
      print(alpha[i])
      print(alpha[j])
      print(t.test(test[,i],test[,j],paired=TRUE))
    }
  }
}