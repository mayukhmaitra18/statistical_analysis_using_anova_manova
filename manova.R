#EXECUTE THE BELOW STATEMENTS IF YOU DONT HAVE THE BELOW PACKAGES INSTALLED
install.packages("ggpubr")
install.packages("dplyr")

#LOADING DATASET
dataset2 <- read.csv(file.choose())

#GETTING THE MENU GROUPS
levels(dataset2$menu)

#DATA DISTRIBUTION menu and time
library(dplyr)
group_by(dataset2, menu) %>%
  summarise(
    count = n(),
    mean = mean(time, na.rm = TRUE),
    sd = sd(time, na.rm = TRUE)
  )

#DATA DISTRIBUTION menu and error
library(dplyr)
group_by(dataset2, menu) %>%
  summarise(
    count = n(),
    mean = mean(error, na.rm = TRUE),
    sd = sd(error, na.rm = TRUE)
  )

# Box plots between menu and time
# ++++++++++++++++++++
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(dataset2, x = "menu", y = "time", 
          color = "menu", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#FFA07A"),
          order = c(levels(dataset2$menu)),
          ylab = "time", xlab = "menu")

# Box plots between menu and error
# ++++++++++++++++++++
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(dataset2, x = "menu", y = "error", 
          color = "menu", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#FFA07A"),
          order = c(levels(dataset2$menu)),
          ylab = "error", xlab = "menu")

# Mean plots between menu and time
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(dataset2, x = "menu", y = "time", 
       add = c("mean_se", "jitter"), 
       order = c(levels(dataset2$menu)),
       ylab = "time", xlab = "menu")

# Mean plots between menu and error
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(dataset2, x = "menu", y = "error", 
       add = c("mean_se", "jitter"), 
       order = c(levels(dataset2$menu)),
       ylab = "error", xlab = "menu")

# Compute the analysis of variance manova
res.man <- manova(cbind(time,error) ~ menu, data = dataset2)

# Summary of the analysis manova
summary(res.man)

#anova summary to compare each variable with menu
summary.aov(res.man)


#pairwise t test using Bonferroni for menu and time
pairwise.t.test(dataset2[,2],dataset2[,4],
                p.adjust.method = "bonferroni")

#pairwise t test using Bonferroni for menu and error
pairwise.t.test(dataset2[,3],dataset2[,4],
                p.adjust.method = "bonferroni")


#IF YOU WANT TO RUN INDIVIDUAL PAIRED T-TEST BETWEEN THE MENU GROUPS AND TIME FACTOR THEN EXECUTE THE BELOW SECTION OF CODE
#THE BELOW SECTION WILL PERFORM PAIRED T-TEST AMONGST EACH OF THE GROUP. THIS IS AN ADDITIONAL EXPERIMENT PERFORMED, 
#BUT HAS NOT BEEN MENTIONED IN REPORT.

#individual pairwise t test among the factors  
upd_df <- dataset2[,c("time","menu")]

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



#IF YOU WANT TO RUN INDIVIDUAL PAIRED T-TEST BETWEEN THE MENU GROUPS AND ERROR FACTOR THEN EXECUTE THE BELOW SECTION OF CODE
#THE BELOW SECTION WILL PERFORM PAIRED T-TEST AMONGST EACH OF THE GROUP. THIS IS AN ADDITIONAL EXPERIMENT PERFORMED, 
#BUT HAS NOT BEEN MENTIONED IN REPORT.

#individual pairwise t test among the factors  
upd_df1 <- dataset2[,c("error","menu")]

alpha1 <- as.list(rep(levels( upd_df1$menu )))

row_num1 = length(which(upd_df1$menu == alpha1[1]))
col_num1 = length(alpha1)

test1 = as.data.frame(alpha1)
for (i in 1:length(alpha1))
{
  colnames(test1)[i] <- alpha1[i]
}

test1 <- test1[-(1),]

indx1 <- sapply(test1, is.factor)
test1[indx1] <- lapply(test1[indx1], function(x) as.numeric(as.character(x)))

for (i in alpha1)
{
  cnt = 1
  for (j in 1:nrow(upd_df1))
  {
    if (i == upd_df1[j,"menu"] )
    {
      test1[cnt,i] = upd_df1[j,"error"]
      cnt = cnt + 1
    }
  }
}

for (i in 1:length(alpha1))
{
  if (i< length(alpha1))
  {
    for (j in (i+1):length(alpha1))
    {
      print('individual pairwise t test for the below factors')
      print(alpha1[i])
      print(alpha1[j])
      print(t.test(test1[,i],test1[,j],paired=TRUE))
    }
  }
}



