#Data Analysis on WHO Datasets on Tubercluosis - Assignment 2
# By Dhaval Thakur, Tejas Pandit & Rushi Bhuva

library(tidyverse)
library(ggplot2)
library(pastecs)
library(ppcor)

df <- read_csv('C:/Users/RUSHI/Desktop/MSCI-718_Assignments/Assignment-2/TB_burden_countries_2020-02-18.csv')
df_1718 <- df %>% filter(year == c(2017,2018))

df_population <- read_csv('C:/Users/RUSHI/Desktop/MSCI-718_Assignments/Assignment-2/TB_expenditure_utilisation_2020-02-18.csv')
df_pop_1718 <- df_population %>% filter(year == c(2017, 2018))

#Joining datasets for analysis from variables present in different dataset
joined <- inner_join(df_1718, df_pop_1718, by = c("country", "g_whoregion", "year"))

#Testing and observing correlations among various variables.
cor(joined$rcvd_lab, joined$e_inc_num, method = 'pearson', use = "complete.obs")

cor(joined$rcvd_tbhiv, joined$e_inc_num, method = 'pearson', use = "complete.obs")

cor(joined$rcvd_tbhiv, joined$e_inc_tbhiv_num, method = 'pearson', use = "complete.obs")

cor(joined$exp_tbhiv, joined$e_inc_tbhiv_num, method = 'pearson', use = "complete.obs")

#ggplot(x = log(df_pop_2018$e_pop_num), y = log(df_pop_2018$e_inc_num)) + geom_point(mapping = aes(x = log(df_pop_2018$e_pop_num), y = log(df_pop_2018$e_inc_num)))

#Data Transformation
str(df_pop_1718)
joined_cleaned <- joined %>% filter(!is.na(exp_tot)) %>% mutate(log_e_inc_num = log(e_inc_num + 1), log_exp_tot = log(exp_tot + 1), log_e_pop_num = log(e_pop_num + 1))
stat.desc(log(joined_cleaned$e_inc_num+1), basic = FALSE, norm = TRUE)

#for incidence number
#histogram of log_e_inc_num
joined_cleaned %>% ggplot(aes(x = log_e_inc_num)) + geom_histogram(mapping = aes(y = ..density..)) + stat_function(fun=dnorm, args=list(mean=mean(joined_cleaned$log_e_inc_num), na.rm=TRUE), sd=sd(joined_cleaned$log_e_inc_num, na.rm=TRUE))
#qqplot of log_e_inc_num
joined_cleaned %>% ggplot(aes(sample = log_e_inc_num)) + stat_qq() + geom_qq_line() + theme_bw()
shapiro.test(joined_cleaned$log_e_inc_num)

#for expenditure total
#Histogram of log_exp_tot
joined_cleaned %>% ggplot(aes(x = log_exp_tot)) + geom_histogram()
#qqplot of log_exp_tot
joined_cleaned %>% ggplot(aes(sample = log_exp_tot)) + stat_qq() +geom_qq_line() + theme_bw()

shapiro.test(joined_cleaned$log_exp_tot)

#correlation test
cor.test(joined_cleaned$e_inc_num, joined_cleaned$exp_tot, method = 'kendall', use = "complete.obs")

#scatter plot to visualize positive correlation
ggplot(data =joined_cleaned, aes(x = joined_cleaned$log_e_inc_num, y = joined_cleaned$log_exp_tot)) + geom_point() + geom_smooth(method = "lm") + theme_bw()

#partial correlation
pcor.test(joined_cleaned[, 'log_e_inc_num'], joined_cleaned[, 'log_exp_tot'], joined_cleaned[, 'log_e_pop_num'], method = "kendall")

#### For Initial graph
df_region <- joined_cleaned %>% group_by(g_whoregion, year) %>% summarise(m = mean(e_inc_num))
#df_region <- pivot_wider(df_region, names_from=year, values_from = m)

df_fund <- joined_cleaned %>% group_by(g_whoregion, year) %>% summarise(m = mean(exp_tot))
#df_fund <- pivot_wider(df_fund, names_from=year, values_from = m)

df_fund %>% ggplot(aes(x = year, y = m)) + geom_line(aes(colour=g_whoregion), size = 1) + theme_bw()
df_region %>% ggplot(aes(x = year, y = m)) + geom_line(aes(colour=g_whoregion), size = 1) + theme_bw()

joined_cleaned %>% ggplot(aes(x = g_whoregion, y = log_e_inc_num)) + geom_boxplot(fill="powderblue") + theme_bw() +  xlab("WHO Region") + ylab("log of incident number")

joined_cleaned %>% ggplot(aes(x = g_whoregion, y = log_exp_tot)) + geom_boxplot(fill="powderblue") + theme_bw() + xlab("WHO Region") + ylab("log of total expenditure")
