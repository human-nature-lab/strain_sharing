### Figure 1
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(dplyr)
library(svglite)

SN_All_Relationship_DF <- readr::read_tsv('../data/export/SN_All_Relationship_DF.tsv')

#Make relationship factor variable for plotting
SN_All_Relationship_DF$relationship <- reorder(as.factor(SN_All_Relationship_DF$relationship),
                                               -SN_All_Relationship_DF$strain_sharing_rate,
                                               FUN = median, na.rm=TRUE)


#Change names for plotting
levels(SN_All_Relationship_DF$relationship) <- c("Partner",
                                                 "Same Building",
                                                 "Mother",
                                                 "Child",
                                                 "Father",
                                                 "Free Time",
                                                 "Personal/\nPrivate",
                                                 "Sibling",
                                                 "Non-Kin\nDif-House",
                                                 "Close Friend",
                                                 "No-Nom\nSame-Vil",
                                                 "No-Nom\nDif-Vil")

median_rels_ssr <- aggregate(strain_sharing_rate ~  relationship, SN_All_Relationship_DF, median, na.rm=TRUE)

rels_pvals <- compare_means(strain_sharing_rate ~ relationship,
                   data = SN_All_Relationship_DF,
                   method = "wilcox.test",
                   p.adjust.method = "BH")
rels_pvals <- rels_pvals %>% filter(p.signif == "ns") %>% 
  mutate(y.position = c(40, 37.5, 35, 32.5, 27))

relationships_all_plot <- 
  ggplot(SN_All_Relationship_DF, aes(relationship, strain_sharing_rate, color = relationship)) +
  geom_boxplot(aes(color = relationship), outlier.shape = NA, size = 1) +
  xlab("Relationship") +
  ylab("Strain-Sharing Rate (%)") +
  theme_pubr() +
  labs_pubr() +
  scale_y_continuous( limits = c(0,42), breaks = seq(0, 40, 5)) +
  theme(
    axis.text.x = element_text(
      vjust = 1,
      hjust = .5,
      size = 10
    ),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 10),
    legend.position = "none"
  ) +
  stat_pvalue_manual(rels_pvals, label = "p.signif", tip.length = 0,  bracket.size = 0.7, size = 5)   +
  geom_text(data = median_rels_ssr,
            color = 'black',
            aes(label = paste0(sprintf(
              "%0.1f", round(strain_sharing_rate, digits = 2)
            ),"%"),
            y = 42,
            fontface = "bold")) +
  scale_x_discrete(labels = paste0(
    levels(SN_All_Relationship_DF$relationship),
    "\n(N=",
    table(SN_All_Relationship_DF$relationship),
    ")"
  )) +
  font("ylab", face = "bold", size = 12) +
  scale_color_manual(values = c("#80d15d",
"#954ed1",
"#cdb753",
"#51347d",
"#d6593c",
"#81c9b8",
"#ca5395",
"#566e3e",
"#8692c8",
"#823f34",
"#d0a39e",
"#403642"))

SN_Symmetrized <- readr::read_tsv('../data/export/SN_Symmetrized.tsv') |> mutate(
  Free_Time = case_when(
    a2701_numeric_max == 4 ~ "Every day",
    a2701_numeric_max == 3 ~ "A few days a week",
    a2701_numeric_max == 2 ~ "A few days a month",
    a2701_numeric_max == 1 ~ "Rarely/never",
),
Meals = case_when(
  a2702_numeric_max == 4 ~ "Almost every day",
  a2702_numeric_max == 3 ~ "Once a week",
  a2702_numeric_max == 2 ~ "Few times a month",
  a2702_numeric_max == 1 ~ "Once a month"
)) |>
mutate(
    Free_Time = as.factor(Free_Time),
    Meals = as.factor(Meals)
)
SN_Symmetrized$Meals <- reorder(as.factor(SN_Symmetrized$Meals),
                                -SN_Symmetrized$strain_sharing_rate,
                                median,na.rm=TRUE)


meals_ssr_median <- aggregate(strain_sharing_rate ~  Meals, SN_Symmetrized,
                              median, na.rm=TRUE)

meal_pvals <- compare_means(strain_sharing_rate ~ Meals,
                            data = SN_Symmetrized,
                            method = "wilcox.test",
                            p.adjust.method = "BH")

meal_pvals <- meal_pvals %>% filter(p.signif == "ns") %>%
  mutate(y.position = c(55, 52))

meals_all_plot <- ggplot(SN_Symmetrized, aes(Meals, strain_sharing_rate, color = Meals)) +
  geom_quasirandom(size = 1, alpha = 0.5) +
  # geom_boxplot( outlier.shape = NA ) +
  # geom_jitter(aes(color = Meals),
  #             alpha = .25,
  #             width = .25,
  #             show.legend = FALSE) +
  xlab("Shared Meal Frequency") +
  ylab("Strain Sharing Rate (%)") +
  scale_y_continuous( limits = c(0, 60), breaks = seq(0, 60, 10)) +
  theme_pubr() +
  labs_pubr() +
  theme(
    axis.text.x = element_text(
      angle = 0,
      vjust = 1,
      hjust = .5,
      size = 10
    ),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_text(data = meals_ssr_median,
            color = 'black',
            aes(label = paste0(sprintf(
              "%0.1f", round(strain_sharing_rate, digits = 2)
            ),"%"),
            y = 60,
            fontface = "bold")) +
  scale_x_discrete(labels = paste0(
    levels(SN_Symmetrized$Meals),
    "\n(N=",
    table(SN_Symmetrized$Meals),
    ")"
  )) +
  stat_pvalue_manual(meal_pvals, label = "p.signif", tip.length = 0, bracket.size = 0.7, size = 5)+
  font("ylab", face = "bold", size = 12) + 
  scale_color_manual(values = c("#01b0f8",
                              "#33d600",
                              "#004672",
                              "#ff6438",
                              "#537200",
                              "#e3006f",
                              "#9216ff"
                              ))


SN_Symmetrized_2 <- SN_Symmetrized %>% filter(Free_Time != "Rarely/never")

free_time_ssr_median <- aggregate(strain_sharing_rate ~  Free_Time, SN_Symmetrized_2,
                                  median, na.rm = TRUE)

#Reorder relationship factor variable for plotting
SN_Symmetrized_2$Free_Time <- reorder(as.factor(SN_Symmetrized_2$Free_Time),
                                              -SN_Symmetrized_2$strain_sharing_rate,
                                               median, na.rm = TRUE)

free_time_pvals <- compare_means(strain_sharing_rate ~ Free_Time,
                            data = SN_Symmetrized_2,
                            method = "wilcox.test",
                            p.adjust.method = "BH")

free_time_pvals <- free_time_pvals %>% filter(p.signif == "ns") %>%
  mutate(y.position = c(45))

free_time_all_plot <- ggplot(SN_Symmetrized_2, aes(Free_Time, strain_sharing_rate, color = Free_Time)) +
  geom_quasirandom(size = 1, alpha = 0.5) +
  xlab("Frequency of Contact") +
  ylab("Strain-Sharing Rate (%)") +
  scale_y_continuous( limits = c(0, 60), breaks = seq(0, 60, 10)) +
  theme_pubr() +
  labs_pubr() +
  theme(
    axis.text.x = element_text(
      angle = 0,
      vjust = 1,
      hjust = .5,
      size = 10
    ),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_text(data = free_time_ssr_median,
            color = 'black',
            aes(label = paste0(sprintf(
              "%0.1f", round(strain_sharing_rate, digits = 2)
            ),"%"),
            y = 60,
            fontface = "bold")) +
  scale_x_discrete(labels = paste0(
    levels(SN_Symmetrized_2$Free_Time),
    "\n(N=",
    table(SN_Symmetrized_2$Free_Time),
    ")"
  )) +
  stat_pvalue_manual(free_time_pvals, label = "p.signif", tip.length = 0, bracket.size = 0.7, size = 5) +
  font("ylab", face = "bold", size = 12) +
    scale_color_manual(values = c("#01b0f8",
                                "#33d600",
                                "#004672",
                                "#ff6438",
                                "#537200",
                                "#e3006f",
                                "#9216ff"
                                ))

SN_Greeting <- readr::read_tsv('../data/export/SN_Greeting.tsv')

SN_Greeting <- SN_Greeting %>% mutate(
  Riskiest_Greeting = case_when(
    max_greeting == 7 ~ "Kiss on the cheek",
    max_greeting == 6 ~ "Hug",
    max_greeting == 5 ~ "Pat on the back",
    max_greeting == 4 ~ "Handshake or hi-five",
    max_greeting == 3 ~ "Verbal salute (Hello)",
    max_greeting == 2 ~ "A gesture (wave, nod, etc.)",
    max_greeting == 1 ~ "A smile",
    max_greeting == 0 ~ "Other/Refused",
  ))

SN_Greeting <- SN_Greeting %>% filter(Riskiest_Greeting != "Other/Refused")

SN_Greeting$Riskiest_Greeting <- factor(SN_Greeting$Riskiest_Greeting,
                                        levels=c("Kiss on the cheek",
                                                 "Hug",
                                                 "Pat on the back",
                                                 "Handshake or hi-five",
                                                 "Verbal salute (Hello)",
                                                 "A smile",
                                                 "A gesture (wave, nod, etc.)"))

greeting_median <- aggregate(strain_sharing_rate ~  Riskiest_Greeting,
                             SN_Greeting, median, na.rm = TRUE)

greetings_pvals <- compare_means(strain_sharing_rate ~ Riskiest_Greeting,
                                 data = SN_Greeting,
                                 p.adjust.method = "BH"
                                 )

greeting_plot <- ggplot(SN_Greeting, aes(Riskiest_Greeting, strain_sharing_rate, color = Riskiest_Greeting)) +
  geom_quasirandom(size = 1, alpha = 0.5) +
  xlab("Greeting Type") +
  scale_x_discrete(labels = paste0(
    levels(as.factor(SN_Greeting$Riskiest_Greeting)),
    "\n(N=",
    table(SN_Greeting$Riskiest_Greeting),
    ")"
  )) +
  ylab("Strain-Sharing Rate (%)") +
  scale_y_continuous( limits = c(0, 60), breaks = seq(0, 60, 10)) +
  theme_pubr() +
  labs_pubr() +
  theme(
    axis.text.x = element_text(
      vjust = 1,
      hjust = .5,
      size = 10
    ),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_text(data = greeting_median,
            color = 'black',
            aes(
              label = paste0(sprintf(
                "%0.1f", round(strain_sharing_rate, digits = 2)
              ),"%"),
              y = 60,
              fontface = "bold"
            )) +
  font("ylab", face = "bold", size = 12) + 
  scale_color_manual(values = c("#01b0f8",
                                "#33d600",
                                "#004672",
                                "#ff6438",
                                "#537200",
                                "#e3006f",
                                "#9216ff"
                                )
)

Downsamp_all_importance <- readr::read_tsv('../data/export/Downsamp_all_importance.tsv')

mse_perms_ssr_all <- data.frame(matrix(nrow = 100, ncol = 17))
colnames(mse_perms_ssr_all) <- c("gender", "indigenous_status", "religion", "age_difference","average_age","wealth_difference", "average_wealth",
                             "education_difference","average_education","household_same","related","average_diet", "different_diet", 'average_bristol','different_bristol',
                             'watersource_same','same_med_usage')

#Gender
print("gender")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  gender_cols <- dat_permuted[,c("gender.mm", "gender.mf")]
  gender_cols <- gender_cols[sample(nrow(gender_cols)),]
  dat_permuted[,c("gender.mm", "gender.mf")] <- gender_cols
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$gender[i] <- mean(lm_perm$residuals^2)

}

#indigenous status
print("indigenous status")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  indigenous_cols <- dat_permuted[,c("indigenous.both", "indigenous.one")]
  indigenous_cols <- indigenous_cols[sample(nrow(indigenous_cols)),]
  dat_permuted[,c("indigenous.both", "indigenous.one")] <- indigenous_cols
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$indigenous_status[i] <- mean(lm_perm$residuals^2)

}
#religion
print("religion")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$religion.same <- sample(dat_permuted$religion.same)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$religion[i] <- mean(lm_perm$residuals^2)

}
#age difference
print("age difference")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$age_difference_abs <- sample(dat_permuted$age_difference_abs)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$age_difference[i] <- mean(lm_perm$residuals^2)

}
#average age
print("average age")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$average_age <- sample(dat_permuted$average_age)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$average_age[i] <- mean(lm_perm$residuals^2)

}
#wealth difference
print("wealth difference")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$wealth_difference_abs <- sample(dat_permuted$wealth_difference_abs)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$wealth_difference[i] <- mean(lm_perm$residuals^2)

}
#average wealth
print("average wealth")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$average_wealth <- sample(dat_permuted$average_wealth)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$average_wealth[i] <- mean(lm_perm$residuals^2)

}
#education
print("education")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$education_difference_abs <- sample(dat_permuted$education_difference_abs)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$education_difference[i] <- mean(lm_perm$residuals^2)

}
#average education
print("average education")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$average_education <- sample(dat_permuted$average_education)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$average_education[i] <- mean(lm_perm$residuals^2)

}
#related
print("related")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$related <- sample(dat_permuted$related)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$related[i] <- mean(lm_perm$residuals^2)

}

print("same_household")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$household_same <- sample(dat_permuted$household_same)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$household_same[i] <- mean(lm_perm$residuals^2)

}
print("different diet")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$different_diet <- sample(dat_permuted$different_diet)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$different_diet[i] <- mean(lm_perm$residuals^2)

}
print("average diet")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$average_diet <- sample(dat_permuted$average_diet)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$average_diet[i] <- mean(lm_perm$residuals^2)

}
print("different bristol")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$different_bristol <- sample(dat_permuted$different_bristol)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$different_bristol[i] <- mean(lm_perm$residuals^2)

}
print("average bristol")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$average_bristol <- sample(dat_permuted$average_bristol)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$average_bristol[i] <- mean(lm_perm$residuals^2)

}
print("watersource_same")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  dat_permuted$watersource_same <- sample(dat_permuted$watersource_same)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$watersource_same[i] <- mean(lm_perm$residuals^2)

}

print("same_med_usage")
dat_permuted <- Downsamp_all_importance
for(i in 1:100){
  med_cols <- dat_permuted[,c('same_usage_painkillers', 'same_usage_antibiotics', 'same_usage_antidiarrheal', 'same_usage_antiparasite', 
                              'same_usage_vitamins', 'same_usage_zinc', 'same_usage_antifungal', 'same_usage_antihypertensives', 
                              'same_usage_antidiabetics', 'same_usage_antiacids', 'same_usage_laxatives')]
  med_cols <- med_cols[sample(nrow(med_cols)),]
  dat_permuted[,c('same_usage_painkillers', 'same_usage_antibiotics', 'same_usage_antidiarrheal', 'same_usage_antiparasite', 
                              'same_usage_vitamins', 'same_usage_zinc', 'same_usage_antifungal', 'same_usage_antihypertensives', 
                              'same_usage_antidiabetics', 'same_usage_antiacids', 'same_usage_laxatives')] <- med_cols
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_all$same_med_usage[i] <- mean(lm_perm$residuals^2)

}

#Get original linear regression model
lm_ssr_all <- lm(strain_sharing_rate ~ . ,
                 data = Downsamp_all_importance)
#Get original mse
mse_ssr_all <- mean(lm_ssr_all$residuals^2)

#Get percent change in auc from original under permutation
#mse_perms_percent_ssr <- (as.numeric(mse_ssr) - mse_perms_percent_ssr) / as.numeric(mse_ssr) * 100
mse_perms_percent_ssr_all <- mse_perms_ssr_all - as.numeric(mse_ssr_all)

#Get mean drop and standard deviation in dataframe
mse_perms_ssr_dat_all_rel <- data.frame(name = names(mse_perms_percent_ssr_all),
                                value = colMeans(mse_perms_percent_ssr_all),
                                sd = apply(mse_perms_percent_ssr_all,2,sd))

#Change Name to Make More Readable
colnames(mse_perms_percent_ssr_all) <-c("Gender",
                           "Indigenous\nStatus",
                           "Religion",
                           "Age\nDifference",
                           "Average\nAge",
                           "Wealth\nDifference",
                           "Average\nWealth",
                           "Education\nDifference",
                           "Average\nEducation",
                           "Household\nSharing",
                           "Has\nrelationship",
                           "Average\ndiet",
                           'Diet\ndifference',
                           'Average\nBristol\nScale',
                           'Different\nBristol\nScale',
                           'Water\nsource',
                           'Medication\nusage'
                           )

#Create plot
fi_all <- ggplot(mse_perms_ssr_dat_all_rel) +
  geom_bar(
    aes(x = reorder(name,-value), y = value),
    stat = "identity",
    fill = "skyblue",
    alpha = 0.7
  ) +
  geom_errorbar(
    aes(
      x = name,
      ymin = value - sd,
      ymax = value + sd
    ),
    width = 0.4,
    colour = "orange",
    alpha = 0.9,
    linewidth = 1.3
  ) +
  coord_cartesian(ylim = c(0, 3.5)) +
  xlab("Covariate") +
  ylab(bquote(MSE[perm] - MSE[orig])) +
  theme_bw() +
  ggtitle("All Social and Familial Relationships") +
  theme_pubr() +
  labs_pubr() +
  theme(axis.text.x = element_text( vjust = 1, hjust=.5, size = 9),
        plot.title = element_text(hjust=.5)
  )


Downsamp_all_importance_nk <- readr::read_tsv('../data/export/Downsamp_all_importance_nk.tsv')

mse_perms_ssr_nk <- data.frame(matrix(nrow = 1000, ncol = 17))
colnames(mse_perms_ssr_nk) <- c("gender", "indigenous_status", "religion", "age_difference","average_age","wealth_difference", "average_wealth",
                             "education_difference","average_education","related","average_diet", "different_diet", 'average_bristol','different_bristol',
                             'watersource_same','same_med_usage','household_same')

#Gender
print("gender")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  gender_cols <- dat_permuted[,c("gender.mm", "gender.mf")]
  gender_cols <- gender_cols[sample(nrow(gender_cols)),]
  dat_permuted[,c("gender.mm", "gender.mf")] <- gender_cols
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$gender[i] <- mean(lm_perm$residuals^2)

}

#indigenous status
print("indigenous status")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  indigenous_cols <- dat_permuted[,c("indigenous.both", "indigenous.one")]
  indigenous_cols <- indigenous_cols[sample(nrow(indigenous_cols)),]
  dat_permuted[,c("indigenous.both", "indigenous.one")] <- indigenous_cols
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$indigenous_status[i] <- mean(lm_perm$residuals^2)

}
#religion
print("religion")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$religion.same <- sample(dat_permuted$religion.same)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$religion[i] <- mean(lm_perm$residuals^2)

}
#age difference
print("age difference")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$age_difference_abs <- sample(dat_permuted$age_difference_abs)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$age_difference[i] <- mean(lm_perm$residuals^2)

}
#average age
print("average age")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$average_age <- sample(dat_permuted$average_age)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$average_age[i] <- mean(lm_perm$residuals^2)

}
#wealth difference
print("wealth difference")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$wealth_difference_abs <- sample(dat_permuted$wealth_difference_abs)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$wealth_difference[i] <- mean(lm_perm$residuals^2)

}
#average wealth
print("average wealth")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$average_wealth <- sample(dat_permuted$average_wealth)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$average_wealth[i] <- mean(lm_perm$residuals^2)

}
#education
print("education")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$education_difference_abs <- sample(dat_permuted$education_difference_abs)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$education_difference[i] <- mean(lm_perm$residuals^2)

}
#average education
print("average education")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$average_education <- sample(dat_permuted$average_education)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$average_education[i] <- mean(lm_perm$residuals^2)

}
#related
print("related")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$related <- sample(dat_permuted$related)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$related[i] <- mean(lm_perm$residuals^2)

}

print("same_household")
for(i in 1:1000){
  #This has to be empty, set to a big number to keep the variable sorted
  mse_perms_ssr_nk$household_same[i] <- 5550

}
print("different diet")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$different_diet <- sample(dat_permuted$different_diet)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$different_diet[i] <- mean(lm_perm$residuals^2)

}
print("average diet")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$average_diet <- sample(dat_permuted$average_diet)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$average_diet[i] <- mean(lm_perm$residuals^2)

}
print("different bristol")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$different_bristol <- sample(dat_permuted$different_bristol)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$different_bristol[i] <- mean(lm_perm$residuals^2)

}
print("average bristol")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$average_bristol <- sample(dat_permuted$average_bristol)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$average_bristol[i] <- mean(lm_perm$residuals^2)

}
print("watersource_same")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  dat_permuted$watersource_same <- sample(dat_permuted$watersource_same)
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$watersource_same[i] <- mean(lm_perm$residuals^2)

}

print("same_med_usage")
dat_permuted <- Downsamp_all_importance_nk
for(i in 1:1000){
  med_cols <- dat_permuted[,c('same_usage_painkillers', 'same_usage_antibiotics', 'same_usage_antidiarrheal', 'same_usage_antiparasite', 
                              'same_usage_vitamins', 'same_usage_zinc', 'same_usage_antifungal', 'same_usage_antihypertensives', 
                              'same_usage_antidiabetics', 'same_usage_antiacids', 'same_usage_laxatives')]
  med_cols <- med_cols[sample(nrow(med_cols)),]
  dat_permuted[,c('same_usage_painkillers', 'same_usage_antibiotics', 'same_usage_antidiarrheal', 'same_usage_antiparasite', 
                              'same_usage_vitamins', 'same_usage_zinc', 'same_usage_antifungal', 'same_usage_antihypertensives', 
                              'same_usage_antidiabetics', 'same_usage_antiacids', 'same_usage_laxatives')] <- med_cols
  lm_perm <- lm(strain_sharing_rate ~ ., data = dat_permuted)
  mse_perms_ssr_nk$same_med_usage[i] <- mean(lm_perm$residuals^2)

}

#Get original linear regression model
lm_ssr <- lm(strain_sharing_rate ~ . ,
                 data = Downsamp_all_importance_nk)

#Get original mse
mse_ssr_nk <-  mean(lm_ssr$residuals^2)

#Get percent change in auc from original under permutation
mse_perms_percent_ssr_nk <- mse_perms_ssr_nk
mse_perms_percent_ssr_nk <- mse_perms_ssr_nk - mse_ssr_nk

#Get mean drop and standard devation in dataframe
mse_perms_ssr_dat_nk <- data.frame(name = names(mse_perms_percent_ssr_nk),
                                value = colMeans(mse_perms_percent_ssr_nk),
                                sd = apply(mse_perms_percent_ssr_nk,2,sd))

#Change Name to Make More Readable
colnames(mse_perms_percent_ssr_nk) <-c("Gender",
                           "Indigenous\nStatus",
                           "Religion",
                           "Age\nDifference",
                           "Average\nAge",
                           "Wealth\nDifference",
                           "Average\nWealth",
                           "Education\nDifference",
                           "Average\nEducation",
                           "Has\nrelationship",
                           "Average\ndiet",
                           'Diet\ndifference',
                           'Average\nBristol\nScale',
                           'Different\nBristol\nScale',
                           'Water\nsource',
                           'Medication\nusage',
                           "Household\nSharing"
                           )

mse_perms_ssr_dat_nk['household_same', c('value','sd')] <- 0

fi_fig <- bind_rows(
mse_perms_percent_ssr_nk |>
  tidyr::pivot_longer(everything()) |>
  mutate(ty = 'Non-Kin Different House Relationships'),
mse_perms_percent_ssr_all |>
  tidyr::pivot_longer(everything()) |>
  mutate(ty = 'All Social and Familial Relationships')
) |>
ggplot(
  aes(
    forcats::fct_reorder2(name, ty, -value),
    value,
    group = ty,
    color = ty
  )
) +
ggbeeswarm::geom_quasirandom(dodge.width = 1, size = 0.3, bandwidth = 0.2) +
scale_color_manual(values = c('skyblue', '#eba487')) +
ylim(c(0, 3.5)) +
  xlab("Covariate") +
  ylab(bquote(MSE[perm] - MSE[orig])) +
  theme_bw() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.text.x = element_text( vjust = 1, hjust=.5, size = 9),
        plot.title = element_text(hjust=.5),
        legend.position = c(0.5,0.05)
  ) +
coord_flip()




# fi_fig <- ggarrange(fi_all_nkh+ rremove("ylab"),
#                     fi_all + rremove("ylab"),
#                     nrow = 2,
#                     labels = c("A", "B"))
svglite("figure1E.svg",
        fix_text_size = FALSE,
        width = 13,
        height = 10)
fi_fig
dev.off()
p1 <- ggarrange(relationships_all_plot,
                labels = c("A"),
                ncol = 1, nrow = 1,
                label.y = 1)

p2 <- ggarrange(free_time_all_plot  ,
                    meals_all_plot  ,
                    labels = c("B", "C"),
                    align = "h",
                    ncol = 2,
                    label.y = 1)

greeting_plot <- ggarrange(greeting_plot,
                           labels = c("D"),
                           label.y = 1)
fi_fig <- ggarrange(fi_fig, labels = c('E'))
fig1 <- ggarrange(p1, p2,greeting_plot, nrow = 3,
          heights = c(1.5,1,1))
fig1 <- ggarrange(fig1, fi_fig, ncol = 2, widths = c(1, 0.3))
svglite("Figure1.svg",
        fix_text_size = FALSE,
        width = 17,
        height = 10)
fig1
dev.off()
