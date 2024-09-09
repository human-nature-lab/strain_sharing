### Figure 2
library(ggplot2)
library(doParallel)
library(ggpubr)
library(dplyr)
library(pROC)
library(igraph)

Downsamp_all <- readr::read_tsv('data/export/Downsamp_all.tsv')

cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)
foreach(i = c(1:5)) %dopar% {
  inds <- c(1:nrow(Downsamp_all))
  test1_inds <- sample(inds, size = floor(.33*nrow(Downsamp_all)), replace = F)
  train1_inds <- inds[!(inds %in% test1_inds)]
  test2_inds <- sample(train1_inds, size = floor(.5*length(train1_inds)), replace = F)
  train2_inds <- inds[!(inds %in% test2_inds)]
  test3_inds <- train2_inds[!(train2_inds %in% test1_inds)]
  train3_inds <- inds[!(inds %in% test3_inds)]

  train1 <- Downsamp_all[train1_inds,]
  test1 <- Downsamp_all[test1_inds,]
  train2 <- Downsamp_all[train2_inds,]
  test2 <- Downsamp_all[test2_inds,]
  train3 <- Downsamp_all[train3_inds,]
  test3 <- Downsamp_all[test3_inds,]
  
  logit.test.1.onlycovars <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                                              indigenous.one + religion.same + age_difference_abs +
                                              average_age + wealth_difference_abs + average_wealth +
                                              education_difference_abs + average_education + household_same + (1|village_code_w3),
                                              data = train1, family = binomial, control=lme4::glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  logit.test.2.onlycovars <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                                              indigenous.one + religion.same + age_difference_abs +
                                              average_age + wealth_difference_abs + average_wealth +
                                              education_difference_abs + average_education + household_same + (1|village_code_w3),
                                              data = train2, family = binomial, control=lme4::glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  logit.test.3.onlycovars <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                                              indigenous.one + religion.same + age_difference_abs +
                                              average_age + wealth_difference_abs + average_wealth +
                                              education_difference_abs + average_education + household_same + (1|village_code_w3),
                                              data = train3, family = binomial, control=lme4::glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  preds.1.onlycovars <- predict(logit.test.1.onlycovars, newdata = test1, type = "response")
  preds.2.onlycovars <- predict(logit.test.2.onlycovars, newdata = test2, type = "response")
  preds.3.onlycovars <- predict(logit.test.3.onlycovars, newdata = test3, type = "response")
  names(preds.3.onlycovars) <- test3_inds
  names(preds.2.onlycovars) <- test2_inds
  names(preds.1.onlycovars) <- test1_inds
  preds.round.onlycovars <- c(preds.1.onlycovars, preds.2.onlycovars, preds.3.onlycovars)
  preds.round.onlycovars <- preds.round.onlycovars[order(as.numeric(names(preds.round.onlycovars)))]
  
  
  logit.test.1 <- lme4::glmer(related ~ strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2 <- lme4::glmer(related ~ strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3 <- lme4::glmer(related ~ strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  preds.1 <- predict(logit.test.1, newdata = test1, type = "response")
  preds.2 <- predict(logit.test.2, newdata = test2, type = "response")
  preds.3 <- predict(logit.test.3, newdata = test3, type = "response")
  names(preds.3) <- test3_inds
  names(preds.2) <- test2_inds
  names(preds.1) <- test1_inds
  preds.round <- c(preds.1, preds.2, preds.3)
  preds.round <- preds.round[order(as.numeric(names(preds.round)))]
  
  logit.test.1.covars <- lme4::glmer(related ~ strain_sharing_rate + gender.mm + gender.mf +
                        age_difference_abs + average_age + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2.covars <- lme4::glmer(related ~ strain_sharing_rate + gender.mm + gender.mf +
                        age_difference_abs + average_age + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3.covars <- lme4::glmer(related ~ strain_sharing_rate + gender.mm + gender.mf +
                        age_difference_abs + average_age + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  
  preds.1.covars <- predict(logit.test.1.covars, newdata = test1, type = "response")
  preds.2.covars <- predict(logit.test.2.covars, newdata = test2, type = "response")
  preds.3.covars <- predict(logit.test.3.covars, newdata = test3, type = "response")
  names(preds.3.covars) <- test3_inds
  names(preds.2.covars) <- test2_inds
  names(preds.1.covars) <- test1_inds
  preds.round.covars <- c(preds.1.covars, preds.2.covars, preds.3.covars)
  preds.round.covars <- preds.round.covars[order(as.numeric(names(preds.round.covars)))]
  
  logit.test.1.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education + household_same +
                        strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education + household_same +
                        strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education + household_same +
                        strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  
  preds.1.all <- predict(logit.test.1.all, newdata = test1, type = "response")
  preds.2.all <- predict(logit.test.2.all, newdata = test2, type = "response")
  preds.3.all <- predict(logit.test.3.all, newdata = test3, type = "response")
  names(preds.3.all) <- test3_inds
  names(preds.2.all) <- test2_inds
  names(preds.1.all) <- test1_inds
  preds.round.all <- c(preds.1.all, preds.2.all, preds.3.all)
  preds.round.all <- preds.round.all[order(as.numeric(names(preds.round.all)))]
  
  list(preds.round, preds.round.onlycovars, preds.round.covars, preds.round.all)
} -> preds
parallel::stopCluster(cl)

pred.all <- rowSums(sapply(preds, `[[`, 1))/5
pred.all.onlycovars <- rowSums(sapply(preds, `[[`, 2))/5
pred.all.covars <- rowSums(sapply(preds, `[[`, 3))/5
pred.all.all <- rowSums(sapply(preds, `[[`, 4))/5

#add on predictions from strain-sharing only model
pred.all.acc <- pred.all.all
pred.all.acc[pred.all.acc >=.5] <- 1
pred.all.acc[pred.all.acc <.5] <- 0

Downsamp_all$predicted <- pred.all.acc

logit.base.roc <- pROC::roc(Downsamp_all$related, pred.all, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")

logit.onlycovars.roc <- pROC::roc(Downsamp_all$related, pred.all.onlycovars, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")

logit.covars.roc <- pROC::roc(Downsamp_all$related, pred.all.covars, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")

logit.all.roc <- pROC::roc(Downsamp_all$related, pred.all.all, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")


roc.list <- list("Strain-Sharing Rate" = logit.base.roc,
                 "Only Socio-Dem Covars" = logit.onlycovars.roc,
                 "SSR + Age + Gender + Household Sharing" = logit.covars.roc,
                 "SSR + All Socio-Dem Covars" = logit.all.roc)

ci.list <- lapply(roc.list, ci.se, specificities = seq(0, 1, l = 25))

dat.ci.list <- lapply(ci.list, function(ciobj) 
  data.frame(x = as.numeric(rownames(ciobj)),
             lower = ciobj[, 1],
             upper = ciobj[, 3]))

roc.plot <- ggroc(roc.list) +
  theme_minimal() +
  geom_abline(slope=1, intercept = 1, linetype = "dashed", alpha=1, color = "grey") +
  coord_equal()

for(i in 1:4) {
  roc.plot <- roc.plot + geom_ribbon(
    data = dat.ci.list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
  } 

roc.plot <- roc.plot + 
theme(plot.title = element_text(size=10, hjust = .5),
      legend.position = c(0.6, 0.2),
      legend.text = element_text(size=10),
      text = element_text(size = 10)) + 
      scale_color_manual(name = "AUCs:",
                         values=c("Red", 'Orange', "Green", "Blue"), 
                       labels=c(paste("Strain-Sharing Rate: ", round(logit.base.roc$auc,2),
                                      " ± ",round(sqrt(pROC::var(logit.base.roc)),3), sep=""), 
                                paste("Only Socio-Dem Covars:", round(logit.onlycovars.roc$auc,2 ),
                                      " ± ",round(sqrt(pROC::var(logit.onlycovars.roc)),3), sep=""), 
                                paste("SSR + Age + Gender: ", round(logit.covars.roc$auc, 2),
                                      " ± ",round(sqrt(pROC::var(logit.covars.roc)),3), sep=""),
                                paste("SSR + All Socio-Dem Covars: ", round(logit.all.roc$auc, 2),
                                      " ± ",round(sqrt(pROC::var(logit.all.roc)),3), sep="")
                                )
                       ) +
  ggtitle("All Social or Familial Relationships")

roc.plot <- roc.plot +
  theme_pubr(legend =  c(0.6, 0.2)) +
  theme(plot.title = element_text(size = 10, hjust = .5)) +
  labs_pubr()

roc.plot <- roc.plot + theme(
  plot.title = element_text(size = 10, hjust = .5),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8)
)


Downsamp_all_nkh <- readr::read_tsv('data/export/Downsamp_all_nkh.tsv')

cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)
foreach(i = c(1:5)) %dopar% {
  inds <- c(1:nrow(Downsamp_all_nkh))
  test1_inds <- sample(inds, size = floor(.33*nrow(Downsamp_all_nkh)), replace = F)
  train1_inds <- inds[!(inds %in% test1_inds)]
  test2_inds <- sample(train1_inds, size = floor(.5*length(train1_inds)), replace = F)
  train2_inds <- inds[!(inds %in% test2_inds)]
  test3_inds <- train2_inds[!(train2_inds %in% test1_inds)]
  train3_inds <- inds[!(inds %in% test3_inds)]
  
  train1 <- Downsamp_all_nkh[train1_inds,]
  test1 <- Downsamp_all_nkh[test1_inds,]
  train2 <- Downsamp_all_nkh[train2_inds,]
  test2 <- Downsamp_all_nkh[test2_inds,]
  train3 <- Downsamp_all_nkh[train3_inds,]
  test3 <- Downsamp_all_nkh[test3_inds,]
  
  logit.test.1 <- lme4::glmer(related ~ strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2 <- lme4::glmer(related ~ strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3 <- lme4::glmer(related ~ strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  preds.1 <- predict(logit.test.1, newdata = test1, type = "response")
  preds.2 <- predict(logit.test.2, newdata = test2, type = "response")
  preds.3 <- predict(logit.test.3, newdata = test3, type = "response")
  names(preds.3) <- test3_inds
  names(preds.2) <- test2_inds
  names(preds.1) <- test1_inds
  preds.round <- c(preds.1, preds.2, preds.3)
  preds.round <- preds.round[order(as.numeric(names(preds.round)))]
  
  logit.test.1.onlycovars <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                                              indigenous.one + religion.same + age_difference_abs +
                                              average_age + wealth_difference_abs + average_wealth +
                                              education_difference_abs + average_education + household_same + (1|village_code_w3),
                                              data = train1, family = binomial, control=lme4::glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  logit.test.2.onlycovars <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                                              indigenous.one + religion.same + age_difference_abs +
                                              average_age + wealth_difference_abs + average_wealth +
                                              education_difference_abs + average_education + household_same + (1|village_code_w3),
                                              data = train2, family = binomial, control=lme4::glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  logit.test.3.onlycovars <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                                              indigenous.one + religion.same + age_difference_abs +
                                              average_age + wealth_difference_abs + average_wealth +
                                              education_difference_abs + average_education + household_same + (1|village_code_w3),
                                              data = train3, family = binomial, control=lme4::glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  preds.1.onlycovars <- predict(logit.test.1.onlycovars, newdata = test1, type = "response")
  preds.2.onlycovars <- predict(logit.test.2.onlycovars, newdata = test2, type = "response")
  preds.3.onlycovars <- predict(logit.test.3.onlycovars, newdata = test3, type = "response")
  names(preds.3.onlycovars) <- test3_inds
  names(preds.2.onlycovars) <- test2_inds
  names(preds.1.onlycovars) <- test1_inds
  preds.round.onlycovars <- c(preds.1.onlycovars, preds.2.onlycovars, preds.3.onlycovars)
  preds.round.onlycovars <- preds.round.onlycovars[order(as.numeric(names(preds.round.onlycovars)))]
  
  logit.test.1.covars <- lme4::glmer(related ~ strain_sharing_rate + gender.mm + gender.mf +
                        age_difference_abs + average_age + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2.covars <- lme4::glmer(related ~ strain_sharing_rate + gender.mm + gender.mf +
                        age_difference_abs + average_age + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3.covars <- lme4::glmer(related ~ strain_sharing_rate + gender.mm + gender.mf +
                        age_difference_abs + average_age + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  
  preds.1.covars <- predict(logit.test.1.covars, newdata = test1, type = "response")
  preds.2.covars <- predict(logit.test.2.covars, newdata = test2, type = "response")
  preds.3.covars <- predict(logit.test.3.covars, newdata = test3, type = "response")
  names(preds.3.covars) <- test3_inds
  names(preds.2.covars) <- test2_inds
  names(preds.1.covars) <- test1_inds
  preds.round.covars <- c(preds.1.covars, preds.2.covars, preds.3.covars)
  preds.round.covars <- preds.round.covars[order(as.numeric(names(preds.round.covars)))]
  
  logit.test.1.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education + household_same + 
                        strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education + household_same + 
                        strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education + household_same + 
                        strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  
  preds.1.all <- predict(logit.test.1.all, newdata = test1, type = "response")
  preds.2.all <- predict(logit.test.2.all, newdata = test2, type = "response")
  preds.3.all <- predict(logit.test.3.all, newdata = test3, type = "response")
  names(preds.3.all) <- test3_inds
  names(preds.2.all) <- test2_inds
  names(preds.1.all) <- test1_inds
  preds.round.all <- c(preds.1.all, preds.2.all, preds.3.all)
  preds.round.all <- preds.round.all[order(as.numeric(names(preds.round.all)))]
  
  list(preds.round, preds.round.onlycovars, preds.round.covars, preds.round.all)
} -> preds
parallel::stopCluster(cl)

pred.all <- rowSums(sapply(preds, `[[`, 1))/5
pred.all.onlycovars <- rowSums(sapply(preds, `[[`, 2))/5
pred.all.covars <- rowSums(sapply(preds, `[[`, 3))/5
pred.all.all <- rowSums(sapply(preds, `[[`, 4))/5

pred.all.acc <- pred.all.all
pred.all.acc[pred.all.acc >=.5] <- 1
pred.all.acc[pred.all.acc <.5] <- 0

Downsamp_all_nkh$predicted <- pred.all.acc

logit.base.roc <- pROC::roc(Downsamp_all_nkh$related, pred.all, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")

logit.onlycovars.roc <- pROC::roc(Downsamp_all_nkh$related, pred.all.onlycovars, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")

logit.covars.roc <- pROC::roc(Downsamp_all_nkh$related, pred.all.covars, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")

logit.all.roc <- pROC::roc(Downsamp_all_nkh$related, pred.all.all, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")

pROC::auc(logit.base.roc)[1]
pROC::auc(logit.covars.roc)[1]
pROC::auc(logit.all.roc)[1]

roc.list <- list("Strain-Sharing Rate" = logit.base.roc,
                 "Only Socio-Dem Covars" = logit.onlycovars.roc,
                 "SSR + Age + Gender" = logit.covars.roc,
                 "SSR + All Socio-Dem Covars" = logit.all.roc)

ci.list <- lapply(roc.list, ci.se, specificities = seq(0, 1, l = 25))

dat.ci.list <- lapply(ci.list, function(ciobj) 
  data.frame(x = as.numeric(rownames(ciobj)),
             lower = ciobj[, 1],
             upper = ciobj[, 3]))

roc.plot.nkh <- ggroc(roc.list) +
  theme_minimal() +
  geom_abline(slope=1, intercept = 1, linetype = "dashed", alpha=1, color = "grey") +
  coord_equal()

for(i in 1:3) {
  roc.plot.nkh <- roc.plot.nkh + geom_ribbon(
    data = dat.ci.list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
  } 

roc.plot.nkh <- roc.plot.nkh + 
theme(plot.title = element_text(size=10, hjust = .5),
      legend.position = c(0.6, 0.2),
      legend.text = element_text(size=10),
      text = element_text(size = 10)) + 
      scale_color_manual(name = "AUCs:",
                         values=c("Red", 'Orange', "Green", "Blue"), 
                       labels=c(paste("Strain-Sharing Rate: ", round(logit.base.roc$auc,2),
                                      " ± ",round(sqrt(pROC::var(logit.base.roc)),3), sep=""), 
                                paste("Only Socio-Dem Covars:", round(logit.onlycovars.roc$auc,2 ),
                                      " ± ",round(sqrt(pROC::var(logit.onlycovars.roc)),3), sep=""), 
                                paste("SSR + Age + Gender: ", round(logit.covars.roc$auc, 2),
                                      " ± ",round(sqrt(pROC::var(logit.covars.roc)),3), sep=""),
                                paste("SSR + All Socio-Dem Covars: ", round(logit.all.roc$auc, 2),
                                      " ± ",round(sqrt(pROC::var(logit.all.roc)),3), sep="")
                                )
                       ) +
  ggtitle("Non-Kin Different House Relationships")

roc.plot.nkh <- roc.plot.nkh +
  theme_pubr(legend =  c(0.6, 0.2)) +
  theme(plot.title = element_text(size = 10, hjust = .5)) +
  labs_pubr()

roc.plot.nkh <- roc.plot.nkh + theme(
  plot.title = element_text(size = 10, hjust = .5),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8)
)


pred_graph <- readr::read_tsv('data/export/pred_graph_all_rel.tsv') |>
    mutate(tp = predicted == related) |>
    igraph::graph_from_data_frame(directed = FALSE) 

pred_colors <- edge_attr(pred_graph, "predicted")
E(pred_graph)$size <- pred_colors*1.5+3.5

pred_colors[pred_colors == 1] <- "blue"
pred_colors[pred_colors == 0] <- "grey"

E(pred_graph)$color <- pred_colors
par(mar=c(0,0,0,0)+.1)
set.seed(1993)
layout <- layout.fruchterman.reingold(pred_graph)

pred_graph_nkh <- readr::read_tsv('data/export/pred_graph_nk.tsv') |>
    mutate(tp = predicted == related) |>
  graph_from_data_frame(directed = FALSE)


pred_colors <- edge_attr(pred_graph_nkh, "predicted")
E(pred_graph_nkh)$size <- pred_colors*1.5+3.5
pred_colors[pred_colors == 1] <- "blue"
pred_colors[pred_colors == 0] <- "grey"

E(pred_graph_nkh)$color <- pred_colors

#Set layout
#Add missing vertices
layout <- as.data.frame(layout)
layout$vertex_name <- names(V(pred_graph))

missing_verts <- layout$vertex_name[!(layout$vertex_name %in% names(V(pred_graph_nkh)))]
pred_graph_nkh <- add_vertices(pred_graph_nkh,length(missing_verts), name = missing_verts)

layout_nkh <- layout[match(names(V(pred_graph_nkh)),layout$vertex_name), ]
layout_nkh <- as.matrix(layout_nkh %>% select(-vertex_name))
layout <- as.matrix(layout %>% select(-vertex_name))

p1_test <- as.ggplot(expression(
  plot(
    pred_graph,
    vertex.label = NA,
    vertex.size = 5,
    layout = layout,
    edge.width = E(pred_graph)$size,
    vertex.color = "orange"
  ),
  title("All Social or Familial Relationships",
        cex.main = .8),
  legend(
    x = .3,
    y = 1,
    legend = c("True Positive", "False Negative"),
    col = c("blue", "grey"),
    lty = c(1, 1),
    lwd = 2,
    bty = "n"
  )
))
set.seed(1)
p2_test <- as.ggplot(expression(
  plot(
    pred_graph_nkh,
    vertex.label = NA,
    vertex.size = 5,
    layout = layout_nkh,
    edge.width = E(pred_graph_nkh)$size,
    vertex.color = "orange"
  ),
  title("Non-Kin Different House Relationships",
        cex.main = .8),
  legend(
    x = .3,
    y = 1.1,
    legend = c("True Positive", "False Negative"),
    col = c("blue", "grey"),
    lty = c(1, 1),
    lwd = 2,
    bty = "n"
  )
)) 

Downsamp_all_stable_ties <- readr::read_tsv('data/export/Downsamp_all_stable_ties.tsv')
Downsamp_all_stable_ties_nkh <- readr::read_tsv('data/export/Downsamp_all_stable_ties_nkh.tsv')

cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)
foreach(i = c(1:5)) %dopar% {
  print(i)
  inds <- c(1:nrow(Downsamp_all_stable_ties))
  test1_inds <- sample(inds, size = floor(.33*nrow(Downsamp_all_stable_ties)), replace = F)
  train1_inds <- inds[!(inds %in% test1_inds)]
  test2_inds <- sample(train1_inds, size = floor(.5*length(train1_inds)), replace = F)
  train2_inds <- inds[!(inds %in% test2_inds)]
  test3_inds <- train2_inds[!(train2_inds %in% test1_inds)]
  train3_inds <- inds[!(inds %in% test3_inds)]

  train1 <- Downsamp_all_stable_ties[train1_inds,]
  test1 <- Downsamp_all_stable_ties[test1_inds,]
  train2 <- Downsamp_all_stable_ties[train2_inds,]
  test2 <- Downsamp_all_stable_ties[test2_inds,]
  train3 <- Downsamp_all_stable_ties[train3_inds,]
  test3 <- Downsamp_all_stable_ties[test3_inds,]

  logit.test.1 <- lme4::glmer(related ~ strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2 <- lme4::glmer(related ~ strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3 <- lme4::glmer(related ~ strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  preds.1 <- predict(logit.test.1, newdata = test1, type = "response")
  preds.2 <- predict(logit.test.2, newdata = test2, type = "response")
  preds.3 <- predict(logit.test.3, newdata = test3, type = "response")
  names(preds.3) <- test3_inds
  names(preds.2) <- test2_inds
  names(preds.1) <- test1_inds
  preds.round <- c(preds.1, preds.2, preds.3)
  preds.round <- preds.round[order(as.numeric(names(preds.round)))]
  
  
  logit.test.1 <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        household_same +(1|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2 <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        household_same + (1|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3 <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        household_same + (1|village_code_w3),
                              data = train3, family = binomial)
  preds.1 <- predict(logit.test.1, newdata = test1, type = "response")
  preds.2 <- predict(logit.test.2, newdata = test2, type = "response")
  preds.3 <- predict(logit.test.3, newdata = test3, type = "response")
  names(preds.3) <- test3_inds
  names(preds.2) <- test2_inds
  names(preds.1) <- test1_inds
  preds.onlycovars <- c(preds.1, preds.2, preds.3)
  preds.onlycovars <- preds.onlycovars[order(as.numeric(names(preds.onlycovars)))]
  

  logit.test.1.covars <- lme4::glmer(related ~ gender.mm + gender.mf + 
                                     age_difference_abs + average_age  +
                                    strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2.covars <- lme4::glmer(related ~ gender.mm + gender.mf +
                                       age_difference_abs + average_age +
                                       strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3.covars <- lme4::glmer(related ~ gender.mm + gender.mf + 
                                       age_difference_abs + average_age +
                                       strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  
  preds.1.covars <- predict(logit.test.1.covars, newdata = test1, type = "response")
  preds.2.covars <- predict(logit.test.2.covars, newdata = test2, type = "response")
  preds.3.covars <- predict(logit.test.3.covars, newdata = test3, type = "response")
  names(preds.3.covars) <- test3_inds
  names(preds.2.covars) <- test2_inds
  names(preds.1.covars) <- test1_inds
  preds.round.covars <- c(preds.1.covars, preds.2.covars, preds.3.covars)
  preds.round.covars <- preds.round.covars[order(as.numeric(names(preds.round.covars)))]
  
  
  logit.test.1.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  
  preds.1.all <- predict(logit.test.1.all, newdata = test1, type = "response")
  preds.2.all <- predict(logit.test.2.all, newdata = test2, type = "response")
  preds.3.all <- predict(logit.test.3.all, newdata = test3, type = "response")
  names(preds.3.all) <- test3_inds
  names(preds.2.all) <- test2_inds
  names(preds.1.all) <- test1_inds
  preds.round.all <- c(preds.1.all, preds.2.all, preds.3.all)
  preds.round.all <- preds.round.all[order(as.numeric(names(preds.round.all)))]
  
  list(preds.round, preds.onlycovars, preds.round.covars, preds.round.all)
} -> preds_all
parallel::stopCluster(cl)

pred.all <- rowSums(sapply(preds_all, `[[`, 1))/5
pred.all.onlycovars <- rowSums(sapply(preds_all, `[[`, 2))/5
pred.all.covars <- rowSums(sapply(preds_all, `[[`, 3))/5
pred.all.all <- rowSums(sapply(preds_all, `[[`, 4))/5

#add on predictions from strain-sharing only model
pred.all.acc <- pred.all.all
pred.all.acc[pred.all.acc >=.5] <- 1
pred.all.acc[pred.all.acc <.5] <- 0

Downsamp_all_stable_ties$predicted <- pred.all.acc

logit.base.roc <- pROC::roc(Downsamp_all_stable_ties$related, pred.all, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")
logit.onlycovars.roc <- pROC::roc(Downsamp_all_stable_ties$related, pred.all.onlycovars, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")
logit.covars.roc <- pROC::roc(Downsamp_all_stable_ties$related, pred.all.covars, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")
logit.all.roc <- pROC::roc(Downsamp_all_stable_ties$related, pred.all.all, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")


roc.list <- list("Strain-Sharing Rate" = logit.base.roc,
                  "Only Socio-Dem Covars" = logit.onlycovars.roc,
                  "SSR + Age + Gender + Household Sharing" = logit.covars.roc,
                  "SSR + All Socio-Dem Covars" = logit.all.roc)

ci.list <- lapply(roc.list, ci.se, specificities = seq(0, 1, l = 25))

dat.ci.list <- lapply(ci.list, function(ciobj) 
  data.frame(x = as.numeric(rownames(ciobj)),
             lower = ciobj[, 1],
             upper = ciobj[, 3]))

roc.plot.stable <- ggroc(roc.list) +
  theme_minimal() +
  geom_abline(slope=1, intercept = 1, linetype = "dashed", alpha=1, color = "grey") +
  coord_equal()

for(i in 1:4) {
  roc.plot.stable <- roc.plot.stable + geom_ribbon(
    data = dat.ci.list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
  } 

roc.plot.stable <- roc.plot.stable + 
theme(plot.title = element_text(size=10, hjust = .5),
      legend.position = c(0.6, 0.2),
      legend.text = element_text(size=10),
      text = element_text(size = 10)) + 
      scale_color_manual(name = "AUCs:",
                         values=c("Red", 'Orange', "Green", "Blue"), 
                       labels=c(paste("Strain-Sharing Rate: ", round(logit.base.roc$auc,2),
                                      " ± ",round(sqrt(pROC::var(logit.base.roc)),3), sep=""), 
                                paste("Only Socio-Dem Covars:", round(logit.onlycovars.roc$auc,2 ),
                                      " ± ",round(sqrt(pROC::var(logit.onlycovars.roc)),3), sep=""), 
                                paste("SSR + Age + Gender: ", round(logit.covars.roc$auc, 2),
                                      " ± ",round(sqrt(pROC::var(logit.covars.roc)),3), sep=""),
                                paste("SSR + All Socio-Dem Covars: ", round(logit.all.roc$auc, 2),
                                      " ± ",round(sqrt(pROC::var(logit.all.roc)),3), sep="")
                                )
                       ) +
  ggtitle("All Social or Familial Relationships (Stable ties)")

roc.plot.stable <- roc.plot.stable +
  theme_pubr(legend =  c(0.6, 0.2)) +
  theme(plot.title = element_text(size = 10, hjust = .5)) +
  labs_pubr()

roc.plot.stable <- roc.plot.stable + theme(
  plot.title = element_text(size = 10, hjust = .5),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8)
)

cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)
foreach(i = c(1:5)) %dopar% {
  inds <- c(1:nrow(Downsamp_all_stable_ties_nkh))
  test1_inds <- sample(inds, size = floor(.33*nrow(Downsamp_all_stable_ties_nkh)), replace = F)
  train1_inds <- inds[!(inds %in% test1_inds)]
  test2_inds <- sample(train1_inds, size = floor(.5*length(train1_inds)), replace = F)
  train2_inds <- inds[!(inds %in% test2_inds)]
  test3_inds <- train2_inds[!(train2_inds %in% test1_inds)]
  train3_inds <- inds[!(inds %in% test3_inds)]
  
  train1 <- Downsamp_all_stable_ties_nkh[train1_inds,]
  test1 <- Downsamp_all_stable_ties_nkh[test1_inds,]
  train2 <- Downsamp_all_stable_ties_nkh[train2_inds,]
  test2 <- Downsamp_all_stable_ties_nkh[test2_inds,]
  train3 <- Downsamp_all_stable_ties_nkh[train3_inds,]
  test3 <- Downsamp_all_stable_ties_nkh[test3_inds,]
  
  logit.test.1 <- lme4::glmer(related ~ strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2 <- lme4::glmer(related ~ strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3 <- lme4::glmer(related ~ strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  preds.1 <- predict(logit.test.1, newdata = test1, type = "response")
  preds.2 <- predict(logit.test.2, newdata = test2, type = "response")
  preds.3 <- predict(logit.test.3, newdata = test3, type = "response")
  names(preds.3) <- test3_inds
  names(preds.2) <- test2_inds
  names(preds.1) <- test1_inds
  preds.round <- c(preds.1, preds.2, preds.3)
  preds.round <- preds.round[order(as.numeric(names(preds.round)))]
  
    
  logit.test.1 <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        household_same +(1|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2 <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        household_same + (1|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3 <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        household_same + (1|village_code_w3),
                              data = train3, family = binomial)
  preds.1 <- predict(logit.test.1, newdata = test1, type = "response")
  preds.2 <- predict(logit.test.2, newdata = test2, type = "response")
  preds.3 <- predict(logit.test.3, newdata = test3, type = "response")
  names(preds.3) <- test3_inds
  names(preds.2) <- test2_inds
  names(preds.1) <- test1_inds
  preds.onlycovars <- c(preds.1, preds.2, preds.3)
  preds.onlycovars <- preds.onlycovars[order(as.numeric(names(preds.onlycovars)))]
  

  logit.test.1.covars <- lme4::glmer(related ~ gender.mm + gender.mf + 
                                     age_difference_abs + average_age  +
                                    strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2.covars <- lme4::glmer(related ~ gender.mm + gender.mf +
                                       age_difference_abs + average_age +
                                       strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3.covars <- lme4::glmer(related ~ gender.mm + gender.mf + 
                                       age_difference_abs + average_age +
                                       strain_sharing_rate + household_same + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  
  preds.1.covars <- predict(logit.test.1.covars, newdata = test1, type = "response")
  preds.2.covars <- predict(logit.test.2.covars, newdata = test2, type = "response")
  preds.3.covars <- predict(logit.test.3.covars, newdata = test3, type = "response")
  names(preds.3.covars) <- test3_inds
  names(preds.2.covars) <- test2_inds
  names(preds.1.covars) <- test1_inds
  preds.round.covars <- c(preds.1.covars, preds.2.covars, preds.3.covars)
  preds.round.covars <- preds.round.covars[order(as.numeric(names(preds.round.covars)))]
  
  logit.test.1.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train1, family = binomial)
  logit.test.2.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train2, family = binomial)
  logit.test.3.all <- lme4::glmer(related ~ gender.mm + gender.mf + indigenous.both +
                        indigenous.one + religion.same + age_difference_abs +
                        average_age + wealth_difference_abs + average_wealth +
                        education_difference_abs + average_education +
                        average_diet + different_diet + average_bristol + different_bristol + watersource_same + 
                        same_usage_painkillers + same_usage_antibiotics + same_usage_antidiarrheal + same_usage_antiparasite + same_usage_vitamins + same_usage_zinc + same_usage_antifungal + same_usage_antihypertensives + same_usage_antidiabetics + same_usage_antiacids + same_usage_laxatives +
                        strain_sharing_rate + (0+strain_sharing_rate|village_code_w3),
                              data = train3, family = binomial)
  
  preds.1.all <- predict(logit.test.1.all, newdata = test1, type = "response")
  preds.2.all <- predict(logit.test.2.all, newdata = test2, type = "response")
  preds.3.all <- predict(logit.test.3.all, newdata = test3, type = "response")
  names(preds.3.all) <- test3_inds
  names(preds.2.all) <- test2_inds
  names(preds.1.all) <- test1_inds
  preds.round.all <- c(preds.1.all, preds.2.all, preds.3.all)
  preds.round.all <- preds.round.all[order(as.numeric(names(preds.round.all)))]
  
  list(preds.round, preds.onlycovars, preds.round.covars, preds.round.all)
} -> preds_nkh
parallel::stopCluster(cl)

pred.all <- rowSums(sapply(preds_nkh, `[[`, 1))/5
pred.all.onlycovars <- rowSums(sapply(preds_nkh, `[[`, 2))/5
pred.all.covars <- rowSums(sapply(preds_nkh, `[[`, 3))/5
pred.all.all <- rowSums(sapply(preds_nkh, `[[`, 4))/5

pred.all.acc <- pred.all.all
pred.all.acc[pred.all.acc >=.5] <- 1
pred.all.acc[pred.all.acc <.5] <- 0

Downsamp_all_stable_ties_nkh$predicted <- pred.all.acc

logit.base.roc <- pROC::roc(Downsamp_all_stable_ties_nkh$related, pred.all, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")
logit.onlycovars.roc <- pROC::roc(Downsamp_all_stable_ties_nkh$related, pred.all.onlycovars, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")
logit.covars.roc <- pROC::roc(Downsamp_all_stable_ties_nkh$related, pred.all.covars, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")
logit.all.roc <- pROC::roc(Downsamp_all_stable_ties_nkh$related, pred.all.all, ci=TRUE, plot=FALSE,
                levels=c("0","1"), direction = "<")


roc.list <- list("Strain-Sharing Rate (Stable ties)" = logit.base.roc,
                  "Only Socio-Dem Covars" = logit.onlycovars.roc,
                  "SSR + Age + Gender + Household Sharing" = logit.covars.roc,
                  "SSR + All Socio-Dem Covars (Stable ties)" = logit.all.roc)

ci.list <- lapply(roc.list, ci.se, specificities = seq(0, 1, l = 25))

dat.ci.list <- lapply(ci.list, function(ciobj) 
  data.frame(x = as.numeric(rownames(ciobj)),
             lower = ciobj[, 1],
             upper = ciobj[, 3]))

roc.plot.nkh.stable <- ggroc(roc.list) +
  theme_minimal() +
  geom_abline(slope=1, intercept = 1, linetype = "dashed", alpha=1, color = "grey") +
  coord_equal()

for(i in 1:4) {
  roc.plot.nkh.stable <- roc.plot.nkh.stable + geom_ribbon(
    data = dat.ci.list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
  } 

roc.plot.nkh.stable <- roc.plot.nkh.stable + 
theme(plot.title = element_text(size=10, hjust = .5),
      legend.position = c(0.6, 0.2),
      legend.text = element_text(size=10),
      text = element_text(size = 10)) + 
      scale_color_manual(name = "AUCs:",
                         values=c("Red", 'Orange', "Green", "Blue"), 
                       labels=c(paste("Strain-Sharing Rate: ", round(logit.base.roc$auc,2),
                                      " ± ",round(sqrt(pROC::var(logit.base.roc)),3), sep=""), 
                                paste("Only Socio-Dem Covars:", round(logit.onlycovars.roc$auc,2 ),
                                      " ± ",round(sqrt(pROC::var(logit.onlycovars.roc)),3), sep=""), 
                                paste("SSR + Age + Gender: ", round(logit.covars.roc$auc, 2),
                                      " ± ",round(sqrt(pROC::var(logit.covars.roc)),3), sep=""),
                                paste("SSR + All Socio-Dem Covars: ", round(logit.all.roc$auc, 2),
                                      " ± ",round(sqrt(pROC::var(logit.all.roc)),3), sep="")
                                )
                       ) +
  ggtitle("Non-Kin Different House Relationships (Stable ties)")

roc.plot.nkh.stable <- roc.plot.nkh.stable +
  theme_pubr(legend =  c(0.6, 0.2)) +
  theme(plot.title = element_text(size = 10, hjust = .5)) +
  labs_pubr()

roc.plot.nkh.stable <- roc.plot.nkh.stable + theme(
  plot.title = element_text(size = 10, hjust = .5),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8)
)
netw <- ggarrange(p1_test, p2_test, nrow = 2, labels = c('A','D'), widths = 1)
rocs_1 <-ggarrange(roc.plot, roc.plot.nkh, nrow = 2, labels = c("B", "E"), widths = 1)
rocs_2 <-ggarrange(roc.plot.stable, roc.plot.nkh.stable, nrow = 2, labels = c("C", "F"), widths = 1)
svglite('Figure2.svg', fix_text_size = FALSE)
ggarrange(netw, rocs_1, rocs_2)
dev.off()