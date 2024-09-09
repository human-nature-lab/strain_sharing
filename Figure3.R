library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggplotify)
library(ggpubr)
library(foreach)
library(svglite)

wide_ssr <- readr::read_tsv('data/export/longitudinal_networks_long_ssr.tsv')

village_map <- tribble(~village_code, ~village_name_deid,
  21 , "Azpeitia",
  26 , "Mungia",
  116 , "Basauri",
  162 , "Erandio",
)

foreach(i = c(26, 162, 21, 116)) %do% {
    wide_ssr |>
    filter(tie == 'yes', village_code_ego == i) |>
    mutate(c = T1 > median(T1, na.rm = TRUE)) |>
    graph_from_data_frame(directed=FALSE) -> sg_t1
  
    wide_ssr |>
    filter(tie == 'yes', village_code_ego == i) |>
    mutate(c = T2 > median(T1, na.rm = TRUE)) |>
    graph_from_data_frame(directed=FALSE) -> sg_t2

  pred_colors <- edge_attr(sg_t1, "c")
  E(sg_t1)$size <- 2
  pred_colors[pred_colors == TRUE] <- "#027fffff"
  pred_colors[pred_colors == FALSE] <- "grey"
  E(sg_t1)$color <- pred_colors

  pred_colors <- edge_attr(sg_t2, "c")
  E(sg_t2)$size <- 2
  pred_colors[pred_colors == TRUE] <- "#027fffff"
  pred_colors[pred_colors == FALSE] <- "grey"
  E(sg_t2)$color <- pred_colors

  set.seed(1993)
  layout <- layout.fruchterman.reingold(sg_t1)
  v_t1 <- as.ggplot(expression(par(mar=c(0,0,0,0)+.1),plot(sg_t1,
       vertex.label = NA,
       vertex.size = 3,
       layout = layout,
       edge.width = E(sg_t1)$size,
       main = paste('T1', village_map[village_map$village_code == i,'village_name_deid', TRUE]))
      ))
  v_t2 <- as.ggplot(expression(par(mar=c(0,0,0,0)+.1), plot(sg_t2,
       vertex.label = NA,
       vertex.size = 3,
       layout = layout,
       edge.width = E(sg_t2)$size, 
       main = paste('T2', village_map[village_map$village_code == i,'village_name_deid', TRUE])),
  legend("topleft",
         legend=c(">median", "<median"),
         col=c("#027fffff", "grey"),
         lty = c(1,1),
         lwd = 2,
         bty = "n")))
  ggarrange(v_t1, v_t2, nrow =2, heights = c(1,1))
} -> plots_t1t2

plots_t1t2 <- ggarrange(plotlist = plots_t1t2, nrow = 1)

sharing_rate_only_followup <- readr::read_tsv('data/export/longitudinal_sharing_rate.tsv')
sharing_rate_only_followup$village_code_ego <- as.factor(sharing_rate_only_followup$village_code_ego)
sharing_rate_only_followup$T2 <- scale(sharing_rate_only_followup$T2)
sharing_rate_only_followup$T1 <- scale(sharing_rate_only_followup$T1)
sharing_rate_only_followup$maha_dist <- scale(sharing_rate_only_followup$maha_dist)

lmer_first_model  <- lmerTest::lmer(T2 ~ T1 + relationship + maha_dist + (1|village_code_ego) + (1|ego), data = sharing_rate_only_followup)
dotplot_compare_models <- data.frame(summary(lmer_first_model)$coef[2:4,1:2], class = 'no_interact') |>
  tibble::rownames_to_column('term') |>
  mutate(term = stringr::str_remove(term, '...[0-9]')) |>
  rename(SE = `Std..Error`) |>
  mutate(term = factor(term, levels = unique(term))) |>
  ggplot(aes(reorder(term, desc(term)), Estimate, group=class)) +
  geom_errorbar(aes(ymin=Estimate-1.96*SE, ymax=Estimate+1.96*SE, color = class),size = 1.5, width=.2, position = position_dodge(width = 0.4)) +
  geom_point(aes(color = class), size = 4, position = position_dodge(width = 0.4)) +
  coord_flip() +
  cowplot::theme_cowplot() + xlab('Terms') +
  scale_color_manual( values = c('orange', '#440154')) +
  guides(color = 'none')

svglite('Figure3.svg', fix_text_size = FALSE, bg='transparent', height = 11, width = 23)
ggarrange(plots_t1t2, ggarrange(dotplot_compare_models, ggplot() + theme_minimal(), heights = c(0.75, 0.25), ncol = 1), nrow = 1, labels = c('A', 'B'), widths = c(1, .3))
dev.off()

svglite('Figure3b.svg', fix_text_size = FALSE, bg='transparent', height = 11, width = 23)
dotplot_compare_models
dev.off()
