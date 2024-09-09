library(ggplot)
library(ggpubr)
library(dplyr)
library(igraph)

centrality_sim_df <- readr::read_tsv('../data/export/centrality_sim_df.tsv')
centrality_sim_df$cluster_bucket <- case_when(
  centrality_sim_df$cluster_coef_all <= .25 ~ "0 - .25",
  centrality_sim_df$cluster_coef_all <= .5 ~ ".25 - .5",
  centrality_sim_df$cluster_coef_all <= .75 ~ ".5 - .75",
  centrality_sim_df$cluster_coef_all <= 1 ~ ".75 - 1"
  )

cluster_pvals <- compare_means(sim_deg_one ~ cluster_bucket,
                               centrality_sim_df,
                               p.adjust.method = "BH")

cluster_pvals <- cluster_pvals %>%
  filter(p.signif != "ns") %>%
  mutate(y.position = c(25, 27.5,30,32.5,35))

cluster_pvals$p.adj <- paste0("p = ",cluster_pvals$p.adj)

cluster_bucket_fig <- ggboxplot(
  centrality_sim_df,
  x = "cluster_bucket",
  y = "sim_deg_one",
  outlier.shape = NA,
  color = "cluster_bucket",
  show.legend = FALSE
) +
  # geom_jitter(
  #   aes(color = cluster_bucket),
  #   alpha = .5,
  #   width = .25,
  #   show.legend = FALSE
  # ) +
  stat_pvalue_manual(cluster_pvals, label = "p.adj", tip.length = 0) +
  xlab("Clustering Coefficient") +
  ylab("Average Strain-Sharing Rate\nWith First Degree Connections (%)") +
  theme_pubr() +
  labs_pubr() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0,40))



add.alpha <- function(col, alpha = 1) {
  if (missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb) / 255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha = alpha))
}

SN_Village <- read.csv('../data/export/sn_basauri.tsv')
strain_vil <- read.csv('../data/export/mbiome_basauri.tsv', check.names = FALSE)
rownames(strain_vil) <- strain_vil[,1]
strain_vil <- strain_vil[,-1]

sn_vil <-
  igraph::simplify(graph_from_data_frame(SN_Village, directed = FALSE))

mbiome_all <-
  simplify(graph_from_adjacency_matrix(as.matrix(strain_vil), mode = "undirected", weighted = TRUE))

#Cluster
set.seed(450)
set.seed(769)
mbiome_cluster <-  cluster_louvain(mbiome_all, weights = E(mbiome_all)$weight )
sn_cluster <- cluster_louvain(sn_vil, resolution = 1)

max_cluster <-
  max(max(membership(mbiome_cluster)), max(membership(sn_cluster)))

colors <- pals::cols25(max_cluster)


temp <- membership(mbiome_cluster)
swap <- c(temp)
swap <- case_when(swap == 4 ~ 5,
                  swap == 3 ~ 2,
                  swap == 2 ~ 4,
                  swap == 1 ~ 3,
                  TRUE ~ swap)
names(swap) <- names(temp)

V(sn_vil)$color <- colors[membership(sn_cluster)]
sn_colors <- colors[membership(sn_cluster)]

set.seed(1)
illustrative_village_sn <- as.ggplot(expression(plot(#sn_cluster,
  sn_vil,
  vertex.label = NA,
  vertex.size = 5,
  mark.col = colors,
  mark.border = colors,
  col = sn_colors,
  edge.color = "black",
  layout = layout_with_fr,
  #main = "Social Network Clusters"
),
title("Social Network Clusters",line = 0)))

V(sn_vil)$color <- swap[match(V(sn_vil)$name, names(swap))]
V(sn_vil)$color <- colors[as.numeric(V(sn_vil)$color)]

set.seed(1)
illustrative_village_overlay <- as.ggplot(expression(plot(
  sn_vil,
  vertex.label = NA,
  vertex.size = 5,
  edge.color = "black",
  layout = layout_with_fr#,
  #main = "Microbiome Similarity Clusters\non Social Network"
),
title("Microbiome Similarity Clusters\non Social Network", line = -.75)))

colors <- add.alpha(colors, alpha = .5)
mbiome_cols <- colors[swap]
mbiome_cols_rect <- colors
mbiome_cols_rect <- mbiome_cols_rect[c(3,4,2,5)]
V(mbiome_all)$color <- mbiome_cols

set.seed(15)
illustrative_village_mbiome <- as.ggplot(expression(plot(
  mbiome_cluster,
  mbiome_all,
  col = mbiome_cols,
  mark.col = mbiome_cols_rect,
  mark.border = mbiome_cols_rect,
  vertex.label = NA,
  vertex.size = 5,
  edge.width = E(mbiome_all)$weight / 25,
  edge.color = "black",
  layout = layout_with_fr#,
  #main = "Microbiome Similarity Clusters"
),
title("Microbiome Similarity Clusters",line = 0)))

master_key <- readr::read_tsv('../data/export/master_key.tsv')

species_data <- readr::read_tsv("../data/mpa4_merged_profiles.tsv", skip = 1) |> 
  filter(stringr::str_detect(clade_name, 't__')) |> 
  select(clade_name, ends_with('-05')) |> 
  tibble::column_to_rownames('clade_name') 
species_data <- species_data[,master_key$mb_m05]
colnames(species_data) <- master_key$respondent_master_id[match(colnames(species_data), master_key$mb_m05)]

vil_graph <- readr::read_csv('../data/export/village_15_adj_mat.tsv')
vil_graph <- vil_graph[,-1]
vil_graph <- as.matrix(vil_graph)
rownames(vil_graph) <- colnames(vil_graph)
set.seed(22)
vil_graph <- igraph::graph_from_adjacency_matrix(vil_graph, mode = 'undirected')

clusters <- membership(cluster_louvain(vil_graph)) + 1500

species_for_niche <- unlist(species_data["k__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Enterococcaceae|g__Enterococcus|s__Enterococcus_faecium|t__SGB7967",]) / 100

ordered_species <-
  species_for_niche[match(names(clusters), names(species_for_niche))]


pal <- colorRampPalette(c("red", "yellow"))
my_pal <- pal(2)

test_cols <- my_pal[ceiling(ordered_species)+1]
V(vil_graph)$color <- test_cols[match(names(V(vil_graph)),
                                      names(ordered_species))]

V(vil_graph)$size <- log2(ordered_species*100000+5)+2

set.seed(22)
par(mar= c(0,0,0,0))
plot(
  vil_graph,
  vertex.label = NA
)
title(paste0("Differentially Abundant Species: Enterococcus_faecium (SGB7967)"),
      cex.main=1,
      line = -1)
legend(
"bottomleft",
  legend = c("Species Present",
             "Species Absent"),
  pt.bg  = rev(my_pal),
  pch    = 21,
  cex    = 1,
  border = NA,
  bty    = "n"
)
set.seed(22)
species_da_1 <- as.ggplot(expression(plot(
  vil_graph,
  vertex.label = NA
),
title(paste0("Differentially Abundant Species:\nEnterococcus_faecium (SGB7967)"),
      cex.main=1,
      line = -1),
legend(
  'bottomleft',
  legend = c("Species Present",
             "Species Absent"),
  pt.bg  = rev(my_pal),
  pch    = 21,
  cex    = 1,
  border = NA,
  bty    = "n"
)))

species_for_niche <- unlist(species_data["k__Bacteria|p__Actinobacteria|c__Coriobacteriia|o__Coriobacteriales|f__Coriobacteriaceae|g__GGB9379|s__GGB9379_SGB14372|t__SGB14372",]) /100
ordered_species <-
  species_for_niche[names(species_for_niche)]

pal <- colorRampPalette(c("red", "yellow"))
my_pal <- pal(2)

test_cols <- my_pal[ceiling(ordered_species)+1]
V(vil_graph)$color <- test_cols[match(names(V(vil_graph)),
                                      names(ordered_species))]

V(vil_graph)$size <- log2(ordered_species*100000+5)+2

set.seed(22)
species_da_2 <- as.ggplot(expression(plot(vil_graph,
     vertex.label = NA
     ),
title(
  paste0(
    "Differentially Abundant Species:\nCoriobacteriaceae (GGB9379_SGB14372)"
  ),
  cex.main = 1,
  line = -1
),
legend(
  'bottomleft',
  legend = c("Species Present",
             "Species Absent"),
  pt.bg  = rev(my_pal),
  pch    = 21,
  cex    = 1,
  border = NA,
  bty    = "n"
)))

observed <- readr::read_tsv('../data/export/observed_clusters.tsv')
null_clusters <- readr::read_tsv('../data/export/null_clusters.tsv')

p_vals <- data.frame(
  vils = observed$vils,
  p_val = c(
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[1]] >= observed$obs[1]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[2]] >= observed$obs[2]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[3]] >= observed$obs[3]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[4]] >= observed$obs[4]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[5]] >= observed$obs[5]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[6]] >= observed$obs[6]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[7]] >= observed$obs[7]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[8]] >= observed$obs[8]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[9]] >= observed$obs[9]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[10]] >= observed$obs[10]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[11]] >= observed$obs[11]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[12]] >= observed$obs[12]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[13]] >= observed$obs[13]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[14]] >= observed$obs[14]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[15]] >= observed$obs[15]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[16]] >= observed$obs[16]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[17]] >= observed$obs[17]) /
      10000,
    sum(null_clusters$rand_inds[null_clusters$vils ==
                                  observed$vils[18]] >= observed$obs[18]) /
      10000
  )
)

p_vals$p_val = paste0('p = ', p_vals$p_val)

cluster_pvals_fig <- 
  ggplot(null_clusters, aes(x=rand_inds ))+
  geom_histogram(color="black",fill="grey", bins = 100, show.legend = FALSE)+
  #facet_grid(as.factor(vils) ~ .) +
  facet_wrap(~ vils, ncol=2) +
  geom_vline(data=observed,
             aes(xintercept=obs, color="red"),
             linetype="solid", show.legend = FALSE) +
  theme_pubr() +
  ylab("Frequency") +
  xlab("Adjusted Rand Index") +
  ggtitle("Permutation Null Distributions")+
  scale_x_continuous(breaks = c(0, .2,.4)) +
  geom_text(data = p_vals, aes(label = p_val,
                               y = 2000,
                               x = .4)) +
  theme_pubr()+
  theme(
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(hjust = .5, face = "bold"),
    axis.title  = element_text(face = "bold"),
    axis.text  = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(0,3000)) +
  scale_y_continuous(limits = c(0, 4000), breaks = c(0, 1500, 3000))


fig5_1 <- ggarrange(cluster_bucket_fig,
          illustrative_village_mbiome,
          labels = c("A", "B"),
          nrow = 1)

fig5_2 <- ggarrange(illustrative_village_sn,
          illustrative_village_overlay,
          labels = c("C", "D"),
          nrow = 1)

#Need to get this from the species_niches script
fig5_3<- ggarrange(species_da_1, species_da_2,
          labels = c("F", "G"), nrow = 1)

fig5_4 <- ggarrange(fig5_1,
                    fig5_2,
                    fig5_3,
                    nrow = 3)

fig5 <- ggarrange(fig5_4,
                  ggarrange(cluster_pvals_fig, labels = "E"),
                  widths = c(2,1),
                  nrow = 1)
svglite("Figure5.svg",
        fix_text_size = FALSE,
        width = 12,
        height = 12)
fig5
dev.off()
