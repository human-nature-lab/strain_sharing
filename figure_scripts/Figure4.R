library(ggplot)
library(ggpubr)
library(dplyr)
library(igraph)
library(ggplotify)

centrality_sim_df <- readr::read_tsv('../data/export/centrality_sim_df.tsv')
deg_c_sharing_first <-
  ggplot(centrality_sim_df, aes(x = degree_c, y = sim_deg_one)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  ylab("Average Strain-Sharing Rate\nWith First Degree Connections (%)") +
  xlab("Degree Centrality") +
  theme_pubr() +
  labs_pubr()

deg_c_sharing_all <-
  ggplot(centrality_sim_df, aes(x = degree_c, y = strain_sim_all)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  ylab("Average Strain-Sharing Rate\nWith All Village Members (%)") +
  xlab("Degree Centrality") +
  theme_pubr() +
  labs_pubr()

deg_df <- readr::read_tsv('../data/export/ssr_by_degree_plot_df.tsv')
ssr_by_degree_plot <- ggplot(deg_df, aes(x=Degree, y=Precision, group=Model, color = Model)) +
  geom_line(aes(color=Model, linetype = Model))+
  geom_pointrange(aes(ymin=Precision-SD, ymax=Precision+SD))+
  scale_linetype_manual(values=c("dashed", "solid")) + 
  scale_color_manual(values = c("#FC4E07", "#2E9FDF"))+
  #ggtitle("Average Strain-Sharing Rate By Geodesic Distance") +
  ylab("Median Strain-Sharing Rate (%)") + 
  xlab("Geodesic Distance") +
  theme_pubr() +
  theme(legend.position = c(0.75, 0.75),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  labs_pubr()

ssr_by_degree_plot <- ggarrange(ssr_by_degree_plot, labels = "A")

set.seed(22)
test <- readr::read_csv('../data/export/village_15_adj_mat.tsv')
test <- test[,-1]
test <- as.matrix(test)
rownames(test) <- colnames(test)
test <- igraph::graph_from_adjacency_matrix(test, mode = 'undirected')

vertex_attr(test)$similarity <- centrality_sim_df$strain_sim_all[match(names(V(test)),
                                           centrality_sim_df$respondent_master_id)]


palette <- heat.colors(3)

palette <- rev(palette)


#Trichotomize sharing
vertex_attr(test)$color <- case_when(
  vertex_attr(test)$similarity <= quantile(vertex_attr(test)$similarity, .33) ~ palette[1],
  vertex_attr(test)$similarity <= quantile(vertex_attr(test)$similarity, .66) ~ palette[2],
  TRUE ~ palette[3],
)

vertex_attr(test)$size <- (centrality_sim_df$degree_c[match(names(V(test)),
                                           centrality_sim_df$respondent_master_id)] +1) /2

set.seed(22)
strain_sim_cent_network <- as.ggplot(expression(
  plot(
    test,
    vertex.label = NA,
    layout = layout_with_fr(test)
  ),
  legend(x = -.5,
         y = 1.4,
    #'topleft',
    legend = c(">= 4.6% (Top Tertile)",
                  ">= 3.9% (Middle Tertile)",
                  ">= 0% (Bottom Tertile)"),
    pt.bg  = rev(palette),
    pch    = 21,
    cex    = .75,
    bty    = "n",
    y.intersp = 1.25,
    title = "Average Strain-Sharing Rate With\nAll Village Members"
  )
))

set.seed(22)

vertex_attr(test)$similarity <- centrality_sim_df$sim_deg_one[match(names(V(test)),
                                           centrality_sim_df$respondent_master_id)]
palette <- heat.colors(3)
palette <- rev(palette)

vertex_attr(test)$color <- palette[order(vertex_attr(test)$similarity)]

#Trichotomize sharing
vertex_attr(test)$color <- case_when(
  vertex_attr(test)$similarity <= quantile(vertex_attr(test)$similarity, .33) ~ palette[1],
  vertex_attr(test)$similarity <= quantile(vertex_attr(test)$similarity, .66) ~ palette[2],
  TRUE ~ palette[3],
)

vertex_attr(test)$size <- (centrality_sim_df$degree_c[match(names(V(test)),
                                           centrality_sim_df$respondent_master_id)] +1) /2

set.seed(22)
sim_deg_one_network <- as.ggplot(expression(
  plot(
    test,
    vertex.label = NA,
    layout = layout_with_fr
  ),
  legend(x = -.5,
         y = 1.4,
    #'topleft',
    legend = c(">= 9.7% (Top Tertile)",
                  ">= 7.3% (Middle Tertile)",
                  ">= 0% (Bottom Tertile)"),
    pt.bg  = rev(palette),
    pch    = 21,
    cex    = .75,
    bty    = "n",
    y.intersp = 1.25,
    title = "Average Strain-Sharing Rate With\nFirst Degree Connections"
  )
))

sim_scatter_fig <- ggarrange(deg_c_sharing_all,
          deg_c_sharing_first,
          nrow = 2,
          labels = c("B", "D"))

sim_network_fig <- ggarrange(strain_sim_cent_network,
          sim_deg_one_network,
          nrow = 2,
          labels = c("C", "E"))

svglite("Figure4.svg",
        width = 14,
        height = 8,
        fix_text_size = FALSE)
ggarrange(ssr_by_degree_plot,sim_scatter_fig, sim_network_fig,nrow = 1)
dev.off()
