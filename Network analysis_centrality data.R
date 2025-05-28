#mentioning netowrk


library(igraph)
library(readxl)
library(ggraph)
library(dplyr)
library(writexl)
library(patchwork)  # For combining plots

# Load node list (assuming it's the same for all periods or adjust if different)
# If node attributes differ by period, load them per sheet similarly
node_list <- read_excel("C:/Users/wk2lam/Downloads/Feb 26_node list.xlsx")  # replace with your actual node list sheet name

# Load edges from each period and add period column
edges_2013_2017 <- read_excel("C:/Users/wk2lam/Downloads/Apr 7_target node.xlsx", sheet = "2013-2017") %>% mutate(Period = "2013-2017")
edges_2018_2019 <- read_excel("C:/Users/wk2lam/Downloads/Apr 7_target node.xlsx", sheet = "2018-2019") %>% mutate(Period = "2018-2019")
edges_2020_2024 <- read_excel("C:/Users/wk2lam/Downloads/Apr 7_target node.xlsx", sheet = "2020-2024") %>% mutate(Period = "2020-2024")

# Combine all edges into one dataframe
all_edges <- bind_rows(edges_2013_2017, edges_2018_2019, edges_2020_2024)

# Function to calculate network metrics for a given period
calculate_metrics <- function(edges_df, period_label) {
  edges_period <- edges_df %>% filter(Period == period_label)
  nodes_in_period <- unique(c(edges_period$Source, edges_period$Target))
  nodes_period <- node_list %>% filter(id %in% nodes_in_period)
  
  G <- graph_from_data_frame(d = edges_period[, c("Source", "Target")], vertices = nodes_period, directed = FALSE)
  
  network_size <- vcount(G)
  number_of_ties <- ecount(G)
  density <- edge_density(G)
  average_degree <- mean(degree(G))
  degree_centralization <- centralization.degree(G)$centralization * 100
  clustering_coeff <- transitivity(G, type = "global")
  avg_path_length <- average.path.length(G, directed = FALSE)
  diameter_val <- diameter(G, directed = FALSE)
  
  data.frame(
    Period = period_label,
    Network_Size = network_size,
    Number_of_Ties = number_of_ties,
    Density = density,
    Average_Degree = average_degree,
    Degree_Centralization = degree_centralization,
    Clustering_Coefficient = clustering_coeff,
    Average_Path_Length = avg_path_length,
    Diameter = diameter_val
  )
}

# Calculate metrics for each period
metrics_2013_2017 <- calculate_metrics(all_edges, "2013-2017")
metrics_2018_2019 <- calculate_metrics(all_edges, "2018-2019")
metrics_2020_2024 <- calculate_metrics(all_edges, "2020-2024")

# Combine metrics into one dataframe
metrics_all_periods <- bind_rows(metrics_2013_2017, metrics_2018_2019, metrics_2020_2024)


###overall data

# Combine all edges into one aggregated dataset (2013-2024)
all_edges <- bind_rows(edges_2013_2017, edges_2018_2019, edges_2020_2024)

# Get all unique nodes appearing in combined edges
all_nodes_ids <- unique(c(all_edges$Source, all_edges$Target))

# Filter master node list to only those nodes in combined edges
all_nodes <- node_list %>% filter(id %in% all_nodes_ids)

# Create combined graph for entire 2013-2024 period
G_combined <- graph_from_data_frame(d = all_edges[, c("Source", "Target")], vertices = all_nodes, directed = FALSE)

# Set node labels (adjust column name if different)
V(G_combined)$label <- all_nodes$label


# Calculate network metrics on combined graph
network_size <- vcount(G_combined)
number_of_ties <- ecount(G_combined)
density <- edge_density(G_combined)
average_degree <- mean(degree(G_combined))
degree_centralization <- centralization.degree(G_combined)$centralization * 100  # percent
clustering_coeff <- transitivity(G_combined, type = "global")
average_path_length <- average.path.length(G_combined, directed = FALSE)
diameter_val <- diameter(G_combined, directed = FALSE)


# Prepare metrics dataframe
overall_metrics <- data.frame(
  Period = "Overall 2013-2024",
  Network_Size = network_size,
  Number_of_Ties = number_of_ties,
  Density = density,
  Average_Degree = average_degree,
  Degree_Centralization = degree_centralization,
  Clustering_Coefficient = clustering_coeff,
  Average_Path_Length = average_path_length,
  Diameter = diameter_val
)

print(overall_metrics)


# Optionally, export to Excel
write_xlsx(bind_rows(metrics_all_periods, overall_metrics), "mentioning network_metrics_with_overall_mean.xlsx")


#Co-author network

library(igraph)
library(readxl)
library(ggraph)
library(dplyr)
library(writexl)
library(patchwork)  # For combining plots

# Load node list (assuming it's the same for all periods or adjust if different)
# If node attributes differ by period, load them per sheet similarly
node_list <- read_excel("C:/Users/wk2lam/Downloads/Feb 26_node list.xlsx")  # replace with your actual node list sheet name

# Load edges from each period and add period column
edges_2013_2017 <- read_excel("C:/Users/wk2lam/Downloads/Mar 28_person (1).xlsx", sheet = "2013-2017") %>% mutate(Period = "2013-2017")
edges_2018_2019 <- read_excel("C:/Users/wk2lam/Downloads/Mar 28_person (1).xlsx", sheet = "2018-2019") %>% mutate(Period = "2018-2019")
edges_2020_2024 <- read_excel("C:/Users/wk2lam/Downloads/Mar 28_person (1).xlsx", sheet = "2020-2024") %>% mutate(Period = "2020-2024")

# Combine all edges into one dataframe
all_edges <- bind_rows(edges_2013_2017, edges_2018_2019, edges_2020_2024)

# Function to calculate network metrics for a given period
calculate_metrics <- function(edges_df, period_label) {
  edges_period <- edges_df %>% filter(Period == period_label)
  nodes_in_period <- unique(c(edges_period$Source, edges_period$Target))
  nodes_period <- node_list %>% filter(id %in% nodes_in_period)
  
  G <- graph_from_data_frame(d = edges_period[, c("Source", "Target")], vertices = nodes_period, directed = FALSE)
  
  network_size <- vcount(G)
  number_of_ties <- ecount(G)
  density <- edge_density(G)
  average_degree <- mean(degree(G))
  degree_centralization <- centralization.degree(G)$centralization * 100
  clustering_coeff <- transitivity(G, type = "global")
  avg_path_length <- average.path.length(G, directed = FALSE)
  diameter_val <- diameter(G, directed = FALSE)
  
  data.frame(
    Period = period_label,
    Network_Size = network_size,
    Number_of_Ties = number_of_ties,
    Density = density,
    Average_Degree = average_degree,
    Degree_Centralization = degree_centralization,
    Clustering_Coefficient = clustering_coeff,
    Average_Path_Length = avg_path_length,
    Diameter = diameter_val
  )
}

# Calculate metrics for each period
metrics_2013_2017 <- calculate_metrics(all_edges, "2013-2017")
metrics_2018_2019 <- calculate_metrics(all_edges, "2018-2019")
metrics_2020_2024 <- calculate_metrics(all_edges, "2020-2024")

# Combine metrics into one dataframe
metrics_all_periods <- bind_rows(metrics_2013_2017, metrics_2018_2019, metrics_2020_2024)


###overall data

# Combine all edges into one aggregated dataset (2013-2024)
all_edges <- bind_rows(edges_2013_2017, edges_2018_2019, edges_2020_2024)

# Get all unique nodes appearing in combined edges
all_nodes_ids <- unique(c(all_edges$Source, all_edges$Target))

# Filter master node list to only those nodes in combined edges
all_nodes <- node_list %>% filter(id %in% all_nodes_ids)

# Create combined graph for entire 2013-2024 period
G_combined <- graph_from_data_frame(d = all_edges[, c("Source", "Target")], vertices = all_nodes, directed = FALSE)

# Set node labels (adjust column name if different)
V(G_combined)$label <- all_nodes$label


# Calculate network metrics on combined graph
network_size <- vcount(G_combined)
number_of_ties <- ecount(G_combined)
density <- edge_density(G_combined)
average_degree <- mean(degree(G_combined))
degree_centralization <- centralization.degree(G_combined)$centralization * 100  # percent
clustering_coeff <- transitivity(G_combined, type = "global")
average_path_length <- average.path.length(G_combined, directed = FALSE)
diameter_val <- diameter(G_combined, directed = FALSE)


# Prepare metrics dataframe
overall_metrics <- data.frame(
  Period = "Overall 2013-2024",
  Network_Size = network_size,
  Number_of_Ties = number_of_ties,
  Density = density,
  Average_Degree = average_degree,
  Degree_Centralization = degree_centralization,
  Clustering_Coefficient = clustering_coeff,
  Average_Path_Length = average_path_length,
  Diameter = diameter_val
)

print(overall_metrics)


# Optionally, export to Excel
write_xlsx(bind_rows(metrics_all_periods, overall_metrics), "Co-author network_metrics_with_overall_mean.xlsx")



#Inter-organization network

library(igraph)
library(readxl)
library(ggraph)
library(dplyr)
library(writexl)
library(patchwork)  # For combining plots

# Load node list (assuming it's the same for all periods or adjust if different)
# If node attributes differ by period, load them per sheet similarly
node_list <- read_excel("C:/Users/wk2lam/Downloads/Feb 26_node list.xlsx")  # replace with your actual node list sheet name

# Load edges from each period and add period column
edges_2013_2017 <- read_excel("C:/Users/wk2lam/Downloads/Mar 28_organization.xlsx", sheet = "2013-2017") %>% mutate(Period = "2013-2017")
edges_2018_2019 <- read_excel("C:/Users/wk2lam/Downloads/Mar 28_organization.xlsx", sheet = "2018-2019") %>% mutate(Period = "2018-2019")
edges_2020_2024 <- read_excel("C:/Users/wk2lam/Downloads/Mar 28_organization.xlsx", sheet = "2020-2024") %>% mutate(Period = "2020-2024")

# Combine all edges into one dataframe
all_edges <- bind_rows(edges_2013_2017, edges_2018_2019, edges_2020_2024)

# Function to calculate network metrics for a given period
calculate_metrics <- function(edges_df, period_label) {
  edges_period <- edges_df %>% filter(Period == period_label)
  nodes_in_period <- unique(c(edges_period$Source, edges_period$Target))
  nodes_period <- node_list %>% filter(id %in% nodes_in_period)
  
  G <- graph_from_data_frame(d = edges_period[, c("Source", "Target")], vertices = nodes_period, directed = FALSE)
  
  network_size <- vcount(G)
  number_of_ties <- ecount(G)
  density <- edge_density(G)
  average_degree <- mean(degree(G))
  degree_centralization <- centralization.degree(G)$centralization * 100
  clustering_coeff <- transitivity(G, type = "global")
  avg_path_length <- average.path.length(G, directed = FALSE)
  diameter_val <- diameter(G, directed = FALSE)
  
  data.frame(
    Period = period_label,
    Network_Size = network_size,
    Number_of_Ties = number_of_ties,
    Density = density,
    Average_Degree = average_degree,
    Degree_Centralization = degree_centralization,
    Clustering_Coefficient = clustering_coeff,
    Average_Path_Length = avg_path_length,
    Diameter = diameter_val
  )
}

# Calculate metrics for each period
metrics_2013_2017 <- calculate_metrics(all_edges, "2013-2017")
metrics_2018_2019 <- calculate_metrics(all_edges, "2018-2019")
metrics_2020_2024 <- calculate_metrics(all_edges, "2020-2024")

# Combine metrics into one dataframe
metrics_all_periods <- bind_rows(metrics_2013_2017, metrics_2018_2019, metrics_2020_2024)


###overall data

# Combine all edges into one aggregated dataset (2013-2024)
all_edges <- bind_rows(edges_2013_2017, edges_2018_2019, edges_2020_2024)

# Get all unique nodes appearing in combined edges
all_nodes_ids <- unique(c(all_edges$Source, all_edges$Target))

# Filter master node list to only those nodes in combined edges
all_nodes <- node_list %>% filter(id %in% all_nodes_ids)

# Create combined graph for entire 2013-2024 period
G_combined <- graph_from_data_frame(d = all_edges[, c("Source", "Target")], vertices = all_nodes, directed = FALSE)

# Set node labels (adjust column name if different)
V(G_combined)$label <- all_nodes$label


# Calculate network metrics on combined graph
network_size <- vcount(G_combined)
number_of_ties <- ecount(G_combined)
density <- edge_density(G_combined)
average_degree <- mean(degree(G_combined))
degree_centralization <- centralization.degree(G_combined)$centralization * 100  # percent
clustering_coeff <- transitivity(G_combined, type = "global")
average_path_length <- average.path.length(G_combined, directed = FALSE)
diameter_val <- diameter(G_combined, directed = FALSE)


# Prepare metrics dataframe
overall_metrics <- data.frame(
  Period = "Overall 2013-2024",
  Network_Size = network_size,
  Number_of_Ties = number_of_ties,
  Density = density,
  Average_Degree = average_degree,
  Degree_Centralization = degree_centralization,
  Clustering_Coefficient = clustering_coeff,
  Average_Path_Length = average_path_length,
  Diameter = diameter_val
)

print(overall_metrics)


# Optionally, export to Excel
write_xlsx(bind_rows(metrics_all_periods, overall_metrics), "Inter-organization network_metrics_with_overall_mean.xlsx")



















