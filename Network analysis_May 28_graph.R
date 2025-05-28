###Mentioning Network


# Load necessary libraries
library(readxl)
library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(writexl)
library(lubridate)
library(patchwork)

# 1. Load edge data
edges <- read_excel("C:/Users/wk2lam/Downloads/Apr 30_target node.xlsx", sheet = "All")
colnames(edges) <- c("Date", "Source", "Target")
edges$Date <- as.Date(edges$Date)

# 2. Load node list
node_list <- read_excel("C:/Users/wk2lam/Downloads/Feb 26_node list.xlsx", sheet = "Sheet1")
node_list$id <- as.character(node_list$id)

# 3. Assign periods to edges
edges <- edges %>%
  mutate(
    Period = case_when(
      Date >= as.Date("2013-01-01") & Date <= as.Date("2017-12-31") ~ "2013-2017",
      Date >= as.Date("2018-01-01") & Date <= as.Date("2019-12-31") ~ "2018-2019",
      Date >= as.Date("2020-01-01") & Date <= as.Date("2024-12-31") ~ "2020-2024",
      TRUE ~ NA_character_
    ),
    Source = as.character(Source),
    Target = as.character(Target)
  )

edges_periods <- edges %>% filter(!is.na(Period))
edges_overall <- edges_periods %>% mutate(Period = "Overall 2013-2024")
edges_all <- bind_rows(edges_periods, edges_overall)

# 4. Build node list from all unique nodes in combined edges, join with node_list for labels
all_nodes <- unique(c(edges_all$Source, edges_all$Target))
nodes <- data.frame(id = as.character(all_nodes)) %>%
  left_join(node_list, by = "id")

# Calculate global maximum values
calculate_global_maximums <- function(edges_all, nodes, periods) {
  max_values <- list(degree = 0, node_betweenness = 0, edge_betweenness = 0)
  
  for (period in periods) {
    edges_sub <- edges_all %>% filter(Period == period)
    g <- graph_from_data_frame(
      d = edges_sub[, c("Source", "Target")],
      vertices = nodes,
      directed = FALSE
    )
    
    # Update max degree if needed
    max_degree <- max(degree(g))
    if (max_degree > max_values$degree) max_values$degree <- max_degree
    
    # Update max node betweenness if needed
    max_node_btw <- max(betweenness(g))
    if (max_node_btw > max_values$node_betweenness) max_values$node_betweenness <- max_node_btw
    
    # Update max edge betweenness if needed
    max_edge_btw <- max(edge_betweenness(g))
    if (max_edge_btw > max_values$edge_betweenness) max_values$edge_betweenness <- max_edge_btw
  }
  
  return(max_values)
}

# Get global maximums before creating plots
periods <- unique(edges_all$Period)
global_max <- calculate_global_maximums(edges_all, nodes, periods)

# Enhanced function to analyze network with centrality metrics
analyze_network <- function(edge_df, node_df, period_label, global_max) {
  g <- graph_from_data_frame(
    d = edge_df[, c("Source", "Target")],
    vertices = node_df,
    directed = FALSE
  )
  
  # Calculate vertex degree and betweenness
  V(g)$degree <- degree(g)
  V(g)$betweenness <- betweenness(g)
  
  # Calculate edge betweenness
  E(g)$betweenness <- edge_betweenness(g)
  
  # Prepare node and edge data frames with centrality metrics
  node_data <- data.frame(
    id = V(g)$name,
    label = V(g)$label,
    degree = V(g)$degree,
    betweenness = V(g)$betweenness
  )
  edge_data <- as_data_frame(g, what = "edges")
  edge_data$betweenness <- E(g)$betweenness
  
  # Calculate network-level centralization metrics
  n_nodes <- vcount(g)
  n_edges <- ecount(g)
  dens <- edge_density(g)
  avg_deg <- mean(V(g)$degree)
  
  # Degree and betweenness centralization
  deg_centralization <- centralization.degree(g)$centralization * 100
  btw_centralization <- centr_betw(g)$centralization * 100
  
  metrics <- data.frame(
    Period = period_label,
    Network_Size = n_nodes,
    Number_of_Ties = n_edges,
    Density = dens,
    Average_Degree = avg_deg,
    Degree_Centralization = deg_centralization,
    Betweenness_Centralization = btw_centralization
  )
  
  # Create plot with consistent scales across all periods
  set.seed(123)
  p <- ggraph(g, layout = "kk") +
    geom_edge_link(aes(edge_width = betweenness), color = "black", alpha = 0.7) +
    geom_node_point(aes(size = betweenness, color = degree), alpha = 0.8) +
    geom_node_text(aes(label = label), repel = TRUE, size = 4.5) +
    # Use global maximums to set consistent limits
    scale_size_continuous(range = c(2, 10), name = "betweenness",
                          limits = c(0, global_max$node_betweenness)) +
    scale_color_gradient(low = "#DEEBF7", high = "#08519C", name = "degree",
                         limits = c(0, global_max$degree)) +
    scale_edge_width_continuous(range = c(0.2, 2), guide = "legend",
                                limits = c(0, global_max$edge_betweenness)) +
    theme_void() +
    ggtitle(paste("Mentioning network:", period_label)) +
    theme(legend.position = "bottom")
  
  return(list(metrics = metrics, node_data = node_data, edge_data = edge_data, plot = p))
}

# Analyze networks for each period + overall with consistent scales
results <- list()
for (period in periods) {
  edges_sub <- edges_all %>% filter(Period == period)
  results[[period]] <- analyze_network(edges_sub, nodes, period, global_max)
}

combined_plot <- wrap_plots(lapply(results, function(x) x$plot), ncol = 2) +
  plot_annotation(title = "Mentioning network (2013-2024)",
                  subtitle = "Node size represents betweenness centrality, color shows degree centrality")

print(combined_plot)

#####co-author
# Load necessary libraries
library(readxl)
library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(writexl)
library(lubridate)
library(patchwork)

# 1. Load edge data
edges <- read_excel("C:/Users/wk2lam/Downloads/Mar 28_person (1).xlsx", sheet = "All")
colnames(edges) <- c("Date", "Source", "Target")
edges$Date <- as.Date(edges$Date)

# 2. Load node list
node_list <- read_excel("C:/Users/wk2lam/Downloads/Feb 26_node list.xlsx", sheet = "Sheet1")
node_list$id <- as.character(node_list$id)

# 3. Assign periods to edges
edges <- edges %>%
  mutate(
    Period = case_when(
      Date >= as.Date("2013-01-01") & Date <= as.Date("2017-12-31") ~ "2013-2017",
      Date >= as.Date("2018-01-01") & Date <= as.Date("2019-12-31") ~ "2018-2019",
      Date >= as.Date("2020-01-01") & Date <= as.Date("2024-12-31") ~ "2020-2024",
      TRUE ~ NA_character_
    ),
    Source = as.character(Source),
    Target = as.character(Target)
  )

edges_periods <- edges %>% filter(!is.na(Period))
edges_overall <- edges_periods %>% mutate(Period = "Overall 2013-2024")
edges_all <- bind_rows(edges_periods, edges_overall)

# 4. Build node list from all unique nodes in combined edges, join with node_list for labels
all_nodes <- unique(c(edges_all$Source, edges_all$Target))
nodes <- data.frame(id = as.character(all_nodes)) %>%
  left_join(node_list, by = "id")

# Calculate global maximum values
calculate_global_maximums <- function(edges_all, nodes, periods) {
  max_values <- list(degree = 0, node_betweenness = 0, edge_betweenness = 0)
  
  for (period in periods) {
    edges_sub <- edges_all %>% filter(Period == period)
    g <- graph_from_data_frame(
      d = edges_sub[, c("Source", "Target")],
      vertices = nodes,
      directed = FALSE
    )
    
    # Update max degree if needed
    max_degree <- max(degree(g))
    if (max_degree > max_values$degree) max_values$degree <- max_degree
    
    # Update max node betweenness if needed
    max_node_btw <- max(betweenness(g))
    if (max_node_btw > max_values$node_betweenness) max_values$node_betweenness <- max_node_btw
    
    # Update max edge betweenness if needed
    max_edge_btw <- max(edge_betweenness(g))
    if (max_edge_btw > max_values$edge_betweenness) max_values$edge_betweenness <- max_edge_btw
  }
  
  return(max_values)
}

# Get global maximums before creating plots
periods <- unique(edges_all$Period)
global_max <- calculate_global_maximums(edges_all, nodes, periods)

# Enhanced function to analyze network with centrality metrics
analyze_network <- function(edge_df, node_df, period_label, global_max) {
  g <- graph_from_data_frame(
    d = edge_df[, c("Source", "Target")],
    vertices = node_df,
    directed = FALSE
  )
  
  # Calculate vertex degree and betweenness
  V(g)$degree <- degree(g)
  V(g)$betweenness <- betweenness(g)
  
  # Calculate edge betweenness
  E(g)$betweenness <- edge_betweenness(g)
  
  # Prepare node and edge data frames with centrality metrics
  node_data <- data.frame(
    id = V(g)$name,
    label = V(g)$label,
    degree = V(g)$degree,
    betweenness = V(g)$betweenness
  )
  edge_data <- as_data_frame(g, what = "edges")
  edge_data$betweenness <- E(g)$betweenness
  
  # Calculate network-level centralization metrics
  n_nodes <- vcount(g)
  n_edges <- ecount(g)
  dens <- edge_density(g)
  avg_deg <- mean(V(g)$degree)
  
  # Degree and betweenness centralization
  deg_centralization <- centralization.degree(g)$centralization * 100
  btw_centralization <- centr_betw(g)$centralization * 100
  
  metrics <- data.frame(
    Period = period_label,
    Network_Size = n_nodes,
    Number_of_Ties = n_edges,
    Density = dens,
    Average_Degree = avg_deg,
    Degree_Centralization = deg_centralization,
    Betweenness_Centralization = btw_centralization
  )
  
  # Create plot with consistent scales across all periods
  set.seed(123)
  p <- ggraph(g, layout = "kk") +
    geom_edge_link(aes(edge_width = betweenness), color = "black", alpha = 0.7) +
    geom_node_point(aes(size = betweenness, color = degree), alpha = 0.8) +
    geom_node_text(aes(label = label), repel = TRUE, size = 4.5) +
    # Use global maximums to set consistent limits
    scale_size_continuous(range = c(2, 10), name = "betweenness",
                          limits = c(0, global_max$node_betweenness)) +
    scale_color_gradient(low = "#DEEBF7", high = "#08519C", name = "degree",
                         limits = c(0, global_max$degree)) +
    scale_edge_width_continuous(range = c(0.2, 2), guide = "legend",
                                limits = c(0, global_max$edge_betweenness)) +
    theme_void() +
    ggtitle(paste("Mentioning network:", period_label)) +
    theme(legend.position = "bottom")
  
  return(list(metrics = metrics, node_data = node_data, edge_data = edge_data, plot = p))
}

# Analyze networks for each period + overall with consistent scales
results <- list()
for (period in periods) {
  edges_sub <- edges_all %>% filter(Period == period)
  results[[period]] <- analyze_network(edges_sub, nodes, period, global_max)
}

combined_plot <- wrap_plots(lapply(results, function(x) x$plot), ncol = 2) +
  plot_annotation(title = "co-author network (2013-2024)",
                  subtitle = "Node size represents betweenness centrality, color shows degree centrality")

print(combined_plot)



#####Inter organization

# Load necessary libraries
library(readxl)
library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(writexl)
library(lubridate)
library(patchwork)

# 1. Load edge data
edges <- read_excel("C:/Users/wk2lam/Downloads/Mar 28_organization.xlsx", sheet = "All")
colnames(edges) <- c("Date", "Source", "Target")
edges$Date <- as.Date(edges$Date)

# 2. Load node list
node_list <- read_excel("C:/Users/wk2lam/Downloads/Feb 26_node list.xlsx", sheet = "Sheet1")
node_list$id <- as.character(node_list$id)

# 3. Assign periods to edges
edges <- edges %>%
  mutate(
    Period = case_when(
      Date >= as.Date("2013-01-01") & Date <= as.Date("2017-12-31") ~ "2013-2017",
      Date >= as.Date("2018-01-01") & Date <= as.Date("2019-12-31") ~ "2018-2019",
      Date >= as.Date("2020-01-01") & Date <= as.Date("2024-12-31") ~ "2020-2024",
      TRUE ~ NA_character_
    ),
    Source = as.character(Source),
    Target = as.character(Target)
  )

edges_periods <- edges %>% filter(!is.na(Period))
edges_overall <- edges_periods %>% mutate(Period = "Overall 2013-2024")
edges_all <- bind_rows(edges_periods, edges_overall)

# 4. Build node list from all unique nodes in combined edges, join with node_list for labels
all_nodes <- unique(c(edges_all$Source, edges_all$Target))
nodes <- data.frame(id = as.character(all_nodes)) %>%
  left_join(node_list, by = "id")

# Calculate global maximum values
calculate_global_maximums <- function(edges_all, nodes, periods) {
  max_values <- list(degree = 0, node_betweenness = 0, edge_betweenness = 0)
  
  for (period in periods) {
    edges_sub <- edges_all %>% filter(Period == period)
    g <- graph_from_data_frame(
      d = edges_sub[, c("Source", "Target")],
      vertices = nodes,
      directed = FALSE
    )
    
    # Update max degree if needed
    max_degree <- max(degree(g))
    if (max_degree > max_values$degree) max_values$degree <- max_degree
    
    # Update max node betweenness if needed
    max_node_btw <- max(betweenness(g))
    if (max_node_btw > max_values$node_betweenness) max_values$node_betweenness <- max_node_btw
    
    # Update max edge betweenness if needed
    max_edge_btw <- max(edge_betweenness(g))
    if (max_edge_btw > max_values$edge_betweenness) max_values$edge_betweenness <- max_edge_btw
  }
  
  return(max_values)
}

# Get global maximums before creating plots
periods <- unique(edges_all$Period)
global_max <- calculate_global_maximums(edges_all, nodes, periods)

# Enhanced function to analyze network with centrality metrics
analyze_network <- function(edge_df, node_df, period_label, global_max) {
  g <- graph_from_data_frame(
    d = edge_df[, c("Source", "Target")],
    vertices = node_df,
    directed = FALSE
  )
  
  # Calculate vertex degree and betweenness
  V(g)$degree <- degree(g)
  V(g)$betweenness <- betweenness(g)
  
  # Calculate edge betweenness
  E(g)$betweenness <- edge_betweenness(g)
  
  # Prepare node and edge data frames with centrality metrics
  node_data <- data.frame(
    id = V(g)$name,
    label = V(g)$label,
    degree = V(g)$degree,
    betweenness = V(g)$betweenness
  )
  edge_data <- as_data_frame(g, what = "edges")
  edge_data$betweenness <- E(g)$betweenness
  
  # Calculate network-level centralization metrics
  n_nodes <- vcount(g)
  n_edges <- ecount(g)
  dens <- edge_density(g)
  avg_deg <- mean(V(g)$degree)
  
  # Degree and betweenness centralization
  deg_centralization <- centralization.degree(g)$centralization * 100
  btw_centralization <- centr_betw(g)$centralization * 100
  
  metrics <- data.frame(
    Period = period_label,
    Network_Size = n_nodes,
    Number_of_Ties = n_edges,
    Density = dens,
    Average_Degree = avg_deg,
    Degree_Centralization = deg_centralization,
    Betweenness_Centralization = btw_centralization
  )
  
  # Create plot with consistent scales across all periods
  set.seed(123)
  p <- ggraph(g, layout = "kk") +
    geom_edge_link(aes(edge_width = betweenness), color = "black", alpha = 0.7) +
    geom_node_point(aes(size = betweenness, color = degree), alpha = 0.8) +
    geom_node_text(aes(label = label), repel = TRUE, size = 4.5) +
    # Use global maximums to set consistent limits
    scale_size_continuous(range = c(2, 10), name = "betweenness",
                          limits = c(0, global_max$node_betweenness)) +
    scale_color_gradient(low = "#DEEBF7", high = "#08519C", name = "degree",
                         limits = c(0, global_max$degree)) +
    scale_edge_width_continuous(range = c(0.2, 2), guide = "legend",
                                limits = c(0, global_max$edge_betweenness)) +
    theme_void() +
    ggtitle(paste("Mentioning network:", period_label)) +
    theme(legend.position = "bottom")
  
  return(list(metrics = metrics, node_data = node_data, edge_data = edge_data, plot = p))
}

# Analyze networks for each period + overall with consistent scales
results <- list()
for (period in periods) {
  edges_sub <- edges_all %>% filter(Period == period)
  results[[period]] <- analyze_network(edges_sub, nodes, period, global_max)
}

combined_plot <- wrap_plots(lapply(results, function(x) x$plot), ncol = 2) +
  plot_annotation(title = "Interorganization network (2013-2024)",
                  subtitle = "Node size represents betweenness centrality, color shows degree centrality")

print(combined_plot)

