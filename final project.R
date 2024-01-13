library(igraph)

folder_path <- "C:\\Users\\Sherif Tarfa\\Desktop\\gplus"

file_list <- list.files(path = folder_path, full.names = TRUE)

node_count <- 0

for (file in file_list) {
  file_extension <- tools::file_ext(file)
  
  if (file_extension == "edges") {
    edges_data <- read.table(file)
    graph <- graph_from_data_frame(edges_data, directed = FALSE)
    node_count <- node_count + vcount(graph)
    plot(graph, edge.arrow.size = 0.2, vertex.size = 2, vertex.label = NA)
  }
}

# Print the total number of nodes
cat("Total number of nodes:", node_count, "\n")
################################

# Create an empty list to store the top nodes and degrees
top_nodes <- c()
top_degrees <- c()

# Loop over each file in the file_list
for (file in file_list) {
  file_extension <- tools::file_ext(file)
  
  # Read and analyze based on file extension
  if (file_extension == "edges") {
    # Read the .edges file
    edges_data <- read.table(file)
    
    # Convert the edges data frame to a graph object
    graph <- graph_from_data_frame(edges_data, directed = FALSE)
    
    # Calculate the degree for each vertex in the graph
    degrees <- degree(graph)
    
    # Get the indices of the top 20 degrees
    top_indices <- order(degrees, decreasing = TRUE)[1:20]
    
    # Retrieve the nodes and degrees for the top indices
    top_nodes <- c(top_nodes, V(graph)[top_indices])
    top_degrees <- c(top_degrees, degrees[top_indices])
  }
}

# Print the first top 20 nodes and their degrees
cat("Top 20 degrees:\n")
for (i in 1:20) {
  cat("Node", top_nodes[i], ": Degree", top_degrees[i], "\n")
}
  
##########################
total_degrees <- 0
num_nodes <- 0

# Loop over each file in the file_list
for (file in file_list) {
  file_extension <- tools::file_ext(file)
  
  # Read and analyze based on file extension
  if (file_extension == "edges") {
    # Read the .edges file
    edges_data <- read.table(file)
    
    # Convert the edges data frame to a graph object
    graph <- graph_from_data_frame(edges_data, directed = FALSE)
    
    # Calculate the degree for each vertex in the graph
    degrees <- degree(graph)
    
    # Update total degrees and number of nodes
    total_degrees <- total_degrees + sum(degrees)
    num_nodes <- num_nodes + vcount(graph)
  }
}

# Calculate average degree
average_degree <- total_degrees / num_nodes

# Print the average degree
cat("Average Degree:", average_degree, "\n")
#####################


# Iterate over the files
for (file in file_list) {
  file_extension <- tools::file_ext(file)
  
  # Read and analyze based on file extension
  if (file_extension == "edges") {
    # Read the .edges file
    edges_data <- read.table(file)
    
    # Convert the edges data frame to a graph object
    graph <- graph_from_data_frame(edges_data, directed = FALSE)
    
    # Calculate the connectivity of the graph
    is_connected <- is.connected(graph)
    
    # Print the connectivity result
    cat("Connectivity:", is_connected, "\n")
  }
}
##########################
for (file in file_list) {
  file_extension <- tools::file_ext(file)
  
  # Read and analyze based on file extension
  if (file_extension == "edges") {
    # Read the .edges file
    edges_data <- read.table(file)
    
    # Convert the edges data frame to a graph object
    graph <- graph_from_data_frame(edges_data, directed = FALSE)
    
    # Calculate the betweenness centrality for each node
    betweenness_values <- rbind(betweenness_values, 
                                data.frame(Node = 1:vcount(graph),
                                           Betweenness = betweenness(graph)))
  }
}

# Order the betweenness values in descending order
top_20_betweenness <- head(betweenness_values[order(-betweenness_values$Betweenness), ], 20)

# Print the top 20 betweenness centrality values
cat("Top 20 betweenness centrality values:\n")
print(top_20_betweenness)
##########################
# Create an empty list to store the top nodes and their closeness centrality
top_nodes <- c()
closeness <- c()

# Loop over each file in the file_list
for (file in file_list) {
  file_extension <- tools::file_ext(file)
  
  # Read and analyze based on file extension
  if (file_extension == "edges") {
    # Read the .edges file
    edges_data <- read.table(file)
    
    # Convert the edges data frame to a graph object
    graph <- graph_from_data_frame(edges_data, directed = FALSE)
    
    # Calculate the closeness centrality for each vertex in the graph
    closeness_centrality <- closeness(graph)
    
    # Get the indices of the top 20 closeness centrality
    top_indices <- order(closeness_centrality, decreasing = TRUE)[1:20]
    
    # Retrieve the nodes and closeness centrality for the top indices
    top_nodes <- c(top_nodes, V(graph)[top_indices])
    closeness <- c(closeness, closeness_centrality[top_indices])
  }
}

# Print the top 20 nodes and their closeness centrality
cat("Top 20 nodes and their closeness centrality:\n")
for (i in 1:20) {
  cat("Node:", top_nodes[i], "// Closeness Centrality:", closeness[i], "\n")
}