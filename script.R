library(igraph)
library(visNetwork)
library(dplyr)
library(ggiraph)
library(ggraph)
library(ggplot2)

data<-read.csv('asoiaf-all-edges.csv')
data<-data[,c(1,2,5)]

#nodes<-read.csv('asoiaf-all-nodes.csv')

#Creating indirected graph
#g<-graph_from_data_frame(data,vertices = nodes,directed = F)
g<-graph_from_data_frame(data,directed = F)

#The number of vertices of the graph
vcount(g)

#The list of vertices of the graph
V(g)

#The number of edges of the graph
ecount(g)

#The list of edges of the graph
E(g)

#The diameter of the graph
diameter(g)

#The number of triangles in the graph
length(triangles(g))/3 

#Number of edges with weight more than 12
sum(E(g)$weight > 12)

#Top-10 characters by degree
head(sort(degree(g), decreasing = TRUE), 10)

#Top-10 characters by weighted degree
head(sort(strength(g,weights = E(g)$weight), decreasing = TRUE), 10)

#Top-10 characters by local clustering coefficient
head(sort(transitivity(g, type = "local"), decreasing = TRUE), 10)

#The global clustering coefficient of the graph
transitivity(g,type = 'global')

#Plotting

#col  <- c("aquamarine3","blue","deeppink","goldenrod3","darkorchid2","yellow","red","skyblue1","darkgreen","black","white")
my_color<-'lightblue'
#my_color <- col[as.numeric(as.factor(V(g)$Group))]

set.seed(23)
plot(
  g,
  vertex.label = NA,              # No labels on vertices
  edge.width = E(g)$weight/12, 
  vertex.color='lightblue',# Edge width based on weights
  vertex.size = 2,               # Size of vertices
  edge.color = "gray",           # Edge color      # Vertex color
  vertex.frame.color = "black",   # Color of vertex borders
  edge.curved = 0.1,              # Slightly curve the edges
  layout = layout_with_fr,        
  main = "‘A Song of Ice and Fire’ network Plot"
)

#legend(-2,0.8,legend = levels(as.factor(V(g)$Group)),cex = 0.6,fill = col,bty = "n",title.adj = 0,title = "House of Character")

sub_nodes<-V(g)[degree(g) >= 9]

# Create the subgraph
subgraph <- induced_subgraph(g, sub_nodes)

set.seed(23)
plot(
  subgraph,
  vertex.label=NA,
  edge.width = E(subgraph)$weight/20,
  vertex.color='lightblue',
  vertex.size = 3,              
  edge.color = "gray",           
  vertex.frame.color = "black",
  vertex.color=my_color[sub_nodes],
  edge.curved = 0.1,              
  layout = layout_with_fr,       
  main = "‘A Song of Ice and Fire’ network Plot \nfor characters with 9 or more connections"
)

#legend(-2,0.8,legend = levels(as.factor(V(g)$Group)),cex = 0.6,fill = col,bty = "n",title.adj = 0,title = "House of Character")

edge_density(g)
edge_density(subgraph)

head(sort(closeness(g, normalized = TRUE),decreasing = T),15)

head(sort(betweenness(g, normalized = TRUE),decreasing = T),15)

page_rank<-page_rank(g, weights = E(g)$weight)$vector

set.seed(23)
plot(
  g,
  vertex.label = NA,            
  edge.width = E(g)$weight/20, 
  vertex.color='lightblue',
  vertex.size = page_rank*200,              
  edge.color = "gray",           
  vertex.frame.color = "black",   
  edge.curved = 0.1,              
  layout = layout_with_fr,        
  main = "‘A Song of Ice and Fire’ network Plot"
)

#legend(-2,0.8,legend = levels(as.factor(V(g)$Group)),cex = 0.6,fill = col,bty = "n",title.adj = 0,title = "House of Character")

#############################

# Assuming g is your igraph object and page_rank is already calculated
page_rank <- page_rank(g)$vector  # Ensure page_rank is a numeric vector of the page ranks
page_rank <- page_rank[sub_nodes]

# Create a data frame of the edge attributes
edges <- get.data.frame(subgraph, what = "edges")

# Convert igraph object to a data frame for nodes
nodes <- data.frame(name = V(subgraph)$name, page_rank = page_rank)

set.seed(23)
# Create the ggraph plot
p <- ggraph(subgraph, layout = 'fr') + 
  geom_edge_link(aes(width = weight/10), color = "black", alpha = 0.9) +
  geom_point_interactive(aes(x = x, y = y, size = page_rank/400, tooltip = name, data_id = name), color = 'black', fill = 'lightblue', shape = 21, stroke = 0.5) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("‘A Song of Ice and Fire’ network Plot") +
  scale_size_continuous(range = c(0.5, 10)) +
  scale_edge_width_continuous(range = c(0.1, 2))

# Convert to an interactive plot using ggiraph
girafe(ggobj = p)
