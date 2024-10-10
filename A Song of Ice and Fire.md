# ‘A Song of Ice and Fire’ Network Analysis and Visualization with R

# Introduction

“A Song of Ice and Fire” is set in a fictional world with unpredictable,
years-long seasons. Three centuries prior, the Targaryen dynasty united
the Seven Kingdoms of Westeros using dragons to establish their
dominance. Their reign ended with Robert Baratheon’s rebellion, which
killed the last Targaryen king, Aerys II, and saw Robert crowned king.

The series begins 15 years later, as the struggle for the Iron Throne
ensues following King Robert’s death. His heir, Joffrey, is proclaimed
king, but Lord Eddard Stark discovers Joffrey’s illegitimacy and is
executed for challenging his claim. This ignites the War of the Five
Kings, with factions vying for control and two regions seeking
independence.

In the far north, the Night’s Watch guards a massive ice wall against
supernatural Others and wildlings, with Jon Snow, Eddard’s bastard son,
rising to lead them. Meanwhile, Daenerys Targaryen, the exiled daughter
of Aerys II, gains power in Essos by hatching dragons and conquering
Slaver’s Bay, aiming to reclaim her ancestral throne in Westeros.

- **Summary of A Song of Ice and Fire Novels**

| \# | Book Title | Pages | Chapters | Words | Audio | US release |
|----|----|----|----|----|----|----|
| 1 | A Game of Thrones | 694 | 73 | 292,727 | 33 h 53 min | August 1996 |
| 2 | A Clash of Kings | 768 | 70 | 318,903 | 37 h 17 min | February 1999 |
| 3 | A Storm of Swords | 973 | 82 | 414,604 | 47 h 37 min | November 2000 |
| 4 | A Feast for Crows | 753 | 46 | 295,032 | 31 h 10 min | November 2005 |
| 5 | A Dance with Dragons | 1056 | 73 | 414,788 | 48 h 56 min | July 2011 |
| 6 | The Winds of Winter | Forthcoming | \- | \- | \- | \- |
| 7 | A Dream of Spring | Forthcoming | \- | \- | \- | \- |
|  | **Total** | 4,244 | 344 | 1,736,054 | 198 h 53 min | 1996–present |

- **Number of Chapters per Point-of-View Character in A Song of Ice and
  Fire Series**

| POV Character | Game | Clash | Storm | Feast | Dance | (Winds) | Sum |
|----|----|----|----|----|----|----|----|
| Bran Stark | 7 | 7 | 4 | 3 | \- | \- | 21 |
| Catelyn Stark | 11 | 7 | 7 | \- | \- | \- | 25 |
| Daenerys Targaryen | 10 | 5 | 6 | 10 | \- | \- | 31 |
| Eddard Stark | 15 | \- | \- | \- | \- | \- | 15 |
| Jon Snow | 9 | 8 | 12 | 13 | \- | \- | 42 |
| Arya Stark | 5 | 10 | 13 | 3 | 2 | ≥1\[57\] | ≥34 |
| Tyrion Lannister | 9 | 15 | 11 | \- | 12 | ≥2\[103\] | ≥49 |
| Sansa Stark | 6 | 8 | 7 | 3 | \- | ≥1\[57\] | ≥25 |
| Davos Seaworth | \- | 3 | 6 | \- | 4 | \- | 13 |
| Theon Greyjoy | \- | 6 | \- | 7 | \- | ≥1\[62\] | ≥14 |
| Jaime Lannister | \- | \- | 9 | 7 | 1 | \- | 17 |
| Samwell Tarly | \- | \- | 5 | 5 | \- | \- | 10 |
| Aeron Greyjoy | \- | \- | 2 | \- | ≥1\[59\] | \- | ≥3 |
| Areo Hotah | \- | 1 | 1 | \- | ≥1\[104\] | \- | ≥3 |
| Cersei Lannister | \- | \- | 10 | 2 | \- | \- | 12 |
| Brienne of Tarth | \- | \- | 8 | \- | \- | \- | 8 |
| Asha Greyjoy | \- | \- | 1 | 3 | \- | \- | 4 |
| Arys Oakheart | \- | \- | 1 | \- | \- | \- | 1 |
| Victarion Greyjoy | \- | \- | 2 | 2 | \- | ≥1\[103\] | ≥5 |
| Arianne Martell | \- | \- | 2 | \- | ≥2\[57\] | \- | ≥4 |
| Quentyn Martell | \- | \- | \- | 4 | \- | \- | 4 |
| Jon Connington | \- | \- | \- | 2 | \- | \- | 2 |
| Melisandre | \- | \- | \- | 1 | \- | \- | 1 |
| Barristan Selmy | \- | \- | \- | 4 | \- | ≥2\[105\]\[106\] | ≥6 |
| Prologue/Epilogue | 1/– | 1/– | 1/1 | 1/– | 1/1 | 1/TBD | ≥8 |
| **Total (characters)** | 73 (9) | 70 (10) | 82 (12) | 46 (13) | 73 (18) | ≥13 (≥9) | ≥357 (≥24) |

This table illustrates the number of chapters each point-of-view
character has in each book of the series, along with the total chapters
and character count.

![The Iron Throne: the throne of the monarch of the fictional Seven
Kingdoms of
Westeros](https://images3.alphacoders.com/144/thumb-1920-144539.jpg)

# ‘A Song of Ice and Fire’ network

Let’s dive into the character network and analyze the existing
connections, using R and igraph. \## Importing Libraries

``` r
library(igraph)
library(visNetwork)
library(dplyr)
library(ggiraph)
library(ggraph)
library(ggplot2)
```

## Importing data

``` r
data<-read.csv('asoiaf-all-edges.csv')
data<-data[,c(1,2,5)]
```

## Creating an undirected weighted graph

``` r
g<-graph_from_data_frame(data,directed = F)
```

# Network Properties

## The number of vertices of the graph

``` r
vcount(g)
```

    [1] 796

## The list of vertices of the graph

``` r
V(g)
```

    + 796/796 vertices, named, from 2e8f8db:
      [1] Addam-Marbrand                         
      [2] Aegon-Frey-(son-of-Stevron)            
      [3] Aegon-I-Targaryen                      
      [4] Aegon-Targaryen-(son-of-Rhaegar)       
      [5] Aegon-V-Targaryen                      
      [6] Aemon-Targaryen-(Dragonknight)         
      [7] Aemon-Targaryen-(Maester-Aemon)        
      [8] Aenys-Frey                             
      [9] Aeron-Greyjoy                          
     [10] Aerys-I-Targaryen                      
    + ... omitted several vertices

## The number of edges of the graph

``` r
ecount(g)
```

    [1] 2823

## The list of edges of the graph

``` r
E(g)
```

    + 2823/2823 edges from 2e8f8db (vertex names):
     [1] Addam-Marbrand             --Brynden-Tully    
     [2] Addam-Marbrand             --Cersei-Lannister 
     [3] Addam-Marbrand             --Gyles-Rosby      
     [4] Addam-Marbrand             --Jaime-Lannister  
     [5] Addam-Marbrand             --Jalabhar-Xho     
     [6] Addam-Marbrand             --Joffrey-Baratheon
     [7] Addam-Marbrand             --Kevan-Lannister  
     [8] Addam-Marbrand             --Lyle-Crakehall   
     [9] Addam-Marbrand             --Oberyn-Martell   
    [10] Addam-Marbrand             --Tyrion-Lannister 
    + ... omitted several edges

## The diameter of the graph

``` r
diameter(g)
```

    [1] 53

## The number of triangles in the graph

``` r
length(triangles(g))/3 
```

    [1] 5655

## Number of edges with weight more than 12

``` r
sum(E(g)$weight > 12)
```

    [1] 610

## Top-10 characters by degree

``` r
head(sort(degree(g), decreasing = TRUE), 10)
```

     Tyrion-Lannister          Jon-Snow   Jaime-Lannister  Cersei-Lannister 
                  122               114               101                97 
    Stannis-Baratheon        Arya-Stark     Catelyn-Stark       Sansa-Stark 
                   89                84                75                75 
         Eddard-Stark        Robb-Stark 
                   74                74 

## Top-10 characters by weighted degree

``` r
head(sort(strength(g,weights = E(g)$weight), decreasing = TRUE), 10)
```

      Tyrion-Lannister           Jon-Snow   Cersei-Lannister  Joffrey-Baratheon 
                  2873               2757               2232               1762 
          Eddard-Stark Daenerys-Targaryen    Jaime-Lannister        Sansa-Stark 
                  1649               1608               1569               1547 
            Bran-Stark   Robert-Baratheon 
                  1508               1488 

## Top-10 characters by local clustering coefficient

``` r
head(sort(transitivity(g, type = "local"), decreasing = TRUE), 10)
```

    Aegon-Frey-(son-of-Stevron)                      Albett 
                              1                           1 
               Alerie-Hightower                  Allar-Deem 
                              1                           1 
                  Alys-Karstark             Alysane-Mormont 
                              1                           1 
                         Amabel                       Arron 
                              1                           1 
               Baelor-Blacktyde          Baelor-I-Targaryen 
                              1                           1 

## The global clustering coefficient of the graph

``` r
transitivity(g,type = 'global')
```

    [1] 0.2090367

# Subgraph

## The entire network

``` r
set.seed(23)
plot( 
  g,
  vertex.label = NA,              
  edge.width = E(g)$weight/100, 
  vertex.color='lightblue',
  vertex.size = 1,               
  edge.color = "black",           
  vertex.frame.color = "black",   
  edge.curved = 0.1,              
  layout = layout_with_fr,        
  main = "‘A Song of Ice and Fire’ network Plot"
)
```

![](project1_files/figure-commonmark/unnamed-chunk-15-1.svg)

## Vertices that have 9 or more connections

- Selecting the vertices with 9 or more connections

``` r
sub_nodes<-V(g)[degree(g) >= 9]
```

- Creating a sub-graph of the selected vertices

``` r
subgraph <- induced_subgraph(g, sub_nodes)
```

- Plotting the sub-graph

``` r
set.seed(23)
plot(
  subgraph,
  vertex.label=NA,
  edge.width = E(subgraph)$weight/80,
  vertex.color='lightblue',
  vertex.size = 2,              
  edge.color = "black",           
  vertex.frame.color = "black",
  edge.curved = 0.1,              
  layout = layout_with_fr,       
  main = "‘A Song of Ice and Fire’ network Plot \nfor characters with 9 or more connections"
)
```

![](project1_files/figure-commonmark/unnamed-chunk-18-1.svg)

## Edge density

- Edge density of the entire graph

``` r
edge_density(g)
```

    [1] 0.008921968

- Edge density of the sub-graph

``` r
edge_density(subgraph)
```

    [1] 0.09494239

# Centrality

## Closeness Centrality

``` r
head(sort(closeness(g, normalized = TRUE),decreasing = T),15)
```

      Jaime-Lannister  Robert-Baratheon Stannis-Baratheon     Theon-Greyjoy 
           0.09587554        0.09244186        0.09118018        0.09111748 
          Jory-Cassel   Tywin-Lannister  Tyrion-Lannister  Cersei-Lannister 
           0.09075342        0.09044369        0.08984066        0.08981021 
     Brienne-of-Tarth          Jon-Snow Joffrey-Baratheon     Rodrik-Cassel 
           0.08939615        0.08895603        0.08785501        0.08773866 
         Eddard-Stark     Doran-Martell        Robb-Stark 
           0.08682831        0.08654474        0.08653532 

- Jon Snow’s relatively high rank in the closeness scores suggests that
  he has strong connections with other characters within the narrative.
  This is consistent with Jon Snow’s prominent role in the “A Song of
  Ice and Fire” series as one of the central protagonists.

There are several factors that could contribute to Jon Snow’s high
closeness score:

1.  **Relationships**: Jon Snow interacts with a wide range of
    characters throughout the series, including members of his own
    family (such as Eddard Stark and his siblings), his sworn brothers
    in the Night’s Watch, key political figures (such as Stannis
    Baratheon), and other major players in the series (such as Tyrion
    Lannister and Daenerys Targaryen). These interactions create
    connections and relationships that contribute to his overall
    closeness score.

2.  **Story Arcs**: Jon Snow’s storyline intersects with many major
    plotlines in the series, particularly those involving the Night’s
    Watch, the Wall, and the threat of the White Walkers. His actions
    and decisions often have far-reaching consequences that impact other
    characters and events in the story, further increasing his closeness
    to various elements of the narrative.

## Betweeness Centrality

``` r
head(sort(betweenness(g, normalized = TRUE),decreasing = T),15)
```

              Jon-Snow      Theon-Greyjoy    Jaime-Lannister Daenerys-Targaryen 
            0.13211964         0.12326573         0.11677631         0.09419228 
     Stannis-Baratheon   Robert-Baratheon   Tyrion-Lannister   Cersei-Lannister 
            0.09291440         0.09252286         0.09162375         0.07734002 
       Tywin-Lannister         Robb-Stark         Arya-Stark    Barristan-Selmy 
            0.06358359         0.06295788         0.06132325         0.05630053 
          Eddard-Stark        Sansa-Stark   Brienne-of-Tarth 
            0.05562270         0.05042041         0.04947298 

- Jon Snow has the highest betweenness centrality score, indicating that
  he often serves as a central figure who connects various characters
  within the story. This aligns with Jon’s role as a central protagonist
  who interacts with a wide range of characters from different factions
  and storylines.

![Jon Snow: a prominent point of view character in the novels, and has
been called one of Martin’s “finest
creations”](https://metro.co.uk/wp-content/uploads/2019/04/SEI_601281802.jpg?quality=90&strip=all&zoom=1&resize=644%2C428)

# Ranking and Visualization

- PageRank values

``` r
page_rank<-page_rank(g, weights = E(g)$weight)$vector
```

- Network Plot with PageRank

``` r
set.seed(23)
plot(
  g,
  vertex.label = NA,            
  edge.width = E(g)$weight/100, 
  vertex.color='lightblue',
  vertex.size = page_rank*100,              
  edge.color = "black",           
  vertex.frame.color = "black",   
  edge.curved = 0.1,              
  layout = layout_with_fr,        
  main = "‘A Song of Ice and Fire’ network Plot",
  sub="Size = PageRank"
)
```

![](project1_files/figure-commonmark/unnamed-chunk-24-1.svg)

- Subgraph Plot with PageRank

``` r
set.seed(23)
plot(
  subgraph,
  vertex.label = NA,            
  edge.width = E(subgraph)$weight/80, 
  vertex.color='lightblue',
  vertex.size = page_rank[sub_nodes]*150,              
  edge.color = "black",           
  vertex.frame.color = "black",   
  edge.curved = 0.1,              
  layout = layout_with_fr,        
  main = "‘A Song of Ice and Fire’ network Plot \nfor characters with 9 or more connections",
  sub="Size = PageRank"
)
```

![](project1_files/figure-commonmark/unnamed-chunk-25-1.svg)
