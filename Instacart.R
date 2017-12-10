
#inputting the main order-products dataset
order_products_prior = read.csv(choose(),sep = ',')

#calculating the product frequency
prod_freq = as.data.frame(table(order_products_prior$product_id))
mean(prod_freq$Freq)

#calculating the order size
order_freq = as.data.frame(table(order_products_prior$order_id))
mean(order_freq$Freq)

#5-point summaries for both orders and products frequencies
summary(order_freq$Freq)
summary(prod_freq$Freq)

#taking products freq between mediun and 3rd quarter
p_mean = subset(prod_freq, prod_freq$Freq>59 & prod_freq$Freq<261)
colnames(p_mean)[1]="product_id"

#Network 1 = low order size = 2 to 5
o_mean = subset(order_freq, order_freq$Freq>1 & order_freq$Freq<6)

#Network 2 = 1st q order size = 5
o_mean = subset(order_freq, order_freq$Freq==5)

#Network 3 = 1st to med order size = 5 to 8
o_mean = subset(order_freq, order_freq$Freq>4 & order_freq$Freq<8)

#Network 4 = mean order size = 10
o_mean = subset(order_freq, order_freq$Freq==10)

#Network 5 = med to mean order size = 8 to 10
o_mean = subset(order_freq, order_freq$Freq>7 & order_freq$Freq<11)

#Network 6 = higher than median order size = 10 to 14
o_mean = subset(order_freq, order_freq$Freq>9 & order_freq$Freq<15)

#Network 7 = 3rd q order size = 14
o_mean = subset(order_freq, order_freq$Freq==14)

colnames(o_mean)[1]="order_id"

op = merge(order_products_prior, p_mean, by="product_id")
op2 = merge(op,o_mean,by="order_id")
op3 = merge(order_products_prior,op2,by="order_id")
op4 = op3[,-c(3:9)]
colnames(op4)[2]="product_id"

library(data.table)
lst = by(op4$product_id, op4$order_id, FUN = combn, m= 2)

#getting combinations of products within an order
p_comb = data.frame(group = rep(unique(as.character(op4$order_id)), sapply(lst, ncol)), t(do.call(cbind, lst)))

#p_comb = as.data.frame(t(combn(op3$product_id.x,2)))
colnames(p_comb)[1] = "order_id"
colnames(p_comb)[2] = "Source"
colnames(p_comb)[3] = "Target"

#removing rows with same source and targets
p_comb = p_comb[p_comb$Source!=p_comb$Target,]

#removing the same combinations
p_comb$comb = paste(p_comb$Source,p_comb$Target,sep = "-")
p_comb$comb2 = paste(p_comb$Target,p_comb$Source,sep = "-")

p_comb12 = rbind(p_comb$comb,p_comb$comb2)

pcomb_freq = table(p_comb12)
max(pcomb_freq)
pcomb_freq = as.data.frame(pcomb_freq)

names(pcomb_freq)[1]="comb"
pcomb_final = unique(merge(p_comb[2:4],subset(pcomb_freq,Freq>29),by="comb"))[2:4]
names(pcomb_final)[3]="weight"

library(igraph)

plot(p_graph)
p_graph = graph.data.frame(pcomb_final, directed = F)
is.simple(p_graph)
p_graph = simplify(p_graph, remove.multiple =T) 
V(p_graph)
E(p_graph)

transitivity(p_graph,type = "global")
edge_density(p_graph)

ebc <- edge.betweenness.community(p_graph, directed=F)
modularity(ebc)
  # Now we have the merges/splits and we need to calculate the modularity
# for each merge for this we'll use a function that for each edge
# removed will create a second graph, check for its membership and use
# that membership to calculate the modularity
mods <- sapply(0:ecount(p_graph), function(i){
  g2 <- delete.edges(p_graph, ebc$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership
  # March 13, 2014 - compute modularity on the original graph g 
  # (Thank you to Augustin Luna for detecting this typo) and not on the induced one g2. 
  modularity(p_graph,cl)
})

# we can now plot all modularities
plot(mods, pch=20)

# Now, let's color the nodes according to their membership
g2<-delete.edges(p_graph, ebc$removed.edges[seq(length=which.max(mods)-1)])
V(p_graph)$color=clusters(g2)$membership

p_out = as.data.frame(clusters(g2)$membership)
p_out[2] = V(g2)$name

p_out_freq = as.data.frame(table(p_out$`clusters(g2)$membership`))
table(p_out_freq$Freq)

# Let's choose a layout for the graph
p_graph$layout <- layout.fruchterman.reingold

p_names = read.csv("C:/Users/Amit Bhalerao/Documents/CareerCenter/Kaggle Projects/Instacart/products_all.csv")

names(p_out)[1]="Cluster number"
names(p_out)[2]="product_id"
p_out2 = merge(p_out,p_names, by="product_id")[1:3]

# plot it
plot(p_graph, vertex.label=NA)
