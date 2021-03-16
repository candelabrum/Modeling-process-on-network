library(sand)

update_influenced <- function(g, old_infl)
{
    inf_distr <- rep(0, 100)
    
    for (i in 1:100)
        inf_distr[i] <- 0
    new_influenced <- old_infl;
    for (i in 1:100)
    {
        sum = 0;
        adj_v <- adjacent_vertices(g, i)
        adj_vector <- adj_v[[1]]
        #for (v in adj_vector)
        #{
        #    sum  = sum + old_infl[v]
        #}
        sum = old_infl[i]
        tmp <- inf_distr[100*degree_distribution(g)[i] + 1] + sum
        inf_distr[100*degree_distribution(g)[i] + 1] <- tmp
    }
    max_degree <- length(degree_distribution(g))
    print(inf_distr)
    for(i in 1:max_degree)
    {
        print("Degree")
        print(degree_distribution(g)[i])
        if (degree_distribution(g)[i] != 0)
            print(inf_distr[i]/(100*degree_distribution(g)[i]))
    }
}

set.seed(42)
g.er <- erdos.renyi.game(100, 0.10)

X11();
influenced <- rbinom(100, 1, 0.20);
g.er$influenced <- influenced
g.er.colors <- as.character(length(influenced));
g.er.colors[influenced == 1] <- "green";
g.er.colors[influenced == 0] <- "deepskyblue";
update_influenced(g.er, influenced)
plot(g.er, layout=layout.fruchterman.reingold, vertex.color=g.er.colors)
print(influenced)

Sys.sleep(100);
#write update influenced and recalculate smth properties of graph
