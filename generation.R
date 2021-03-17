library(sand)

get_cost_seq <- function(k)
{
    if (k < 200 && k > 500)
        return (1);
    return (10);
} 

change_state <- function(A_k_m, degree, cost_seq_vert, state)
{
    if (state == 1)
        return (cost_seq_vert <= A_k_m)

    return rbinom(1, 1, 0.3);
}

get_A_k_m <- function(adj_vector, old_infl)
{
    sum = 0;
    for (v in adj_vector)
    {
        sum  = sum + old_infl[v]
    }

    return (sum);
}

update_states <- function(g, k)
{
    cost_seq <- runif(100, 0, get_cost(k));
    for (i in 1:100)
    {
        adj_v <- adjacent_vertices(g, i)
        adj_vector <- adj_v[[1]]
        A_k_m <- get_A_k_m(adj_vector,  V(g)$infected)
        state <- change_state(A_k_m, degree(g)[i], cost_seq[i], 
                                        V(g)$infected[i]);
        V(g)$infected[i] <- state;
    }

    return (g)
}

main <- function()
{
    set.seed(42)
    g.er <- erdos.renyi.game(100, 0.10)
    
    for (k in 1:1000)
        g <- update_states(g, k)

    X11();
    influenced <- rbinom(100, 1, 0.20);
    g.er$influenced <- influenced
    g.er.colors <- as.character(length(influenced));
    g.er.colors[influenced == 1] <- "green";
    g.er.colors[influenced == 0] <- "deepskyblue";
    influenced <- update_influenced(g.er, influenced)
    plot(g.er, layout=layout.fruchterman.reingold,vertex.color=g.er.colors)
    print(influenced)

    Sys.sleep(100);

#write update influenced and recalculate smth properties of graph
}

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
        A_k_m <- get_A_k_m(adj_vector, old_infl)
        #
        #tmp <- inf_distr[100*degree_distribution(g)[i] + 1] + sum
        #inf_distr[100*degree_distribution(g)[i] + 1] <- tmp
         
    }
    max_degree <- length(degree_distribution(g))
    print(inf_distr)
    res = c(1:100);
    for(i in 1:max_degree)
    {
        print("Degree")
        print(degree_distribution(g)[i])
        res[i] = 0;
        if (degree_distribution(g)[i] != 0)
            res[i] = inf_distr[i]/(100*degree_distribution(g)[i])
    }

    return (res)
}


