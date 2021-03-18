library(sand)

get_cost_seq <- function(k)
{
    if (k < 20 || k > 50)
        return (1);
    return (10);
} 

change_state <- function(A_k_m, degree, cost_seq_vert, state)
{
    if (state == 0)
        return (cost_seq_vert <= A_k_m)

    return (rbinom(1, 1, 0.3));
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

get_P_d <- function(g)
{
    return (degree_distribution(g))
}

get_ro_k <- function(g)
{
    degrees <- degree(g);
    res <- rep(0, 100);
    for (i in 1:100)
    {
        if (g$influenced[i] == 1)
            res[degrees[i] + 1] =  res[degrees[i] + 1] + 1;
    }
    degr_distr <- 100*get_P_d(g);
    for (i in 1:length(degr_distr))
    {
        if (degr_distr[i] != 0)
            res[i] = res[i]/degr_distr[i];
    }

    return (res);
}

get_infected_link_prob <- function(g)
{
    res = 0;
    denominator = 0;

    P_d <- get_P_d(g);
    ro_k <- get_ro_k(g);

    for(d in 1:min(length(P_d), length(ro_k)))
    {
        res = res +  d*P_d[d]*ro_k[d];
        denominator = denominator + d*P_d[d];
    }
    #print("P_d\n")
    #print(P_d)
    #print("Denominator\n")
    #print(denominator)
    #print(res)

    return (res/denominator)
}

update_states <- function(g, k)
{
    cost_seq <- runif(100, 0, get_cost_seq(k));
    for (i in 1:100)
    {
        adj_v <- adjacent_vertices(g, i)
        adj_vector <- adj_v[[1]]
        A_k_m <- get_A_k_m(adj_vector,  g$influenced)
        state <- change_state(A_k_m, degree(g)[i], cost_seq[i], 
                                        g$influenced[i]);
        g$influenced[i] <- state;
    }

    return (g)
}

main <- function()
{
    set.seed(42)
    proba <- vector()
    g.er <- erdos.renyi.game(100, 0.10)

    influenced <- rbinom(100, 1, 0.05);
    g.er$influenced <- influenced

    theta <- vector() 
    X11()
    for(k in 1:100)
    {
        print(k);
        g.er <- update_states(g.er, k)

        g.er.colors <- as.character(length(influenced));
        g.er.colors[g.er$influenced == 1] <- "green";
        g.er.colors[g.er$influenced == 0] <- "deepskyblue";
        influenced <- update_influenced(g.er, influenced)
        print("influenced")
        proba[k] <- sum(g.er$influenced)/100;
        #plot(g.er, layout=layout.fruchterman.reingold,
        #            vertex.color=g.er.colors)
        #print(g.er$influenced)
            

        theta[k] <- get_infected_link_prob(g.er);
        #Sys.sleep(5);
    }
    print(theta)
    par(mfrow = (c(2, 1)))
    plot(theta)
    plot(proba)
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
    #print(inf_distr)
    res = c(1:100);
    for(i in 1:max_degree)
    {
        #print("Degree")
        #print(degree_distribution(g)[i])
        res[i] = 0;
        if (degree_distribution(g)[i] != 0)
            res[i] = inf_distr[i]/(100*degree_distribution(g)[i])
    }

    return (res)
}

main();
Sys.sleep(100);
