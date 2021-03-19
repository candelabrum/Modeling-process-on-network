library(sand)

fact <- function(n)
{
    if (n <= 1)
        return (1);

    return (n*fact(n-1));
}

binom <- function(n, m)
{
    return (fact(n)/(fact(n-m)*fact(m)));
}


get_cost_seq <- function(k)
{
    if (k < 20 || k > 50)
        return (1);
    return (10);
} 

p_01 <- function(a, k)
{
    return (min(a/get_cost_seq(k), 1));
}

emp_theta <- function(emp_ro_k, g)
{
    res = 0;
    denominator = 0;

    P_d <- get_P_d(g);
    ro_k <- emp_ro_k;
    D <- length(degree(g));

    for(d in 1:min(length(P_d), length(ro_k), D))
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

emp_p_01 <- function(emp_ro_k, d, g, k)
{
    sum = 0;
    emp_theta <- emp_theta(emp_ro_k, g)

    for (a in 0:d)
    {
        sum = sum + p_01(a, k)*binom(d, a)*(emp_theta ** a)*
                            ((1 - emp_theta) ** (d-a));
    }
    
    return ((1-emp_ro_k[d])*sum);
}

p_10 <- function()
{
    return (1/3);
}

emp_p_10 <- function(emp_ro_k, d, g, k)
{
    sum = 0;
    emp_theta <- emp_theta(emp_ro_k, g)

    for (a in 0:d)
    {
        sum = sum + p_10()*binom(d, a)*(emp_theta ** a)*
                            ((1 - emp_theta) ** (d-a));
    }
    
    return ((emp_ro_k[d])*sum);
}

update_emp_ro_k <- function(emp_ro_k, g, k)
{
    D <- length(degree_distribution(g));
    print(D)
    ro_k <- emp_ro_k
    if (ro_k[90] > 0)
        exit();
    for(d in 1:D)
    {
        ro_k[d] <- ro_k[d] + (1/100)*(emp_p_01(emp_ro_k, d, g, k) - 
                                        emp_p_10(emp_ro_k, d, g, k));
    }

    return (ro_k);
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

#get_theory_p_01_ <- function(
main <- function()
{
    set.seed(42)
    proba <- vector()
    g.er <- erdos.renyi.game(100, 0.20)

    influenced <- rbinom(100, 1, 0.05);
    g.er$influenced <- influenced
    
    ro_k <- get_ro_k(g.er);
    theta <- vector() 
    X11()
    emp_theta <- vector();
    for(k in 1:100)
    {
        print(k);
        g.er <- update_states(g.er, k)

        g.er.colors <- as.character(length(influenced));
        g.er.colors[g.er$influenced == 1] <- "green";
        g.er.colors[g.er$influenced == 0] <- "deepskyblue";
        influenced <- update_influenced(g.er, influenced)
        proba[k] <- sum(g.er$influenced)/100;
        #plot(g.er, layout=layout.fruchterman.reingold,
        #            vertex.color=g.er.colors)
        #print(g.er$influenced)
        if (k <= 5)
        {
            emp_ro_k <- update_emp_ro_k(get_ro_k(g.er), g.er, k);
        }
        if (k > 5) 
            emp_ro_k <- update_emp_ro_k(emp_ro_k, g.er, k);
        
        emp_theta[k] <- emp_theta(emp_ro_k, g.er);
        #plot(emp_ro_k)
            
         
        theta[k] <- get_infected_link_prob(g.er);
    }

    print(theta)
    par(mfrow = (c(2, 1)))
    plot(theta, main = "Real Infected probabily link",
         col = "deepskyblue", type = "l")
    plot(emp_theta, main = "Theorical estimate",
                col = "deepskyblue", type = "l")
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
