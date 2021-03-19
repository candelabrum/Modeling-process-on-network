# Modeling-process-on-network
Modeling epidemic, information spread and rumor, etc.

This is model of some small economic network from book
"Cooperative and Graph signal processing", Part 21.
Assume each agen is myopic optimizer and chooses to adopt he technology
only if с_m <= A_k_m, where c_m, m = 1, ..., 100 are i.i.d random 
variables simulated from uniform distribution U[0, C(s_k)].
A_k_m - number of infected neighbors of node m at time k.
The agent evolves as discrete time Markov process.
p_01 = min(A_k_m/C(s_k), 1) -  transition probabilities
p_10 = 0.3 - probability that a product fails 

Let infected link probability - probability that at time k a uniformly
sampled link in the network points to an infected node.

to see results, just:
make
