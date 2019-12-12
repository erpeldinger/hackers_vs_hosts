
------------------------- 1 - COMPILATION --------------------------------------

In order to compile the project, you need only one command : make

------------------------- 2 - TESTING ------------------------------------------

The main file is ftest.ml, so in order to launch a test, you should write the 
following commands in a terminal : 

./ftest.native graphs/<file>.txt source sink graphs/<file>.dot
dot -Tsvg graphs/<file>.dot > graphs/<file>.svg

Note that 3 graphs are available for testings :
        - graphFF.txt ; containing 9 nodes (from 0 to 9)
        - graph_bip/.txt ; containing 10 nodes (from 0 to 8)
        - graphMF.txt ; containing 5 nodes (from 0 to 4)

See below how to test each part of the project.


------------------------- 3 - FORD FULKERSON------------------------------------

This part of the project implements the Ford Fulkerson algorithm in order to 
find the maximum flow of a path found between 2 nodes in a graph.

Note : our algorithm is to be used with the graphFF.txt file. 
To do so, the "FORD FULKERSON" part in the ftest.ml file must be uncommented.

To improve : 
        - Our Ford Fulkerson algorithm cannot be appplied to a bipartite graph, 
this is an issue to be solved.
        - At the end of the Ford Fulkerson algorithm, we export a residual graph
instead of a graph of flow, which is not ideal to observe the expected result.


------------------------- HACKERS VS HOSTS -------------------------------------

This part has been partly implemented. It it supposed to solve a bipartite 
matching use-case : housing hackers during an event. 

Note : cannot be tested.

See further details in /appli-hackers_vs_hosts/readme_hvh.pdf

------------------------- MAX FLOW / MIN COST ----------------------------------

This part of the project is a mixture between the Ford Fulkerson algorithm,
the Bellman-Ford one and the Busacker-Gowen one, in order to get the maximum 
flow corresponding to the minimum cost in a graph.

Note : our algorithm is to be used with all graphs. It can be applied to a 
bipartite graph.
To do so, the "FORD FULKERSON MAX FLOX MIN COST" part in the ftest.ml file 
must be uncommented.

To improve : 
        - Calculations are performed on two different graphs (the graph of flow
and residual one). In order to reduced the number of operations, all 
calculations should be done on only one graph (as implemented in the Ford-
Fulkerson part).
        - In order to optimize the code, we could use more predefined functions
of modules (or of graph.ml).
        - The "get_cout_chemin" function could be deleted so that costs should
getted in the final list of costs.  
        - Within the two functions "arcs1_MCFM" and "arcs2_MCMF" the case where 
the cost is 0 is not taken into account, to do so, something has to be done with
the max_int.


