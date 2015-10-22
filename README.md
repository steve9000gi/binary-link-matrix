R script blm.R 2015/10/22 (blm: binary link matrix)
Accepts paths to input and output directories as command line arguments. For each json file in
the input directory, read that file into variable "map", then create a "binary link matrix" as a
data-frame of dimension n, where n is the number of nodes in the input JSON file. The rows and
columns of the data frame are named by the node names "map$nodes$name". The value of m[i, j] is 1
if there is a link between node[i] and node[j], otherwise 0. For each file write 1) the blm to a
csv output file, along with 2) a list of nodes showing id, type (role, responsibility, etc.), and
name (associated text), and 3) a list of links showing source and target node ids and name
(associated text).

Also create a single "node classes" file wth a list of the names of all the nodes from all the
files in the input directory, where those names are organized by node type. Thus, names for all
the roles are listed together, names for all responsibilities are listed, etc.

