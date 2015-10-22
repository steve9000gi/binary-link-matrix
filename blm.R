# R script blm.R 2015/10/22 (blm: binary link matrix)
# Accepts paths to input and output directories as command line arguments. For each json file in
# the input directory, read that file into variable "map", then create a "binary link matrix" as a
# data-frame of dimension n, where n is the number of nodes in the input JSON file. The rows and
# columns of the data frame are named by the node names "map$nodes$name". The value of m[i, j] is 1
# if there is a link between node[i] and node[j], otherwise 0. For each file write 1) the blm to a
# csv output file, along with 2) a list of nodes showing id, type (role, responsibility, etc.), and
# name (associated text), and 3) a list of links showing source and target node ids and name
# (associated text).
#
# Also create a single "node classes" file wth a list of the names of all the nodes from all the
# files in the input directory, where those names are organized by node type. Thus, names for all
# the roles are listed together, names for all responsibilities are listed, etc.

library(tools)
library(jsonlite)

generateMap = function(inputFileName) {
  return (fromJSON(inputFileName))
}

generateOutputBLMFilePath = function(inputFileName, outputDirectoryPath) {
  charVec = strsplit(inputFileName, "/")
  fNameBase = file_path_sans_ext(charVec[[1]][length(charVec[[1]])]);
  outputBLMPath = paste0(outputDirectoryPath, "/", fNameBase, "-BLM.csv", collapse = "")
  return (outputBLMPath)
}

generateOutputNodeClassesFileName = function(outputDirectoryPath) {
  return(paste0(outputDirectoryPath, "/NodeClasses.csv"))
}

initLinkDataFrame <- function(nodeId) {
  # Create a data frame whose row and column names are the node ids, and all of
  # whose elements are 0.
  nNodes = length(nodeId)[1]
  m = matrix(NA, nrow = nNodes, ncol = nNodes)
  colnames(m) = rownames(m) = nodeId
  linkDF = data.frame(m)
  colnames(linkDF) = nodeId
  for (row in 1:nNodes) {
    for(column in 1:nNodes) {
      linkDF[row, column] <- 0
    }
  }
  return (linkDF)
}

populateLinkDataFrame = function(links, blankDF) {
  # For a given cell from row[i] and column[j], where i and j are the ids of
  # their respective nodes, put 1 in that cell if there is a link from
  # node[id = i] and node[id=j] in either direction.
  df = blankDF
  nLinks = dim(links)[1]
  write(paste0('nLinks: "', nLinks, '"; ', typeof(nLinks)), "/Users/stevec/Documents/blm/error.out", append = TRUE)
  
  if (!is.null(nLinks)) {
    for (i in 1:nLinks) {
      s = as.character(links$source[i])
      t = as.character(links$target[i])
      df[[s, t]] = 1
      df[[t, s]] = 1
    }
  }
  return (df)  
}

generateNodeTypeVector = function(map) {
  # Accepts a map, returns a vector of the corresponding node
  # types, based on our System Support Map conventions.
  nShapes = length(map$nodes$shape)
  nodeTypes=c()
  for (i in 1:nShapes) {
    nodeTypes[i] = switch(map$nodes$shape[i],
                          "circle"    = "role",
                          "rectangle" = "responsibility",
                          "diamond"   = "need",
                          "ellipse"   = "resource",
                          "star"      = "wish",
                          "noBorder"  =  "text"
                          )
  }
  return (nodeTypes)
}

generateNodeClasses = function(map) {
  # Accepts a map, returns the corresponding node
  # classes, based on our System Support Map conventions.
  nShapes = length(map$nodes$shape)
  #classes = list("role" : c(), )
  roles  = c()
  resps  = c()
  needs  = c()
  rsrces = c()
  wishes = c()
  texts  = c()
  for (i in 1:nShapes) {
    shape = map$nodes$shape[i]
    if (shape == "circle") {
      roles = c(roles, map$nodes$name[i])
    } else if (shape == "rectangle") {
      resps = c(resps, map$nodes$name[i])
    } else if (shape == "diamond") {
      needs = c(needs, map$nodes$name[i])
    } else if (shape == "ellipse") {
      rsrces = c(rsrces, map$nodes$name[i])
    } else if (shape == "star") {
      wishes = c(wishes, map$nodes$name[i])
    } else if (shape == "noBorder") {
      texts = c(texts, map$nodes$name[i])
    } else {
      print("Unknown shape")
    }
  }
  return (list(roles, resps, needs, rsrces, wishes, texts))
}

processJSON = function(inputFileName, outputDirectoryPath) {
  map = generateMap(inputFileName)
  blmFilePath = generateOutputBLMFilePath(inputFileName, outputDirectoryPath)
  outputDirectoryPath
  
  nodeType = generateNodeTypeVector(map)
  classes = generateNodeClasses(map)
  nodes <- data.frame(map$nodes$id, nodeType, map$nodes$name)
  names(nodes) <- c("NodeID", "NodeType", "Name")
  links <- data.frame(map$links$source, map$links$target, map$links$name)
  if (length(links) > 0) {
    names(links) <- c("Source", "Target", "Name")
  }

  rowNames = 1:length(map$nodes$id)
  NodeID = map$nodes$id
  linkDF = populateLinkDataFrame(map$links, initLinkDataFrame(map$nodes$id))
  tryCatch({
    write.table(cbind(nodes, NodeID, linkDF),
                file = blmFilePath, append = FALSE,
                quote = FALSE, row.names = rowNames, col.names = NA, sep = "\t")
    write("\n", file = blmFilePath, append = TRUE)
    write.table(links,
                file = blmFilePath, append = TRUE,
                quote = FALSE, row.names = TRUE, col.names = NA, sep = "\t")
  }, error = function(e) {
    message(e)
    return(NULL)
  })
  return (classes)
}

# main
args = commandArgs()
outputDirectoryPath = args[7]

outputNodeClassesFileName = generateOutputNodeClassesFileName(outputDirectoryPath)
roles = c()
resps = c()
needs = c()
rsrces = c()
wishes = c()
texts = c()

setwd(args[6])
inputFiles = list.files(path = args[6], pattern = "*.json")
nInputFiles = length(inputFiles)

for (i in 1:nInputFiles) {
  classes = processJSON(inputFiles[i], outputDirectoryPath)
  roles = c(roles, classes[[1]])
  resps = c(resps, classes[[2]])
  needs = c(needs, classes[[3]])
  rsrces = c(rsrces, classes[[4]])
  wishes = c(wishes, classes[[5]])
  texts = c(texts, classes[[6]])
}

write("ROLES:", outputNodeClassesFileName, append = FALSE)
write(roles, outputNodeClassesFileName, append = TRUE)
write("\nRESPONSIBILITIES:", outputNodeClassesFileName, append = TRUE)
write(resps, outputNodeClassesFileName, append = TRUE)
write("\nNEEDS:", outputNodeClassesFileName, append = TRUE)
write(needs, outputNodeClassesFileName, append = TRUE)
write("\nRESOURCES:", outputNodeClassesFileName, append = TRUE)
write(rsrces, outputNodeClassesFileName, append = TRUE)
write("\nWISHES:", outputNodeClassesFileName, append = TRUE)
write(wishes, outputNodeClassesFileName, append = TRUE)
write("\nTEXTS:", outputNodeClassesFileName, append = TRUE)
write(texts, outputNodeClassesFileName, append = TRUE)