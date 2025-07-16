################################################################################

# This file contains all additional functions that are
# required to extract information from a tree

################################################################################

#find node number of sibling and check that the sibling exist in the tree
get_valid_sibling_node <- function(node_number, frame) {
  possible_parents <- as.numeric(row.names(frame))
  parent <- floor(node_number / 2)
  if (!all(parent %in% possible_parents)) return(NA)
  
  if (node_number %% 2 == 0) {
    return(parent * 2 + 1)  # sibling is right
  } else {
    return(parent * 2)      # sibling is left
  }
}

# provide a list of all nodes in path 
get_path_nodes <- function(leaf_node) {
  path_nodes <- c()
  node <- leaf_node
  while (node >= 1) {
    path_nodes <- c(as.character(node), path_nodes)  # prep end to keep order from root to leaf
    node <- floor(node / 2)
  }
  return(path_nodes)
}

#provide a list of all variables used at main nodes where a split occurs
get_path_variables <- function(leaf_node) {
  leaf_node <- as.character(leaf_node)
  var_list <- c()
  path_splits <- path.rpart(tree, nodes = leaf_node, print.it = F)
  
  for (split in path_splits) {
    for (step in split) {
      if (step == "root") next
      split_parts <- strsplit(step, "=", fixed = TRUE)[[1]]
      if (length(split_parts) != 2) next
      
      var <- split_parts[1]
      var_list <- c(var_list, var)
    }
  }
  
  return(var_list)
}

# provide a list of all leves in a path
get_path_levels <- function(leaf_node) {
  leaf_node <- as.character(leaf_node)
  lvl_list <- c()
  path_splits <- path.rpart(tree, nodes = leaf_node, print.it = FALSE)
  
  for (split in path_splits) {
    for (step in split) {
      if (step == "root") next
      split_parts <- strsplit(step, "=", fixed = TRUE)[[1]]
      if (length(split_parts) != 2) next
      lvl <- split_parts[2]
      lvl_list <- c(lvl_list, lvl)
    }
  }
  
  return(lvl_list)
}

# output the node number of two child nodes when a node number is given
get_child_nodes <- function(node) {
  c(2 * node, 2 * node + 1)
}