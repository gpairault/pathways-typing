################################################################################

# Detecting logical dead-ends where a level is lost when a variable appears twice in a tree

# There are two main steps here:
#      1. Detect nodes where a level as disappeared when the node is repeated in the path
#      2. Check that the missing level isn't present in the sibling node

################################################################################


library(tidyverse)
library(rpart)
library(tidyverse)
library(rpart)

# load additional functions to work with rpart tree
source("extra_rpart_functions.R")

# LOAD RPART TREE FROM SEGMENTATION OUTPUT

tree <- readRDS('data/output/tree_rural_pruned.rds')
frame <- tree$frame
leaf_nodes <- as.numeric(row.names(frame[frame$var == "<leaf>", ]))
paths_list <- path.rpart(tree, nodes = leaf_nodes, print.it = FALSE)


# PART 1: DETECT NODES WHERE A VARIABLE IS TWICE IN THE SAME PATH AND A LEVEL DISAPPEARED

detect_lost_label <- function(tree) {
  results <- list()
  for (path_name in names(paths_list)) {
    path <- paths_list[[path_name]]
    var_first_levels <- list()
    
    for (split in path) {
      if (split == "root") next
      split_parts <- strsplit(split, "=", fixed = TRUE)[[1]]
      if (length(split_parts) != 2) next
      
      var <- split_parts[1]
      lvl <- strsplit(split_parts[2], ",", fixed = TRUE)[[1]]
      
      # Store the levels if its the first variable's occurrence in the path
      if (is.null(var_first_levels[[var]])) {
        var_first_levels[[var]] <- lvl
      } else {
      # Compare current level to first stored level
        seen_levels <- var_first_levels[[var]]
        missing_levels <- setdiff(seen_levels, lvl)
        
      # Output leaf node where a level dissapeared in the path
        if (length(missing_levels) > 0) {
          result_entry <- list(
            variable = var,
            first_seen = paste(seen_levels, collapse = ", "),
            remaining = paste(lvl, collapse = ", "),
            disappeared = paste(missing_levels, collapse = ", "),
            affected_leaf = path_name
          )
          results <- append(results, list(result_entry))
        }
      }
    }
  }
  
  if (length(results) == 0) {
    return(tibble::tibble(
      variable = character(),
      first_seen = character(),
      remaining = character(),
      disappeared = character(),
      affected_leaf = character()
    ))
  } else {
    result_df <- do.call(rbind, lapply(results, as.data.frame))
    return(tibble::as_tibble(result_df))
  }
}


df_detect_label <- detect_lost_label(tree)
df_detect_label %>% View()


# PART 2 : # CHECK THAT MISSING LEVEL ISN'T PRESENT IN THE SIBLING NODE
    
identify_dead_ends <- function(df_lost_label, frame_tree) {
  real_dead_end_df <- data.frame()
  
  # Iterate over unique affected leaf nodes from previous step
  for (leaf_node_number in unique(df_lost_label$affected_leaf)) {
    
    # Get all rows in df_lost_label for the current leaf node
    rows <- which(df_lost_label$affected_leaf == leaf_node_number)
    
    # Loop over each associated variable entry for this leaf
    for (i in rows) {
      split_var <- df_lost_label$variable[i]
      missing_level <- df_lost_label$disappeared[i]
      # split level by "," to identify which one disappeared
      missing_level_list<- strsplit(missing_level, ",", fixed = TRUE)[[1]]
      node_level <- df_lost_label$remaining[i]
      
      path_node_list <- get_path_nodes(as.numeric(leaf_node_number))
      path_var_list <- get_path_variables(as.character(leaf_node_number))
      
      # Check for multiple occurrences safely
      match_positions <- which(path_var_list == split_var)
      if (length(match_positions) < 2) next  # skip if second occurrence doesn't exist
      
      var_position_path <- match_positions[2] + 1 # take root into account
      # Obtain node number where the level disappeared
      var_node_number <- path_node_list[var_position_path]
      
      sibling_node <- find_valid_sibling_node(as.numeric(var_node_number), frame_tree)
      sibling_var <- tail(get_path_variables(sibling_node), n = 1)
      sibling_path_lvl <- get_path_levels(sibling_node)
      sibling_node_level <- sibling_path_lvl[var_position_path - 1] #adjust for root
      # Store the level in the sibling node as a vector list
      # Tests will be performed in all levels of the sibling eg. x = a or b or c
      sibling_node_level_list <- if (!is.null(sibling_node_level) &&
                                     length(sibling_node_level) > 0 &&
                                     !is.na(sibling_node_level)) {
        strsplit(as.character(sibling_node_level), ",", fixed = TRUE)[[1]]
      } else {
        character(0)
      }
      
      # Iterate of each disappeared level found in the previous step
      
      for (missing in missing_level_list){
        missing <- trimws(missing)    
        
        # Flag node as dead-end if none of disappeared level are in the sibling node
        if (!any(missing %in% trimws(sibling_node_level_list)))
        
        # Output: dataframe with "true" logical dead-ends
          real_dead_end_df <- rbind(real_dead_end_df, data.frame(
            node_number = as.character(var_node_number),
            variable_name = split_var,
            missing_cat = missing,
            node_level = paste(node_level, collapse = ","),
            sibling_level = paste(sibling_node_level_list, collapse = ","),
            stringsAsFactors = FALSE
          ))
        }}}
   real_dead_end_df <- unique(real_dead_end_df) 
   return(real_dead_end_df)
}

identify_dead_ends(df_detect_label, frame) %>% View()
