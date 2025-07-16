# ##############################################################################

# Detecting Logical dead-ends using the C-Split method

# The c-split object in rpart provides information for every categorical split in rpart
# For each split, the object stores what happens at the split to each level of the categorical variable
# The 3 values are: 1 - the level goes to the left in the split; 
#                   2 - the level isn't present in the split;
#                   3 - the level goes to the right in the split;
# IF the level of a variable isn't present when it was expected in the split
# THEN the corresponding node is a logical dead-end in the tree
# THUS we are investigating splits where a '2' is present 

################################################################################

library(tidyverse)
library(rpart)

# load additional functions to work with rpart tree
source("extra_rpart_functions.R")


#PART 1: Extracting the required objects from rpart

# Any RDS file containing the rpart object can be used as input here 
tree <- readRDS('data/output/tree_rural_pruned.rds') 

xlevels <- attr(tree, "xlevels")


# Convert rownames of the frame to a column for easier manipulation
frame <- tibble::rownames_to_column(tree$frame, var = "node") %>%
  tibble::as_tibble()

# Convert tree splits and categorical split matrices to tibbles
splits <- tibble::as_tibble(tree$splits)
csplit <- tree$csplit %>%
  tibble::as_tibble() %>%
  tibble::rownames_to_column(var = "index")

# Identify the type of each split: main, primary, or surrogate
n <- nrow(splits)
nn <- frame$ncompete + frame$nsurrogate + !(frame$var == "<leaf>")
ix <- cumsum(c(1L, nn))
ix_prim <- unlist(mapply(ix, ix + c(frame$ncompete, 0), FUN = seq, SIMPLIFY = FALSE))

# Tag each split with its type
split_type <- rep.int("surrogate", n)
split_type[ix_prim[ix_prim <= n]] <- "primary"
split_type[ix[ix <= n]] <- "main"
splits <- dplyr::mutate(splits, type = split_type)

# Extract main splits and align with frame object 
main_splits <- dplyr::filter(splits, type == "main")
main_csplits <- csplit %>% slice(main_splits$index)

# Add ncat and index columns to frame only for non-leaf nodes
not_leaf <- frame$var != "<leaf>"
ncat <- rep.int(0, nrow(frame))
ncat[not_leaf] <- main_splits$ncat
index <- rep.int(0, nrow(frame))
index[not_leaf] <- main_splits$index
frame <- dplyr::mutate(frame, ncat = ncat, index = as.character(index)) %>%
  select("node", "var", "n", "yval", "ncat", "index", "yval2")

# Keep only the main decision nodes where a split occur
main_frame <- frame %>% filter(var != "<leaf>")

# Join the categorical split info with the main frame by using the splitting index
c_split_df <- inner_join(main_frame, main_csplits, by = "index") %>%
  select(node, var, V1, V2, V3, V4, V5, ncat, index)

# PART 2: Identify problematic split due to the absence of an expected level

# Select columns starting by V to detect '2' and create function to detect unvalid '2'
x_cols <- grep("^V\\d+$", names(c_split_df), value = TRUE)

# The function check for '2' in first m columns in csplit, 
# where m is the number of levels for the splitting variable at that node 

validate_row <- function(row, x_cols) {
  values <- as.numeric(row[x_cols])
  ncat <- as.numeric(row[["ncat"]])
  len <- length(values)
  
  # needs to be changed if there is a variable with more than 5 levels 
  allowed_2_positions <- switch(
    as.character(ncat),
    "5" = integer(0),
    "4" = len,
    "3" = (len - 1):len,
    "2" = (len - 2):len,
    NULL
  )
  
  if (is.null(allowed_2_positions)) return(NA)
  disallowed_2 <- which(values == 2)[!(which(values == 2) %in% allowed_2_positions)]
  return(length(disallowed_2) == 0)
}

# Apply the validation function row-wise to identify missing levels 
c_split_df <- c_split_df %>%
  rowwise() %>%
  mutate(check_valid = validate_row(cur_data(), x_cols))

# Flag splits with invalid '2's
flagged_c_split <- c_split_df %>% filter(check_valid == FALSE)

# PART 3: Format c-split to obtain the variable name and missing level for problematic splits

# Create a lookup table of variable levels
max_levels <- max(lengths(xlevels))
col_names <- c("var", paste0("V", seq_len(max_levels)))

xlevel_df <- lapply(names(xlevels), function(var) {
  levels <- xlevels[[var]]
  padded <- c(var, levels, rep(NA, max_levels - length(levels)))
  names(padded) <- col_names
  padded
}) %>%
  do.call(rbind, .) %>%
  as.data.frame(stringsAsFactors = FALSE)

# Join level names with the flagged splits
join_levels <- left_join(flagged_c_split, xlevel_df, by = "var") %>%
  rename(parent_node = node) %>%
  # WHEN a level is missing at a node then both child are dead-ends 
  mutate(child_nodes = list(get_child_node(as.numeric(parent_node)))) 

# Determine which levels were not present (coded as '2')
final_output <- join_levels %>%
  mutate(
    not_present_levels = pmap_chr(
      list(V1.x, V2.x, V3.x, V4.x, V5.x, V1.y, V2.y, V3.y, V4.y, V5.y),
      function(v1, v2, v3, v4, v5, l1, l2, l3, l4, l5) {
        vs <- c(v1, v2, v3, v4, v5)  # Values from csplit
        ls <- c(l1, l2, l3, l4, l5)  # Corresponding labels
        paste(ls[vs == 2 & !is.na(ls)], collapse = ", ")
      }
    )
  ) %>%
  select(variable = var, parent_node, child_nodes, not_present_levels) %>%
  arrange(parent_node)

# View final result with parent node, both child nodes and missing level(s)
final_output %>% View()

# PART 4 : Exclude nodes where the level isn't expected to be present

# This situation occur when a variable is repeated twice in a path
# At the first occurrence, some level(s) of the variable will be excluded from one path
# At the second occurrence, csplit doesn't have this information and still flag the level as missing

# TBC

