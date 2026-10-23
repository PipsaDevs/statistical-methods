# ========================================================================
# Three machines, A, B and C, produce 50%, 30% and 20% of the total
# number of items in a factory, respectively. The percentages of defective
# items produces by these machines are 3%, 5% and 10%. If a batch of
# products is randomly selected, find the proportion of defective items.
# ========================================================================

source("R/chapter4_formulas.R")

machines <- c(0.5, 0.3, 0.2)
items <- c(0.03, 0.05, 0.1)

conditional_probability(machines, items)
