# ===========================================================================
# A spanish pharmaceutical company purchases FFP2 masks from three
# different suppliers based in Argentina, Belgium, and China, with
# probabilities of 1/6, 1/2, and 1/3, respectively. It is observed that the
# Argentine manufacturer produces 18% of defective masks, while the Belgian
# batch has 2 defective masks out of every 25. In a work inspection, it is
# noted that 10% of the masks are defective. You are asked to:
#
# 1. If a mask is chosen at random and it is found to be defective,
# calculate the probability that it comes from China.
#
# 2. If a mask is chosen at random and it is found to be in perfect condition,
# determine from which manufacturer it is most likely to have come.
# ============================================================================

source("R/chapter4_formulas.R")

SupArgBel <- c(1 / 6, 1 / 2) # P(A), P(B)

DefArgBel <- c(0.18, 0.08) # P(D|A), P(D|B)

SupChina <- 1 / 3 # P(C)

Inspection <- 0.1 # P(D)

pdc <- (Inspection - conditional_probability(SupArgBel, DefArgBel)) / SupChina # P(D|C)

pcd <- (SupChina * pdc) / Inspection

print(paste("1:",pcd))

# Missing part 2
