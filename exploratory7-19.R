# Short EDA: Summary Stats
# 1. How much data is missing for each relationship (i.e. % missing for PA, PB, OIP1-OIP5)?
# 2. For what proportion of entries is person A a parent? Person B? % single-parent vs. two parent households?
# 3. Who is the primary caregiver in cases when the primary caregiver (PA) is not a parent?

load("~/git/vdss/data/vdss/data/nis_relationships_short.Rdata")
library("tidyr")
library("dplyr")
library("purrr")
library("tidyverse")

# View data
View(nis_relationships_short)

# Total NAs in data
sum(is.na(nis_relationships_short))

# Percent NA for each column, as list
nis_relationships_short %>% 
  map(~ mean(is.na(.)))

# Answer to question 1: 
# Percent NA for each column, rounded, different organization
round(colMeans(is.na(nis_relationships_short)), 3)

#Answer to question 2:
# Percent PA=Parent, Foster Parent, Guardian, Step Parent, Grandparent,
# Other Family Member, Sibling, or Unrelated
table(nis_relationships_short$PA) / length(nis_relationships_short$PA)
## Results: PA = Parent 91%, Grandparent = 2.5%, Step Parent = 1.3%,
##          Foster Parent = 1.2%, Other Relative = 1.2%, Unrelated = 0.98%,
##          Guardian = 0.84%, Sibling = 0.28%


# Percent who PB is to child
table(nis_relationships_short$PB) / length(nis_relationships_short$PB)
## Results: PB = Parent 32.8%, Unrelated = 10.2%, Step Parent = 8.4%,
##          Grandparent = 4%, Other Relative = 1.4%, Foster Parent = 0.56%,
##          Guardian = 0.28%, Sibling = 0.46%


# % single-parent & two-parents
# Need to think about how to handle NAs**
onetwoparent = nis_relationships_short %>%
  filter(PA == "Parent" | PB == "Parent") %>%
  mutate(twoparent = (PA == "Parent" & PB == "Parent")) %>%
  mutate(oneparent = !(PA == "Parent" & PB == "Parent")) %>%
  select(oneparent, twoparent)

round(colMeans(onetwoparent, na.rm = T), 3)

# % single-foster, % two-foster
fosterparent = nis_relationships_short %>%
  filter(PA == "Foster Parent" | PB == "Foster Parent") %>%
  mutate(two_fosterparent = (PA == "Foster Parent" & PB == "Foster Parent")) %>%
  mutate(one_fosterparent = !(PA == "Foster Parent" & PB == "Foster Parent")) %>%
  select(one_fosterparent, two_fosterparent)

round(colMeans(fosterparent, na.rm = T), 3)

# % Grandparent
grandparent = nis_relationships_short %>%
  filter(PA == "Grandparent" | PB == "Grandparent") %>%
  mutate(two_grandparent = (PA == "Grandparent" & PB == "Grandparent")) %>%
  mutate(one_grandparent = !(PA == "Grandparent" & PB == "Grandparent")) %>%
  select(one_grandparent, two_grandparent)

round(colMeans(grandparent, na.rm = T), 3)

# % Unrelated
unrelated = nis_relationships_short %>%
  filter(PA == "Unrelated" | PB == "Unrelated") %>%
  mutate(two_unrelated = (PA == "Unrelated" & PB == "Unrelated")) %>%
  mutate(one_unrelated = !(PA == "Unrelated" & PB == "Unrelated")) %>%
  select(one_unrelated, two_unrelated)

round(colMeans(unrelated, na.rm = T), 3)

# % Other Relative
unrelated = nis_relationships_short %>%
  filter(PA == "Unrelated" | PB == "Unrelated") %>%
  mutate(two_unrelated = (PA == "Unrelated" & PB == "Unrelated")) %>%
  mutate(one_unrelated = !(PA == "Unrelated" & PB == "Unrelated")) %>%
  select(one_unrelated, two_unrelated)

round(colMeans(unrelated, na.rm = T), 3)

