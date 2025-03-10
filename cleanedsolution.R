# Setting up our dataframe
subject_1 <- c("B","B","B","M","M","C")
subject_2 <- c("M","C","P","C","P","P")
clashes <- c(5,2,4,3,2,1)
# We list all pairs of subjects twice (in both orders)
df <- data.frame(Subject_1 = c(subject_1,subject_2), Subject_2 = c(subject_2,subject_1), Clashes = rep(clashes,2))

# These are the variables where we will store the minimum number of clashes achieved
# And the subject combinations that achieve them
min_score <- Inf
slot_1 <- c()
slot_2 <- c()

# We list the distinct subjects
subjects <- unique(c(subject_1,subject_2))
num_subjects <- length(subjects)

# We loop over all possible numbers of subjects to have in the first slot
for (i in ceiling(num_subjects/2):num_subjects) {
  subject_subsets <- combn(subjects, i, simplify = FALSE)
  # We loop over every set of subjects of a particular size
  for (sub_1 in subject_subsets) {
    sub_1_pairs <- combn(sub_1, 2, simplify = FALSE)
    sub_1_score <- 0
    # We loop over every pair of subjects within the subset to find the total number of clashes
    for (pair in sub_1_pairs) {
      sub_1_score <- sub_1_score + df$Clashes[(df$Subject_1 == pair[1]) & (df$Subject_2 == pair[2])]}
    # We work out the number of clashes in the second slot
    sub_2 <- setdiff(subjects,sub_1)
    sub_2_score <- 0
    if (length(sub_2) >= 2) {
      sub_2_pairs <- combn(sub_2, 2, simplify = FALSE)
      for (pair in sub_2_pairs) {
        sub_2_score <- sub_2_score + df$Clashes[(df$Subject_1 == pair[1]) & (df$Subject_2 == pair[2])]}}
    score <- sub_1_score + sub_2_score
    # If the number of clashes with this timetabling arrangement is less than the previous minimum
    # We update the minimum score and the subjects that go in the two slots
    if (score < min_score) {
      min_score <- score
      slot_1 <- sub_1
      slot_2 <- sub_2}}}

cat("The minimum number of clashes is",min_score,"achieved by having the two slots be",slot_1,"and",slot_2)
