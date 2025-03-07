subject_1 <- c("B","B","B","M","M","C","M","C","P","C","P","P")
subject_2 <- c("M","C","P","C","P","P","B","B","B","M","M","C")
clashes <- c(5,2,4,3,2,1,5,2,4,3,2,1)
df <- data.frame(Subject_1 = subject_1, Subject_2 = subject_2, Clashes = clashes)
# Displaying the dataframe
print(df)

min_score <- 1000
slot_1 <- c()
slot_2 <- c()

subjects <- c("B","M","C","P")
for (i in 2:3) {
  subject_subsets <- combn(subjects, i, simplify = FALSE)
  for (x in subject_subsets) {
    x_pairs <- combn(x, 2, simplify = FALSE)
    x_score <- 0
    for (pair in x_pairs) {
      x_score <- x_score + df$Clashes[(df$Subject_1 == pair[1]) & (df$Subject_2 == pair[2])]
    }
    r <- setdiff(subjects,x)
    if (length(r) == 2) {
      remaining_score <- df$Clashes[(df$Subject_1 == r[1]) & (df$Subject_2 == r[2])]
    } else {
      remaining_score <- 0
    }
    score <- x_score + remaining_score
    if (score < min_score) {
      min_score <- score
      slot_1 <- x
      slot_2 <- r
    }
  }
}
slot_1
slot_2
