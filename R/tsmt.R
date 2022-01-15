#' tsmt
#'
#' @return result of t test, Weclch's test, or Wicoxon's rank sum test
#' @export
#'
#' @examples
#'
tsmt <- function(){
  filename <- readline("File name followed by .csv?: ")
  smpl_data <- read.csv(filename, header = T)
  GA <- smpl_data$value[smpl_data$group == "A"]
  GB <- smpl_data$value[smpl_data$group == "B"]
  normal.A <- shapiro.test(GA)
  normal.B <- shapiro.test(GB)
  equal.var <- var.test(GA, GB)
  if(normal.A$p.value < .05 | normal.B$p.value <.05) {
    testresult <- wilcox.test(GA, GB, digits.rank = 7)
  } else if(equal.var$p.value < .05) {
    testresult <- t.test(GA, GB, var.equal = FALSE,
                         alternative = "two.sided", mu = 0)
  } else {
    testresult <- t.test(GA, GB, var.equal = TRUE,
                         alternative = "two.sided", mu = 0)
  }
  print(testresult)
  A.name <- readline("Name of Group A?:")
  B.name <- readline("Name of Group B?:")
  Group <- c(A.name, B.name)
  Group_Mean <- c(mean(GA), mean(GB))
  se.data <- c(sd(GA)/sqrt(length(GA)), sd(GB)/sqrt(length(GB)))
  tsm.frame <- data.frame(Group, Group_Mean, se.data)
  if(normal.A$p.value < .05 | normal.B$p.value <.05){
    with(smpl_data, boxplot(value ~ group, ylab = "Frequency",
                           col = c("skyblue", "orange") ))
  } else {
    yroof <- round(max(tsm.frame$Group_Mean + tsm.frame$se.data)*1.2, 1)
    bp <- barplot(tsm.frame$Group_Mean,
                  ylab = "Frequency",
                  ylim = c(0, yroof),
                  names.arg = c(A.name, B.name),
                  col = c("skyblue", "orange"))
    arrows(bp, tsm.frame$Group_Mean, bp,tsm.frame$Group_Mean +
             tsm.frame$se.data, angle=90,length=0.25,lwd=2)
    arrows(bp, tsm.frame$Group_Mean, bp,tsm.frame$Group_Mean -
             tsm.frame$se.data, angle=90,length=0.25,lwd=2)}
}
