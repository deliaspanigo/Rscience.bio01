
# Report Package
# https://www.youtube.com/watch?v=_ypkrGyqyZ4

library(report)

report_system()
report_packages()
cite_packages()

base <- mtcars

ttest <- t.test(base[,1], y = base[,2])

report(ttest)
