#grouping the data and t-test
manual <- mtcars[mtcars$am == "1",]$mpg
auto <- mtcars[mtcars$am == "0",]$mpg
matrix(c(mean(auto), mean(manual))) -> tab
rownames(tab) <- c("auto", "manual")
colnames(tab) <- "Transmission Type"
print(tab)

t.test(manual, auto, alternative = "greater")
