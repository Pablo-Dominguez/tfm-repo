## Unused code

```{r Recursive feature elimination, eval=TRUE}
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(zonas[[5]][,names(zonas[[5]]) != "RainTomorrow"], zonas[[5]][,"RainTomorrow"], sizes=2^(2:4), rfeControl=control, metric = "Accuracy")
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
```
enlace: https://link.springer.com/article/10.1023/A:1012487302797


```{r FAMD, eval=FALSE}
# Factor analysis of mixed data

## FAMD
res.famd <- FAMD(zonas[[5]],
                 sup.var = 17,  ## Set the target variable "RainTomorrow" as a supplementary variable
                 graph = TRUE,
                 ncp=8)

## Inspect principal components
get_eigenvalue(res.famd)
```


