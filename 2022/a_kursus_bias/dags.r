library(ggdag)


* selection bias / self-selection bias
* blinding 
* 


https://ggdag.malco.io/articles/bias-structures.html


tidy_ggdag <- dagify(
    O ~ I + A,
    A ~ S,
    I ~ S,
    exposure = "I",
    outcome = "O",
    labels = c(O = "outcome", I = "Intervention", A = "Age", S = "Surgery preference")
) %>%
    tidy_dagitty()


ggdag(tidy_ggdag, use_labels = "label") +
    theme_dag()


ggdag_adjustment_set(tidy_ggdag, node_size = 14) +
    theme(legend.position = "bottom")
