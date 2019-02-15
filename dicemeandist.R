
library(tidyverse)
library(arrangements)
library(animation)

seq_with_rep <- function(x){
	factorial(length(x)) / prod(factorial(table(x)))
}

plot_dicemeandist <- function(n){

	mat <- combinations(6, n, replace = TRUE)
	prob <- apply(mat, 1, seq_with_rep) / 6^n
	average <- apply(mat, 1, mean)
	
	df <- data.frame(prob, average)	%>% 
		group_by(average) %>% 
		summarise(prob = sum(prob))
	
	ex <- sum(df$average*df$prob)
	sd <- sqrt(sum(df$average^2*df$prob) - ex^2)
	stats <- data.frame(min = ex - sd, ex = ex, max = ex + sd)
	
	p <- ggplot(df, aes(average, prob)) +
		geom_pointrange(aes(ymin = 0, ymax = prob),
										linetype="dashed") +
		ylim(c(0, 0.17)) +
		geom_point(aes(ex, 0.025), data = stats, size = 5, color = "#009f8c") +
		geom_errorbarh(aes(xmin = ex - sd, xmax = ex + sd,
											 y = 0.025, height = 0.025),
									 color = "#009f8c", size = 2) +
		theme_minimal(base_size = 18) +
		annotate("label", x = ex - sd, y = 0.05, size = 8,
						 label = "mu - sigma", parse = TRUE) +
		annotate("label", x = ex, y = 0.05, size = 8,
						 label = "mu", parse = TRUE) +
		annotate("label", x = ex + sd, y = 0.05, size = 8,
						 label = "mu + sigma", parse = TRUE) +
		ggtitle(paste0("n = ", n)) +
		xlab("x") + ylab("P(X=x)")
	print(p)
}

animation::saveGIF({
	ani.options(loop = TRUE)
	for(n in 1:10){
		plot_dicemeandist(n)
	}
}, movie.name = "dicemeandist.gif", interval = 0.5)

