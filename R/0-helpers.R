## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2018 Felix Schönbrodt
##-----------------------------------------------------------------------------
## Purpose: Helper functions
##-----------------------------------------------------------------------------

# picture IDs of the "standard six" picture set
standardSix <- c("women in laboratory", "boxer", "trapeze artists", "couple by river", "ship captain", "nightclub scene")


# simple wrapper: formats a number in f.2 format
#' @param trimToZero: if a number is, say -0.0001, trim it to exactly zero (otherwise, it is displayed as -.00 in the correlation matrix)
f2 <- function(x, digits=0, skipZero=FALSE, prepoint=0, trimToZero=0) {
	
	if (trimToZero != 0) {
		x[abs(x)<trimToZero] <- 0
	}
	
	if (skipZero == TRUE) {zero <- "."} else {zero <- "0."}
	
	if (length(dim(x)) == 2) {
		apply(x, 2, function(x2) {gsub("0\\.", zero, sprintf(paste0("%",prepoint,".",digits,"f"), x2))})
	} else {
		gsub("0\\.", zero, sprintf(paste("%.",digits,"f",sep=""), x))
	}
}

# print a number in (latex) bold if it exceeds "bold.cutoff"
f2bold <- function(x, digits=0, bold.cutoff=1) {
	paste0(ifelse(x>=bold.cutoff, "\\textbf{", ""), f2(x, digits), ifelse(x>=bold.cutoff, "}", ""))
}

# print a number x in (latex) bold if another variable y exceeds "bold.cutoff"
f2bold.y <- function(x, y, digits=0, bold.cutoff=0.5) {
	paste0(ifelse(y>=bold.cutoff, "\\textbf{", ""), f2(x, digits), ifelse(y>=bold.cutoff, "}", ""))
}

# format big numbers with a "big mark"
big <- function(x, digits=0, big.mark=",") {	
	if (length(dim(x)) == 2) {
		apply(x, 2, function(x2) {formatC(x2, format="f", digits=digits, big.mark=big.mark)})
	} else {
		formatC(x, format="f", digits=digits, big.mark=big.mark)
	}
}

# nicely formats a p-value
p0 <- function(x, digits=3, latex=FALSE) {
	res <- ""
	if (is.na(x)) res <- "NA"
	if (x >= .1^digits) res <- paste0("p = ", f2(x, digits, skipZero=TRUE))
	if (x <  .1^digits) res <- paste0("p < ", f2(.1^digits, digits, skipZero=TRUE))		
	if (latex==TRUE) res <- gsub("p", "\\emph{p}", res, fixed=TRUE)
	return(res)
}
p <- Vectorize(p0)


# return a result string for single coefficients of a lme4 object
lme4_coef <- function(model, coef_name) {
	res <- paste0(
		"($b$ = ", f2(fixef(model)[coef_name], 2), ", \\emph{SE} = ", f2(sqrt(vcov(model)[coef_name, coef_name]), 2), ", ", p(summary(model)$coefficients[coef_name, "Pr(>|t|)"], latex=TRUE), ")"
	)
	return(res)
}

