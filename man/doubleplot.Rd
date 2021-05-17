\name{doubleplot}
\alias{doubleplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
doubleplot(timestamp, rythm, n_lines, dark_beg, dark_end, cold_beg = rep("00:00:00", n_lines), cold_end = rep("00:00:00", n_lines), bar_width = 300, graph_font = "serif", hot_color = "red3", cold_color = "blue4", x_title = "Time (Zt)", x_title_size = 4.8, x_lables_size = 3.8, y_title = "Day of experiment", y_title_size = 4.8, y_lables_size = 3.8, legend_labels_size = 3.8, graph_title = NULL, graph_title_size = 6, first_day = 1, plot_legend = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timestamp}{
%%     ~~Describe \code{timestamp} here~~
}
  \item{rythm}{
%%     ~~Describe \code{rythm} here~~
}
  \item{n_lines}{
%%     ~~Describe \code{n_lines} here~~
}
  \item{dark_beg}{
%%     ~~Describe \code{dark_beg} here~~
}
  \item{dark_end}{
%%     ~~Describe \code{dark_end} here~~
}
  \item{cold_beg}{
%%     ~~Describe \code{cold_beg} here~~
}
  \item{cold_end}{
%%     ~~Describe \code{cold_end} here~~
}
  \item{bar_width}{
%%     ~~Describe \code{bar_width} here~~
}
  \item{graph_font}{
%%     ~~Describe \code{graph_font} here~~
}
  \item{hot_color}{
%%     ~~Describe \code{hot_color} here~~
}
  \item{cold_color}{
%%     ~~Describe \code{cold_color} here~~
}
  \item{x_title}{
%%     ~~Describe \code{x_title} here~~
}
  \item{x_title_size}{
%%     ~~Describe \code{x_title_size} here~~
}
  \item{x_lables_size}{
%%     ~~Describe \code{x_lables_size} here~~
}
  \item{y_title}{
%%     ~~Describe \code{y_title} here~~
}
  \item{y_title_size}{
%%     ~~Describe \code{y_title_size} here~~
}
  \item{y_lables_size}{
%%     ~~Describe \code{y_lables_size} here~~
}
  \item{legend_labels_size}{
%%     ~~Describe \code{legend_labels_size} here~~
}
  \item{graph_title}{
%%     ~~Describe \code{graph_title} here~~
}
  \item{graph_title_size}{
%%     ~~Describe \code{graph_title_size} here~~
}
  \item{first_day}{
%%     ~~Describe \code{first_day} here~~
}
  \item{plot_legend}{
%%     ~~Describe \code{plot_legend} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (timestamp, rythm, n_lines, dark_beg, dark_end, cold_beg = rep("00:00:00", 
    n_lines), cold_end = rep("00:00:00", n_lines), bar_width = 300, 
    graph_font = "serif", hot_color = "red3", cold_color = "blue4", 
    x_title = "Time (Zt)", x_title_size = 4.8, x_lables_size = 3.8, 
    y_title = "Day of experiment", y_title_size = 4.8, y_lables_size = 3.8, 
    legend_labels_size = 3.8, graph_title = NULL, graph_title_size = 6, 
    first_day = 1, plot_legend = TRUE) 
{
    libraries <- unlist(c("lubridate", "hms", "ggplot2", "gridExtra"))
    required <- unlist(lapply(X = libraries, FUN = require, character.only = TRUE))
    needed <- libraries[required == FALSE]
    if (length(needed) > 0) {
        install.packages(needed)
        lapply(X = needed, FUN = require, character.only = TRUE)
    }
    if (length(timestamp) != length(rythm)) {
        stop("'timestamp' and 'rythm' have different lengths.")
    }
    timestamp <- ymd_hms(timestamp, tz = "")
    if (sum(is.na(timestamp)) > 0) {
        stop("'timestamp' contains out-of-format data.")
    }
    if (sum(rythm < 0) > 0) {
        message("Warning: Be careful, your rythm data has negative value(s). You might want to check.")
    }
    n_lines <- as.integer(n_lines)
    if (n_lines <= 0) {
        stop("'n_lines' must be integer and positive.")
    }
    bar_width <- as.integer(bar_width)
    if (bar_width <= 0) {
        stop("'bar_width' must be integer and positive.")
    }
    if (length(dark_beg) != length(dark_end)) {
        stop("'dark_beg' and 'dark_end' have different lengths.")
    }
    D.beg <- as_hms(dark_beg)
    D.end <- as_hms(dark_end)
    if (sum(is.na(D.beg)) > 0 | sum(is.na(D.end)) > 0) {
        stop("Out-of-format Light-Dark regime(s).")
    }
    if (length(dark_beg) < n_lines & length(dark_end) < n_lines) {
        stop("'dark_beg' and 'dark_end' are shorter than n_lines.")
    }
    if (length(cold_beg) != length(cold_end)) {
        stop("'cold_beg' and 'cold_end' have different lengths.")
    }
    if (missing(cold_beg) == T & missing(cold_end) == T) {
        cold_beg.c <- rep("00:00:00", n_lines)
        cold_end.c <- rep("00:00:00", n_lines)
        C.beg <- as_hms(cold_beg.c)
        C.end <- as_hms(cold_end.c)
    }
    else {
        C.beg <- as_hms(cold_beg)
        C.end <- as_hms(cold_end)
        if (sum(is.na(C.beg)) > 0 | sum(is.na(C.end)) > 0) {
            stop("Out-of-format Hot-Cold regime(s).")
        }
        if (length(cold_beg) < n_lines & length(cold_end) < n_lines) {
            stop("'cold_beg' and 'cold_end' are shorter than n_lines.")
        }
    }
    if (is.numeric(x_title_size) == F | is.numeric(x_lables_size) == 
        F | is.numeric(y_title_size) == F | is.numeric(y_lables_size) == 
        F | is.numeric(legend_labels_size) == F | is.numeric(graph_title_size) == 
        F | is.numeric(first_day) == F) {
        stop("Some number(s) you chose is(are) not numeric.")
    }
    mydata <- data.frame(date(timestamp[1:n_lines]), as_hms(timestamp[1:n_lines]), 
        rythm[1:n_lines], D.beg[1:n_lines], D.end[1:n_lines], 
        C.beg[1:n_lines], C.end[1:n_lines])
    colnames(mydata) <- c("Date", "Time", "Rythm", "Dark_begin", 
        "Dark_end", "Cold_begin", "Cold_end")
    days_contained <- unique(mydata$Date)
    mydata$Temperature <- rep(NA, nrow(mydata))
    for (a in 1:nrow(mydata)) {
        if (mydata[a, 6] < mydata[a, 7]) {
            if (mydata[a, 2] < mydata[a, 6]) {
                mydata[a, 8] <- paste0("hot")
            }
            if (mydata[a, 2] >= mydata[a, 6] & mydata[a, 2] <= 
                mydata[a, 7]) {
                mydata[a, 8] <- paste0("cold")
            }
            if (mydata[a, 2] > mydata[a, 7]) {
                mydata[a, 8] <- paste0("hot")
            }
        }
        if (mydata[a, 6] > mydata[a, 7]) {
            if (mydata[a, 2] < mydata[a, 7]) {
                mydata[a, 8] <- paste0("cold")
            }
            if (mydata[a, 2] >= mydata[a, 7] & mydata[a, 2] <= 
                mydata[a, 6]) {
                mydata[a, 8] <- paste0("hot")
            }
            if (mydata[a, 2] > mydata[a, 6]) {
                mydata[a, 8] <- paste0("cold")
            }
        }
        if (mydata[a, 6] == mydata[a, 7]) {
            mydata[a, 8] <- paste0("constant")
        }
    }
    mydata$Temperature <- as.factor(mydata$Temperature)
    lines_in_plot <- seq(from = 1, to = (length(days_contained) + 
        1))
    mat_layout <- as.matrix(lines_in_plot)
    xmax = as.numeric(as_hms("23:59:59"))
    ymax = max(mydata$Rythm)
    ymax = as.numeric(ymax)
    plots2 <- rep(NA, (length(days_contained) + 1))
    plots2 <- as.list(plots2)
    plots2[[(length(days_contained) + 1)]] <- ggplot() + theme_void()
    for (d in 1:(length(days_contained))) {
        assign(paste0("df", d), mydata[mydata$Date == days_contained[d], 
            ])
        if (get(paste0("df", d))[1, 4] < get(paste0("df", d))[1, 
            5]) {
            plots2[[d]] <- ggplot(get(paste0("df", d)), aes(x = Time, 
                y = Rythm)) + geom_rect(aes(xmin = (Dark_begin[1] - 
                500), xmax = (Dark_end[1] + 500), ymin = -Inf, 
                ymax = Inf), fill = "grey", alpha = 0.5) + geom_bar(aes(fill = Temperature), 
                stat = "identity", width = bar_width) + scale_fill_manual(values = c(hot = hot_color, 
                cold = cold_color, constant = "black")) + theme_void() + 
                geom_hline(yintercept = 0, linetype = "solid", 
                  color = "black", size = 0.5) + theme(plot.margin = unit(c(0.9, 
                0, 0, 0), "pt")) + theme(axis.title = element_blank()) + 
                theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + 
                theme(legend.position = "none") + scale_x_continuous(limits = c(-500, 
                (xmax + 500)), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 
                ymax), expand = c(0, 0))
        }
        if (get(paste0("df", d))[1, 4] > get(paste0("df", d))[1, 
            5]) {
            plots2[[d]] <- ggplot(get(paste0("df", d)), aes(x = Time, 
                y = Rythm)) + geom_rect(aes(xmin = -Inf, xmax = Dark_end[1], 
                ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5) + 
                geom_rect(aes(xmin = Dark_begin[1], xmax = Inf, 
                  ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5) + 
                geom_bar(aes(fill = Temperature), stat = "identity", 
                  width = bar_width) + scale_fill_manual(values = c(hot = hot_color, 
                cold = cold_color, constant = "black")) + theme_void() + 
                geom_hline(yintercept = 0, linetype = "solid", 
                  color = "black", size = 0.5) + theme(plot.margin = unit(c(0.9, 
                0, 0, 0), "pt")) + theme(axis.title = element_blank()) + 
                theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + 
                theme(legend.position = "none") + scale_x_continuous(limits = c(-500, 
                (xmax + 500)), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 
                ymax), expand = c(0, 0))
        }
        if (get(paste0("df", d))[1, 4] == get(paste0("df", d))[1, 
            5]) {
            plots2[[d]] <- ggplot(get(paste0("df", d)), aes(x = Time, 
                y = Rythm)) + geom_bar(aes(fill = Temperature), 
                stat = "identity", width = bar_width) + scale_fill_manual(values = c(hot = hot_color, 
                cold = cold_color, constant = "black")) + theme_void() + 
                geom_hline(yintercept = 0, linetype = "solid", 
                  color = "black", size = 0.5) + theme(plot.margin = unit(c(0.9, 
                0, 0, 0), "pt")) + theme(axis.title = element_blank()) + 
                theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + 
                theme(legend.position = "none") + scale_x_continuous(limits = c(-500, 
                (xmax + 500)), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 
                ymax), expand = c(0, 0))
        }
    }
    plots1 <- rep(NA, (length(days_contained) + 1))
    plots1 <- as.list(plots1)
    plots1[[1]] <- ggplot() + theme_void()
    for (d in 2:(length(days_contained) + 1)) {
        plots1[[d]] <- plots2[[(d - 1)]]
    }
    x1 <- rep(1, (length(days_contained) + 2))
    y1 <- rev(seq(0, (ymax * (length(days_contained) + 1)), by = ymax))
    scale1 <- data.frame(x1, y1)
    scale_days <- ggplot(scale1, aes(x = x1, y = y1)) + geom_segment(x = 0.75, 
        xend = 1, y = y1, yend = y1, linetype = "solid", color = "black", 
        size = 0.4) + geom_vline(xintercept = 0.75, linetype = "solid", 
        color = "black", size = 0.5) + theme_void() + theme(plot.margin = unit(c(0, 
        5, 0, 7), "pt")) + theme(axis.title = element_blank()) + 
        theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + 
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + 
        scale_y_continuous(limits = c(0, (ymax * (length(days_contained) + 
            1))), expand = c(0, 0)) + annotate("text", x = 0.15, 
        y = ((ymax * (length(days_contained) + 1))/2), label = y_title, 
        angle = 90, size = y_title_size, family = graph_font)
    for (d in (length(days_contained):0)) {
        scale_days <- scale_days + annotate("text", x = 0.55, 
            y = (((d) * ymax) + (ymax/2)), label = paste0((length(days_contained) + 
                (first_day - 1)) - d), size = y_lables_size, 
            family = graph_font)
    }
    x2 <- seq(0, 48)
    y2 <- rep(1, 49)
    scale2 <- data.frame(x2, y2)
    scale_hours <- ggplot(scale2, aes(x = x2, y = y2)) + geom_segment(x = x2, 
        xend = x2, y = 0.7, yend = 0.85, linetype = "solid", 
        color = "black", size = 0.4) + geom_segment(x = 0, xend = 0, 
        y = 0.7, yend = 1, linetype = "solid", color = "black", 
        size = 0.4) + geom_segment(x = 24, xend = 24, y = 0.7, 
        yend = 1, linetype = "solid", color = "black", size = 0.4) + 
        geom_segment(x = 48, xend = 48, y = 0.7, yend = 1, linetype = "solid", 
            color = "black", size = 0.4) + geom_hline(yintercept = 0.7, 
        linetype = "solid", color = "black", size = 0.5) + theme_void() + 
        theme(plot.margin = unit(c(5, 0, 0, 0), "pt")) + theme(axis.title = element_blank()) + 
        theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + 
        scale_x_continuous(limits = c(0, 48), expand = c(0, 0)) + 
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + 
        annotate("text", x = (length(x2)/2), y = 0.2, label = x_title, 
            size = x_title_size, family = graph_font) + annotate("text", 
        x = 0.3, y = 0.55, label = "0", size = x_lables_size, 
        family = graph_font) + annotate("text", x = 47.6, y = 0.55, 
        label = "48", size = x_lables_size, family = graph_font)
    for (d in (c(12, 24, 36))) {
        scale_hours <- scale_hours + annotate("text", x = (d), 
            y = 0.55, label = paste0(d), size = x_lables_size, 
            family = graph_font)
    }
    x3 <- seq(0, 48)
    y3 <- rep(1, 49)
    scale3 <- data.frame(x3, y3)
    legend <- ggplot(scale3, aes(x = x3, y = y3)) + theme_void() + 
        theme(plot.margin = unit(c(0, 0, 5, 0), "pt")) + theme(axis.title = element_blank()) + 
        theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + 
        scale_x_continuous(limits = c(0, 48), expand = c(0, 0)) + 
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + 
        annotate("text", x = 6, y = 0.8, label = "Photoperiodic Regime:", 
            size = legend_labels_size, family = graph_font)
    constant_light <- mydata[, 4] == mydata[, 5]
    if (sum(constant_light) == nrow(mydata)) {
        legend <- legend + geom_rect(aes(xmin = 2.5, xmax = 4, 
            ymin = 0.45, ymax = 0.65), colour = "black", size = 0.4, 
            fill = "white") + annotate("text", x = 9, y = 0.55, 
            label = "Constant light (L:L)", size = legend_labels_size, 
            family = graph_font)
    }
    constant_dark <- mydata[, 4] == as_hms("00:00:00") & mydata[, 
        5] == as_hms("23:59:59")
    if (sum(constant_dark) == nrow(mydata)) {
        legend <- legend + geom_rect(aes(xmin = 2.5, xmax = 4, 
            ymin = 0.45, ymax = 0.65), colour = "black", size = 0.4, 
            fill = "grey") + annotate("text", x = 9.1, y = 0.55, 
            label = "Constant dark (D:D)", size = legend_labels_size, 
            family = graph_font)
    }
    if (sum(constant_light) == 0 & sum(constant_dark) == 0) {
        legend <- legend + geom_rect(aes(xmin = 2.5, xmax = 4, 
            ymin = 0.45, ymax = 0.65), colour = "black", size = 0.4, 
            fill = "white", alpha = 1) + geom_rect(aes(xmin = 2.5, 
            xmax = 3.25, ymin = 0.45, ymax = 0.65), colour = "black", 
            size = 0, fill = "white") + geom_rect(aes(xmin = 3.25, 
            xmax = 4, ymin = 0.45, ymax = 0.65), colour = "black", 
            size = 0, fill = "grey") + annotate("text", x = 8.6, 
            y = 0.55, label = "Light-Dark (L:D)", size = legend_labels_size, 
            family = graph_font)
    }
    if (sum(constant_light) > 0 & sum(constant_dark) == (nrow(mydata) - 
        sum(constant_light))) {
        legend <- legend + geom_rect(aes(xmin = 2.5, xmax = 4, 
            ymin = 0.45, ymax = 0.65), colour = "black", size = 0.4, 
            fill = "white") + annotate("text", x = 9, y = 0.55, 
            label = "Constant light (L:L)", size = legend_labels_size, 
            family = graph_font) + geom_rect(aes(xmin = 2.5, 
            xmax = 4, ymin = 0.25, ymax = 0.45), colour = "black", 
            size = 0.4, fill = "grey") + annotate("text", x = 9.1, 
            y = 0.35, label = "Constant dark (D:D)", size = legend_labels_size, 
            family = graph_font)
    }
    if (sum(constant_light) > 0 & sum(constant_light) < nrow(mydata) & 
        sum(constant_dark) == 0) {
        legend <- legend + geom_rect(aes(xmin = 2.5, xmax = 4, 
            ymin = 0.45, ymax = 0.65), colour = "black", size = 0.4, 
            fill = "white", alpha = 1) + geom_rect(aes(xmin = 2.5, 
            xmax = 3.25, ymin = 0.45, ymax = 0.65), colour = "black", 
            size = 0, fill = "white") + geom_rect(aes(xmin = 3.25, 
            xmax = 4, ymin = 0.45, ymax = 0.65), colour = "black", 
            size = 0, fill = "grey") + annotate("text", x = 8.6, 
            y = 0.55, label = "Light-Dark (L:D)", size = legend_labels_size, 
            family = graph_font) + geom_rect(aes(xmin = 2.5, 
            xmax = 4, ymin = 0.25, ymax = 0.45), colour = "black", 
            size = 0.4, fill = "white") + annotate("text", x = 9, 
            y = 0.35, label = "Constant light (L:L)", size = legend_labels_size, 
            family = graph_font)
    }
    if (sum(constant_dark) > 0 & sum(constant_dark) < nrow(mydata) & 
        sum(constant_light) == 0) {
        legend <- legend + geom_rect(aes(xmin = 2.5, xmax = 4, 
            ymin = 0.45, ymax = 0.65), colour = "black", size = 0.4, 
            fill = "white", alpha = 1) + geom_rect(aes(xmin = 2.5, 
            xmax = 3.25, ymin = 0.45, ymax = 0.65), colour = "black", 
            size = 0, fill = "white") + geom_rect(aes(xmin = 3.25, 
            xmax = 4, ymin = 0.45, ymax = 0.65), colour = "black", 
            size = 0, fill = "grey") + annotate("text", x = 8.6, 
            y = 0.55, label = "Light-Dark (L:D)", size = legend_labels_size, 
            family = graph_font) + geom_rect(aes(xmin = 2.5, 
            xmax = 4, ymin = 0.25, ymax = 0.45), colour = "black", 
            size = 0.4, fill = "grey") + annotate("text", x = 9.1, 
            y = 0.35, label = "Constant dark (D:D)", size = legend_labels_size, 
            family = graph_font)
    }
    if (sum(constant_light) > 0 & sum(constant_dark) > 0 & sum(c(constant_light, 
        constant_dark)) < nrow(mydata)) {
        legend <- legend + geom_rect(aes(xmin = 2.5, xmax = 4, 
            ymin = 0.45, ymax = 0.65), colour = "black", size = 0.4, 
            fill = "white", alpha = 1) + geom_rect(aes(xmin = 2.5, 
            xmax = 3.25, ymin = 0.45, ymax = 0.65), colour = "black", 
            size = 0, fill = "white") + geom_rect(aes(xmin = 3.25, 
            xmax = 4, ymin = 0.45, ymax = 0.65), colour = "black", 
            size = 0, fill = "grey") + annotate("text", x = 8.6, 
            y = 0.55, label = "Light-Dark (L:D)", size = legend_labels_size, 
            family = graph_font) + geom_rect(aes(xmin = 2.5, 
            xmax = 4, ymin = 0.25, ymax = 0.45), colour = "black", 
            size = 0.4, fill = "white") + annotate("text", x = 9, 
            y = 0.35, label = "Constant light (L:L)", size = legend_labels_size, 
            family = graph_font) + geom_rect(aes(xmin = 2.5, 
            xmax = 4, ymin = 0.05, ymax = 0.25), colour = "black", 
            size = 0.4, fill = "grey") + annotate("text", x = 9.1, 
            y = 0.15, label = "Constant dark (D:D)", size = legend_labels_size, 
            family = graph_font)
    }
    if (missing(cold_beg) == F & missing(cold_end) == F) {
        legend <- legend + annotate("text", x = 30, y = 0.8, 
            label = "Temperature Regime:", size = legend_labels_size, 
            family = graph_font)
        if (sum(mydata[, 6] != mydata[, 7]) == nrow(mydata)) {
            legend <- legend + geom_rect(aes(xmin = 26.5, xmax = 27.25, 
                ymin = 0.45, ymax = 0.65), colour = "black", 
                size = 0.4, fill = hot_color) + geom_rect(aes(xmin = 27.25, 
                xmax = 28, ymin = 0.45, ymax = 0.65), colour = "black", 
                size = 0.4, fill = cold_color) + annotate("text", 
                x = 33, y = 0.55, label = "Hot-Cold (H:C)", size = legend_labels_size, 
                family = graph_font)
        }
        else {
            legend <- legend + geom_rect(aes(xmin = 26.5, xmax = 27.25, 
                ymin = 0.45, ymax = 0.65), colour = "black", 
                size = 0.4, fill = hot_color) + geom_rect(aes(xmin = 27.25, 
                xmax = 28, ymin = 0.45, ymax = 0.65), colour = "black", 
                size = 0.4, fill = cold_color) + annotate("text", 
                x = 33, y = 0.55, label = "Hot-Cold (H:C)", size = legend_labels_size, 
                family = graph_font) + geom_rect(aes(xmin = 26.5, 
                xmax = 28, ymin = 0.25, ymax = 0.45), colour = "black", 
                fill = "black") + annotate("text", x = 34, y = 0.35, 
                label = "Constant temperature", size = legend_labels_size, 
                family = graph_font)
        }
    }
    acto1 <- arrangeGrob(grobs = plots1, layout_matrix = mat_layout)
    acto2 <- arrangeGrob(grobs = plots2, layout_matrix = mat_layout)
    title <- ggplot(scale3, aes(x = x3, y = y3)) + theme_void() + 
        theme(plot.margin = unit(c(0, 0, 3, 0), "pt")) + theme(axis.title = element_blank()) + 
        theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + 
        scale_x_continuous(limits = c(0, 48), expand = c(0, 0)) + 
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + 
        annotate("text", x = 24, y = 0.5, label = graph_title, 
            size = graph_title_size, family = graph_font)
    if (missing(graph_title) == F & plot_legend == TRUE) {
        grid.arrange(title, scale_days, acto1, acto2, scale_hours, 
            legend, widths = c(2, 10, 10), heights = c(1.5, 10, 
                1.5, 1.5), layout_matrix = cbind(c(NA, 2, NA, 
                NA), c(1, 3, 5, 6), c(1, 4, 5, 6)), right = "")
    }
    if (missing(graph_title) == T & plot_legend == TRUE) {
        grid.arrange(scale_days, acto1, acto2, scale_hours, legend, 
            widths = c(2, 10, 10), heights = c(10, 1.5, 1.5), 
            layout_matrix = cbind(c(1, NA, NA), c(2, 4, 5), c(3, 
                4, 5)), top = "", right = "")
    }
    if (missing(graph_title) == F & plot_legend == FALSE) {
        grid.arrange(title, scale_days, acto1, acto2, scale_hours, 
            widths = c(2, 10, 10), heights = c(1.5, 10, 1.5), 
            layout_matrix = cbind(c(NA, 2, NA), c(1, 3, 5), c(1, 
                4, 5)), right = "")
    }
    if (missing(graph_title) == T & plot_legend == FALSE) {
        grid.arrange(scale_days, acto1, acto2, scale_hours, widths = c(2, 
            10, 10), heights = c(10, 1.5), layout_matrix = cbind(c(1, 
            NA), c(2, 4), c(3, 4)), top = "", right = "")
    }
    light <- rep(NA, length(days_contained))
    dark <- rep(NA, length(days_contained))
    for (d in 1:(length(days_contained))) {
        dark[d] <- round(abs((unique(get(paste0("df", (d)))[, 
            5]) - unique(get(paste0("df", (d)))[, 4]))/3600))
        light[d] <- 24 - dark[d]
    }
    Photo_Regime <- data.frame(days_contained, paste0(light, 
        ":", dark))
    colnames(Photo_Regime) <- c("Date", "Light:Dark")
    if (missing(cold_beg) == T & missing(cold_end) == T) {
        final_text <- noquote(capture.output(cat("--- Doubleplot succeeded! ---\n\n", 
            "Actogram for data contained in: ", length(days_contained), 
            " days.\n\n", "No Hot-Cold cycle(s) declared.\n", 
            "You can check the Light-Dark Regime(s) in the generated table.\n\n", 
            "--- End ---\n")))
        View(Photo_Regime)
    }
    else {
        hot <- rep(NA, length(days_contained))
        cold <- rep(NA, length(days_contained))
        for (d in 1:(length(days_contained))) {
            cold[d] <- round(abs((unique(get(paste0("df", (d)))[, 
                7]) - unique(get(paste0("df", (d)))[, 6]))/3600))
            hot[d] <- 24 - cold[d]
        }
        Photo_Temperature_Regime <- data.frame(days_contained, 
            paste0(light, ":", dark), paste0(hot, ":", cold))
        colnames(Photo_Temperature_Regime) <- c("Date", "Light:Dark", 
            "Hot:Cold")
        final_text <- noquote(capture.output(cat("--- Doubleplot succeeded! ---\n\n", 
            "Actogram for data contained in: ", length(days_contained), 
            " days.\n\n", "You can check the Light-Dark and Hot-Cold Regimes in the generated table.\n\n", 
            "--- End ---\n")))
        View(Photo_Temperature_Regime)
    }
    return(final_text)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
