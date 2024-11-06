#' ---
#' title: "Effect vs. dummy coding"
#' author: "Nora Wickelmaier"
#' date: "2024-11-06"
#' categories:
#'   - R
#'   - Categorical predictors
#' output:
#'   md_document:
#'     variant: gfm
#'     preserve_yaml: true
#' ---

#+ include = FALSE
date <- "2024-11-06"
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.align = "center", fig.path = paste0("../figures/", date, "_"))

set.seed(1101)

#'
#' Let us look at the differences between effect and dummy coding for regression
#' models with categorical predictors.
#'

#' # Model with one categorical factor
#'
#' Let us simulate some data first, for $$n = 10$$ subjects and a group
#' predictor with levels `ctr` for the control group and `trt` for the treatment
#' group.

dat <- data.frame(id    = factor(1:10),
                  group = factor(c("ctr", "trt")),
                  resp  = c(rnorm(5, 1), rnorm(5)))

aggregate(resp ~ group, data = dat, FUN = mean)

#'
#' Per default are uses so-called dummy coding to represent factors. This means
#' that the reference category (here `ctr`) is set to 0 and the second category
#' is set to 1. R takes the first factor level as reference category in dummy
#' coding.

contrasts(dat$group)

#'
#' You can check the default settings of your R session with

options()$contrasts

#'
#' The first entry `"contr.treatment"` indicates dummy coding.
#'

#' ## Linear model with dummy coding
#'
#' Let us now fit a simple linear regression
#'
#' $$resp = \beta_0 + \beta_1 group + \varepsilon$$
#'
#' with $$\varepsilon \sim N(0,\sigma_{\varepsilon})$$.
#'
#' (For a two-level between factor, this is equivalent to an independent $$t$$
#' test.)

lm1 <- lm(resp ~ group, data = dat)
summary(lm1)

coef(lm1)[1]    # mean ctr
sum(coef(lm1))  # mean trt

#'
#' The interpretation of the parameters is straightforward: The Intercept
#' $$\beta_0$$ is the mean response in the control (here the reference) group
#' ($$\bar x_{ctr} = \beta_0$$) and $$\beta_1$$ is the difference between the
#' control and the treatment group ($$\bar x_{trt} = \beta_0 + \beta_1$$).
#'

#' ## Linear model with effect coding
#'
#' Next, we will fit the same model with effect coding. Hereby, the reference
#' group is coded as $$-1$$.

contrasts(dat$group) <- "contr.sum"
contrasts(dat$group)

#'
#' You can see that for effect coding, R takes the _last_ factor level as
#' reference category.
#'
#' Now, let us refit the model.

lm2 <- lm(resp ~ group, dat)
summary(lm2)

sum(coef(lm2))    # mean ctr
-diff(coef(lm2))  # mean trt

#'
#' In this model, the Intercept $$\beta_0$$ is now the overall mean (often
#' called the grand mean).

mean(dat$resp)

#'
#' And $$\beta_1$$ is the difference of the mean in group 1 (here the control
#' group) to the overall mean and its negative value is the difference of group
#' 1 (treatment group) to the overall mean.
#'

#' # Model with two categorical factors
#'
#' Let us again simulate some data first. This time for $$n = 20$$ subjects, a
#' group predictor like before and an additional factor `study` with two levels
#' `s1` and `s2`.

dat <- data.frame(id    = factor(1:20),
                  group = factor(rep(c("ctr", "trt"), each = 10)),
                  study = factor(rep(c("s1", "s2"), each = 5)),
                  resp  = c(rnorm(10, 1), rnorm(10)))

aggregate(resp ~ group + study, data = dat, FUN = mean)

mean(dat$resp)

#+ interactionplot, fig.width = 3.375, fig.height = 3.375, fig.show = "hide"
par(mai = c(.8, .8, .1, .1), mgp = c(2.4, 1, 0))
with(dat, interaction.plot(group, study, resp))

#+ echo = FALSE
knitr::include_graphics(paste0("https://raw.githubusercontent.com/nwickel/blog/refs/heads/main/figures/", date, "_interactionplot-1.png"))

#'
#' Both factors are now dummy coded, since we did not tell R to do otherwise.

contrasts(dat$group)
contrasts(dat$study)

#' ## Linear model with dummy coding
#'
#' Let us now fit regression model with an interaction term
#'
#' $$resp = \beta_0 + \beta_1 group + \beta_2 study + \beta_3 (group \times
#' study) + \varepsilon$$
#'
#' with $$\varepsilon \sim N(0,\sigma_{\varepsilon})$$.
#'
#' (This is equivalent to a 2x2 ANOVA with two between factors.)

lm3 <- lm(resp ~ group * study, data = dat)
summary(lm3)

#'
#' We can now reconstruct the mean values for all four groups with our
#' parameters. The Intercept $$\beta_0$$ is now the mean response in study 1 for
#' the control group.

coef(lm3)[1]
coef(lm3)[1] + coef(lm3)[2]
coef(lm3)[1] + coef(lm3)[3]
coef(lm3)[1] + coef(lm3)[2] + coef(lm3)[3] + coef(lm3)[4]   # sum(coef(lm3))

#'
#' With dummy coding, we can think of the dummy variables as "controlling" which
#' parameters are entered into the mean calculations and which are not. With
#' only categorical predictors, we get a separate equation for each of the four
#' groups.
#'
#' $$\begin{align*}
#'     resp_{11} & = \beta_0 + \varepsilon                      & mean s1 and crt \\
#'     resp_{12} & = \beta_0 + \beta_1 + \varepsilon            & mean s1 and trt \\
#'     resp_{21} & = \beta_0 + \beta_2 + \varepsilon            & mean s2 and crt \\
#'     resp_{22} & = \beta_0 + \beta_1 + \beta_2 + \beta_3 + \varepsilon  & mean s2 und trt
#' $$\end{align*}

#+ dummyplot, fig.width = 3.375, fig.height = 3.375, fig.show = "hide"
colors <- c("lightgray", "darkgray")

datm <- aggregate(resp ~ group + study, data = dat, FUN = mean)

par(mai = c(.8, .8, .1, .1), mgp = c(2.4, 1, 0))
plot(resp ~ jitter(as.numeric(group), .5), data = dat,
     axes = FALSE,
     col = colors[study],
     xlim = c(.7, 2.3),
     xlab = "Group",
     ylab = "Response")
points(resp ~ c(.96, 1.96), data = datm[datm$study == "s1",],
       col = colors[study],
       cex = 3,
       type = "b",
       pch = 16)
points(resp ~ c(1.1, 2.1), data = datm[datm$study == "s2",],
       col = colors[study],
       cex = 3,
       type = "b",
       pch = 16)

axis(1, at = 1:2, labels = c("ctr", "trt"))
axis(2)
box()

legend("topright", c("Study 1", "Study 2"), col = colors, pch = 16, bty = "n", pt.cex = 2)

text(.96,  datm$resp[datm$group == "ctr" & datm$study == "s1"],
     expression(beta[0]))
text(1.96, datm$resp[datm$group == "trt" & datm$study == "s1"],
     expression(beta[0] + beta[1]))
text(1.1,  datm$resp[datm$group == "ctr" & datm$study == "s2"],
     expression(beta[0] + beta[2]))
text(2.1,  datm$resp[datm$group == "trt" & datm$study == "s2"],
     expression(beta[0] + beta[1] + beta[2] + beta[3]))

knitr::include_graphics(paste0("https://raw.githubusercontent.com/nwickel/blog/refs/heads/main/figures/", date, "_dummyplot-1.png"))

#' ## Linear model with dummy coding

contrasts(dat$group) <- "contr.sum"
contrasts(dat$study) <- "contr.sum"

lm4 <- lm(resp ~ group * study, dat)
summary(lm4)

coef(lm4)[1] + coef(lm4)[2] + coef(lm4)[3] + coef(lm4)[4]
coef(lm4)[1] - coef(lm4)[2] + coef(lm4)[3] - coef(lm4)[4]
coef(lm4)[1] + coef(lm4)[2] - coef(lm4)[3] - coef(lm4)[4]
coef(lm4)[1] - coef(lm4)[2] - coef(lm4)[3] + coef(lm4)[4]   # sum(coef(lm3))

#'
#' With effects coding, all parameters are needed for calculating each of the
#' group means. Only the signs change.
#'
#' $$\begin{align*}
#'     resp_{11} & = \beta_0 + \beta_1 + \beta_2 + \beta_3 + \varepsilon & mean s1 and crt \\
#'     resp_{12} & = \beta_0 - \beta_1 + \beta_2 - \beta_3 + \varepsilon & mean s1 and trt \\
#'     resp_{21} & = \beta_0 + \beta_1 - \beta_2 - \beta_3 + \varepsilon & mean s2 and crt \\
#'     resp_{22} & = \beta_0 - \beta_1 - \beta_2 + \beta_3 + \varepsilon & mean s2 und trt
#' $$\end{align*}
#'

#+ effectplot, fig.width = 3.375, fig.height = 3.375, fig.show = "hide"
par(mai = c(.8, .8, .1, .1), mgp = c(2.4, 1, 0))
plot(resp ~ jitter(as.numeric(group), .5), data = dat,
     axes = FALSE,
     col = colors[study],
     xlim = c(.7, 2.3),
     xlab = "Group",
     ylab = "Response")
points(resp ~ c(.96, 1.96), data = datm[datm$study == "s1",],
       col = colors[study],
       cex = 3,
       type = "b",
       pch = 16)
points(resp ~ c(1.1, 2.1), data = datm[datm$study == "s2",],
       col = colors[study],
       cex = 3,
       type = "b",
       pch = 16)
abline(h = mean(dat$resp), lty = 2)

axis(1, at = 1:2, labels = c("ctr", "trt"))
axis(2)
axis(2, at = coef(lm4)[1], labels = expression(beta[0]), las = 2)
box()

arrows(.9, coef(lm4)[1],
       .9, coef(lm4)[1] + coef(lm4)[2],
       length = 0.05,
       col = "#FF6900")
arrows(.93, coef(lm4)[1] + coef(lm4)[2],
       .93, coef(lm4)[1] + coef(lm4)[2] + coef(lm4)[3],
       length = 0.05,
       col = "#91C86E")
arrows(.96, coef(lm4)[1] + coef(lm4)[2] + coef(lm4)[3],
       .96, coef(lm4)[1] + coef(lm4)[2] + coef(lm4)[3] + coef(lm4)[4],
       length = 0.05,
       col = "#3CB4DC")

arrows(1.05, coef(lm4)[1],
       1.05, coef(lm4)[1] + coef(lm4)[2],
       length = 0.05,
       col = "#FF6900")
arrows(1.075, coef(lm4)[1] + coef(lm4)[2],
       1.075, coef(lm4)[1] + coef(lm4)[2] - coef(lm4)[3],
       length = 0.05,
       col = "#91C86E")
arrows(1.1, coef(lm4)[1] + coef(lm4)[2] - coef(lm4)[3],
       1.1, coef(lm4)[1] + coef(lm4)[2] - coef(lm4)[3] - coef(lm4)[4],
       length = 0.05,
       col = "#3CB4DC")

arrows(1.9, coef(lm4)[1],
       1.9, coef(lm4)[1] - coef(lm4)[2],
       length = 0.05,
       col = "#FF6900")
arrows(1.93, coef(lm4)[1] - coef(lm4)[2],
       1.93, coef(lm4)[1] - coef(lm4)[2] + coef(lm4)[3],
       length = 0.05,
       col = "#91C86E")
arrows(1.96, coef(lm4)[1] - coef(lm4)[2] + coef(lm4)[3],
       1.96, coef(lm4)[1] - coef(lm4)[2] + coef(lm4)[3] - coef(lm4)[4],
       length = 0.05,
       col = "#3CB4DC")

arrows(2.05, coef(lm4)[1],
       2.05, coef(lm4)[1] - coef(lm4)[2],
       length = 0.05,
       col = "#FF6900")
arrows(2.075, coef(lm4)[1] - coef(lm4)[2],
       2.075, coef(lm4)[1] - coef(lm4)[2] - coef(lm4)[3],
       length = 0.05,
       col = "#91C86E")
arrows(2.1, coef(lm4)[1] - coef(lm4)[2] - coef(lm4)[3],
       2.1, coef(lm4)[1] - coef(lm4)[2] - coef(lm4)[3] + coef(lm4)[4],
       length = 0.05,
       col = "#3CB4DC")

legend("center",
       c(expression(beta[1]), expression(beta[2]), expression(beta[3])),
       col = c("#FF6900", "#91C86E", "#3CB4DC"), lty = 1, bty = "n")
legend("topright", c("Study 1", "Study 2"), col = colors, pch = 16, bty = "n", pt.cex = 2)

knitr::include_graphics(paste0("https://raw.githubusercontent.com/nwickel/blog/refs/heads/main/figures/", date, "_effectplot-1.png"))


#' # Setting contrasts globally
#'
#' You can also change the settings for the contrast coding globally in R.


options(contrasts = c("contr.treatment", "contr.poly"))

options()$contrasts

contrasts(factor(1:3))

options(contrasts = c("contr.sum", "contr.poly"))

options()$contrasts

contrasts(factor(1:3))


#' # Session info

sessionInfo()

