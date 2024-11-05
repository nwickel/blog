#' ---
#' title: "Understanding categorical predictors in a mixed-effects model"
#' author: "Nora Wickelmaier"
#' date: "`r Sys.Date()`"
#' categories:
#'   - R
#'   - Categorical predictors
#'   - Mixed Models
#' output:
#'   md_document:
#'     variant: gfm
#'     preserve_yaml: true
#' ---

#+ include = FALSE
# rmarkdown::render("design.R", output_options = list(code_download=TRUE))
# rmarkdown::render("design.R", output_format = "github_document", output_dir = "../_posts")
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.align = "center", fig.path = paste0("../figures/", Sys.Date(), "_"))


#'
#' This post is meant to give you a better understanding how to interpret
#' parameters for categorical predictors in a mixed-effects model. Understanding
#' what the parameters mean is helpful when simulating data, e.g., for power
#' simulations.

#+

#' # Example
#'
#' This is an example where three different media are compared to each other: A
#' Multi-Touch Table (MTT) and two different Virtual Reality conditions (VR1 and
#' VR2). These are all presented to every subject (within-factor `mediatype`).
#' Additionally, we have a between-factor `interaction` with levels `passive`
#' and `active`. Subjects can either actively interact for all media types or
#' they can just passively look at things presented to them with the three
#' different media types. The dependent varible is immersion. Subjects should
#' feel more immersed when actively interacting and Virtual Reality should also
#' lead to more immersion than a Multi-Touch Table.

#+ data, fig.height = 5, fig.width = 6, fig.show = "hide"
library(lme4)

# Data simulation for a 2 (between) x 3 (within) design
set.seed(1046)
n <- 100
dat <- data.frame(id = factor(rep(1:n, each = 3)),
                  mediatype = factor(c("MTT", "VR1", "VR2")),
                  interaction = factor(rep(c("active", "passive"), each = 3),
                                       levels = c("passive", "active"))
                  )

## Set parameters
beta  <- c(.5, .2, .3, .4, .7, .4)
sigma <- .8
spi   <- .5
theta <- spi / sigma

## Simulate data
dat$y <- simulate(~ 1 + mediatype + interaction + mediatype:interaction +
                  (1 | id),
                  newparam = list(beta = beta, theta = theta, sigma = sigma),
                  newdata = dat)$sim_1

## Visualize simulated data
datm <- aggregate(y ~ mediatype + interaction, data = dat, FUN = mean)

colors <- c("#3CB4DC", "#91C86E")

par(mai = c(.8, .8, .1, .1), mgp = c(2.4, 1, 0))
plot(y ~ I(1:3), datm[datm$interaction == "passive", ],
     xlab = "Media type",
     ylab = "Immersion",
     xlim = c(0.5, 3.5),
     ylim = c(-2, 4),
     axes = FALSE,
     type = "n")
axis(1, at = 1:3, labels = c("MTT", "VR1", "VR2"))
axis(2)
box()
points(y ~ jitter(as.numeric(mediatype)), dat, col = colors[dat$interaction])
points(y ~ I(1:3), datm[datm$interaction == "passive", ], col = colors[1], type = "b", pch = 16, cex = 2)
points(y ~ I(1:3), datm[datm$interaction == "active", ], col = colors[2], type = "b", pch = 16, cex = 2)
text(1.45, 0.4, "passive", col = colors[1])
text(1.4, 1.6, "active", col = colors[2])

#+ echo = FALSE
knitr::include_graphics(paste0("https://raw.githubusercontent.com/nwickel/blog/refs/heads/main/figures/", Sys.Date(), "_data-1.png"))


#' # Linear mixed-effects model
#'
#' The following model will be fitted to the data.
#' $$y = \beta_0 +
#'        \beta_1 mediatype_{VR1} +
#'        \beta_2 mediatype_{VR2} +
#'        \beta_3 interaction_{active} +
#'        \beta_4 (mediatype_{VR1} \times interaction_{active}) +
#'        \beta_5 (mediatype_{VR2} \times interaction_{active}) +
#'        \upsilon_0 +
#'        \varepsilon$$
#' with $$\upsilon_0 \sim N(0, \sigma^2_{\upsilon_0})$$ and 
#' $$\varepsilon \sim N(0, \sigma^2_{\varepsilon})$$


## Fit linear mixed-effects model
m1 <- lmer(y ~ 1 + mediatype + interaction + mediatype:interaction + (1| id),
           data = dat)

tab <- summary(m1)$coef
ci  <- confint(m1)
knitr::kable(round(cbind(tab, ci[3:8, ]), 3))


#' # Implied model formulae
#'
#' The two categorical predictors are dummy coded. For `interaction` the level
#' `passive` is set as reference category. The dummy variable
#' $$interaction_{active}$$ is therefore 0 for `passive` and 1 for `active`.

contrasts(dat$interaction)

#'
#' The three level factor `mediatype` is represented by two dummy variables,
#' namely $$mediatype_{VR1}$$ and $$mediatype_{VR2}$$. Since `MTT` is the
#' reference category, the coding scheme is as folows.

contrasts(dat$mediatype)

#'
#' When we plug these dummy variables into the model formula from above, we get
#' a model formula for each data point of the aggregated data set.
#'
#' $$\begin{align*}
#'    y_{11} & = \beta_0 +                               \upsilon_0 + \varepsilon & \text{mean for both reference categories (MTT and passive)}\\
#'    y_{12} & = \beta_0 + \beta_1 +                     \upsilon_0 + \varepsilon & \text{effect when going from MTT to VR1 for passive}\\
#'    y_{13} & = \beta_0 + \beta_2 +                     \upsilon_0 + \varepsilon & \text{effect when going from MTT to VR2 for passive}\\
#'    y_{21} & = \beta_0 + \beta_3 +                     \upsilon_0 + \varepsilon & \text{effect when goinf from passive to active for MTT}\\
#'    y_{22} & = \beta_0 + \beta_1 + \beta_3 + \beta_4 + \upsilon_0 + \varepsilon & \text{effect when going from MTT to VR1 for active}\\
#'    y_{23} & = \beta_0 + \beta_2 + \beta_3 + \beta_5 + \upsilon_0 + \varepsilon & \text{effect when going from MTT to VR2 for active}\\
#' \end{align*}$$
#'

#' # Compare to plot

#+ plot, echo = FALSE, fig.height = 5, fig.width = 6, fig.show = "hide"
# datm$pred <- predict(m1,
#         newdata = data.frame(id = 1,
#                              mediatype = levels(dat$mediatype),
#                              interaction = rep(levels(dat$interaction), each = 3)
#                              ),
#         re.form = NA)

datm$pred <- c(fixef(m1)[1],
               fixef(m1)[1] + fixef(m1)[2],
               fixef(m1)[1] + fixef(m1)[3],
               fixef(m1)[1] + fixef(m1)[4],
               fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[4] + fixef(m1)[5],
               fixef(m1)[1] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[6])

colors <- c("lightblue", "lightgray")

par(mai = c(.8, .8, .1, .1), mgp = c(2.4, 1, 0))
plot(y ~ I(1:3), datm[datm$interaction == "passive", ],
     xlab = "Media type",
     ylab = "Immersion",
     xlim = c(0.5, 3.5),
     ylim = c(-2, 4),
     axes = FALSE,
     type = "n")

axis(1, at = 1:3, labels = c("MTT", "VR1", "VR2"))
axis(2)
box()

points(y ~ jitter(as.numeric(mediatype)), dat, col = colors[dat$interaction])

points(y ~ I(1:3), datm[datm$interaction == "passive", ], col = colors[1], type = "b", pch = 16, cex = 2)
points(y ~ I(1:3), datm[datm$interaction == "active", ], col = colors[2], type = "b", pch = 16, cex = 2)

text(1.45, 0.4, "passive", col = colors[1])
text(1.4, 1.6, "active", col = colors[2])

text(1, datm$y[datm$interaction == "passive" & datm$mediatype == "MTT"],
     expression(beta[0]))
text(2, datm$y[datm$interaction == "passive" & datm$mediatype == "VR1"],
     expression(beta[0] + beta[1]))
text(3, datm$y[datm$interaction == "passive" & datm$mediatype == "VR2"],
     expression(beta[0] + beta[2]))
text(1, datm$y[datm$interaction == "active" & datm$mediatype == "MTT"],
     expression(beta[0] + beta[3]))
text(2, datm$y[datm$interaction == "active" & datm$mediatype == "VR1"],
     expression(beta[0] + beta[1] + beta[3] + beta[4]))
text(3, datm$y[datm$interaction == "active" & datm$mediatype == "VR2"],
     expression(beta[0] + beta[2] + beta[3] + beta[5]))

#+ echo = FALSE
knitr::include_graphics(paste0("https://raw.githubusercontent.com/nwickel/blog/refs/heads/main/figures/", Sys.Date(), "_plot-1.png"))

#' # Session info

sessionInfo()

