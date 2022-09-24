#' Make a plot from an lm item
#'
#'
#' @param binom_model Binomial model from base R
#' @return Forest plot, based on ggplot2
#' @export
yy_plt_forest_binom = function(binom_model){
  coef_mat = as.data.frame(summary(lm_model)$coefficients)
  coef_mat = na.omit(coef_mat)
  conf = na.omit(confint.default(lm_model, level = 0.95))
  coef_mat$lci = conf[,1]
  coef_mat$uci = conf[,2]
  coef_mat$label = row.names(coef_mat)
  coef_mat = subset(coef_mat, label != "(Intercept)")
  ggplot(data=coef_mat, aes(x=label,y=exp(Estimate), fill = `Pr(>|z|)` < 0.05)) +
    geom_errorbar(aes(ymin=exp(lci), ymax=exp(uci)),
                  width=0,                    # Width of the error bars
                  position=position_dodge(.9), color = "grey", size = 1.5) +
    geom_point(shape=21, size = 2.5)+
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("") + ylab("Odds ratio (95% CI)") +
    scale_fill_manual(values = hcl.colors(18, "ag_Sunset"))+
    theme_classic()+
    theme(axis.text.x=element_text(colour="black"),
          axis.text.y=element_text(colour="black"))
}

#' Make a plot from an glm, binomal item
#'
#'
#' @param lm_model Linear model from base R
#' @return Forest plot, based on ggplot2
#' @export
yy_plt_forest_lm = function(lm_model){
  coef_mat = as.data.frame(summary(lm_model)$coefficients)
  coef_mat = na.omit(coef_mat)
  conf = na.omit(confint.default(lm_model, level = 0.95))
  coef_mat$lci = conf[,1]
  coef_mat$uci = conf[,2]
  coef_mat$label = row.names(coef_mat)
  coef_mat = subset(coef_mat, label != "(Intercept)")
  ggplot(data=coef_mat, aes(x=label,y=Estimate, fill = `Pr(>|t|)` < 0.05)) +
    geom_errorbar(aes(ymin=lci, ymax=uci),
                  width=0,                    # Width of the error bars
                  position=position_dodge(.9), color = "grey", size = 1.5) +
    geom_point(shape=21, size = 2.5)+
    #geom_pointrange(aes(fill = `Pr(>|t|)` < 0.05)) +
    geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("") + ylab("Effect (95% CI)") +
    scale_fill_manual(values = hcl.colors(18, "ag_Sunset"))+
    theme_classic()+
    theme(axis.text.x=element_text(colour="black"),
          axis.text.y=element_text(colour="black"))
}
