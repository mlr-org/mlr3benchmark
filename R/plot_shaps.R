library(mlr3)
library(ggplot2)

get_shaps <- function(model, task, data = NULL) {
  if (is.null(data)) {
    data = as.matrix(task$data(cols = task$feature_names))
  }

  p = predict(model, data,
              predcontrib = T, approxcontrib = F)
  p = p[,-ncol(p)]

  shaps = reshape2::melt(p)
  vars = reshape2::melt(data)
  relTruth = reshape2::melt(apply(data, 2, function(x)
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))))
  shap.frame = cbind(shaps, relTruth$value, vars$value)
  colnames(shap.frame) = c("id", "Var", "Shap", "relTruth","Truth")

  return(shap.frame)
}
get_interaction_shaps <- function(model, task, data = NULL) {
  if (is.null(data)) {
    data = as.matrix(task$data(cols = task$feature_names))
  }

  p = predict(model, data,
              predinteraction = TRUE)
  p = p[,colnames(data),colnames(data)]

  p = array(c(as.numeric(p), as.numeric(data)), dim = c(dim(p)[1:2], dim(p)[3] + 1),
            dimnames = list(NULL, colnames(data), c(colnames(data), "Truth")))

  return(p)
}


plot_shap_feature <- function(shaps, feature,
                              type = c("b", "p", "s"),
                              se = TRUE, xlab = feature,
                              smooth_color = "blue", smooth_fill = "lightblue",
                              smooth_lwd = 0.5, smooth_level = 0.95,
                              points_color = "black"){

  shaps = shaps[complete.cases(shaps), ]
  type = match.arg(type)
  if (type == "b") {
    ggplot(shaps %>% subset(Var == feature), aes(x = Truth, y = Shap)) +
      geom_point(color = points_color) + geom_smooth(se = se, fill = smooth_fill, color = smooth_color,
                                                     level = smooth_level) +
      labs(x = xlab, y = "SHAP Value") + theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.line.x = element_line(), axis.line.y = element_line())
  } else if (type == "p") {
    ggplot(shaps %>% subset(Var == feature), aes(x = Truth, y = Shap)) +
      geom_point(color = points_color) +
      labs(x = xlab, y = "SHAP Value") + theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.line.x = element_line(), axis.line.y = element_line())
  } else {
    ggplot(shaps %>% subset(Var == feature), aes(x = Truth, y = Shap)) +
      geom_smooth(se = se, lwd = smooth_lwd, fill = smooth_fill, color = smooth_color,
                  level = smooth_level) +
      labs(x = xlab, y = "SHAP Value") + theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.line.x = element_line(), axis.line.y = element_line())
  }

}

plot_shap_summary <- function(shaps, low = "blue", high = "red",
                              jitter_w = 0, jitter_h = 0.2,
                              legend.h = 25, legend.w = 0.5,
                              show.mean = TRUE, mean.abs = TRUE, mean.round = 4) {

  shaps = shaps[complete.cases(shaps), ]

  p = ggplot(shaps, aes(x = Shap, y = Var, colour = relTruth))  +
    geom_vline(xintercept = 0, colour = "darkgray") +
    geom_point() + geom_jitter(width = jitter_w, height = jitter_h) +
    scale_color_gradient(low = low, high = high,
                         n.breaks = 2L,
                         labels = c("Low", "High"),
                         guide = guide_colorbar(barwidth = legend.w, barheight = legend.h,
                                                title.position = "right",
                                                title = "Feature Value",
                                                title.theme = element_text(angle = 90,
                                                                           hjust = 0.5))) +
    theme_minimal() +
    theme(axis.title.y = element_blank(), panel.grid = element_blank(),
          axis.line.x = element_line(), axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 11),
          axis.text.x = element_text(size = 10)) +
    labs(x = "SHAP Value")

  if (show.mean) {
    if (mean.abs) {
      shapmean = aggregate(Shap ~ Var, data = shaps, function(x) mean(abs(x)))[,2]
    } else {
      shapmean = aggregate(Shap ~ Var, data = shaps, mean)[,2]
    }

    p = p + geom_text(aes(x = x, y = y, colour = NULL),
                      data = data.frame(x = max(shaps$Shap) + 0.5, y = 1:length(shapmean)),
                      label = round(shapmean, mean.round))
  }

  p
}

plot_shap_importance <- function(shaps, relative = TRUE, bar_width = 0.4,
                                 low = "blue", high = "red",
                                 legend.position = "none",
                                 legend.w = 0.5, legend.h = 10,
                                 legend_limit = NULL, xlim = NULL,
                                 show.mean = TRUE, mean.round = 4) {

  shaps = shaps[complete.cases(shaps), ]

  agg = cbind(aggregate(Shap ~ Var, data = shaps, function(x) mean(abs(x))),
              Mean = aggregate(Shap ~ Var, data = shaps, mean)[,2])

  if (relative) {
    agg$Shap = (agg$Shap/sum(agg$Shap))*100
  }

  if (is.null(legend_limit)) {
    legend_limit = range(agg$Mean)
  }

  if (is.null(xlim)) {
    xlim = c(0, (max(agg$Shap) - (max(agg$Shap) %% 10)) + 10)
  }

  agg = agg[order(agg$Shap, decreasing = FALSE), ]
  agg$Var = factor(agg$Var, levels = agg$Var)

  p = ggplot(agg,
             aes(x = Shap, y = Var, fill = Mean)) +
    geom_bar(stat = "identity", width = bar_width) +
    scale_fill_gradient(low = low, high = high, n.breaks = 3,
                        limits = legend_limit,
                        guide = guide_colorbar(barwidth = legend.w, barheight = legend.h,
                                               title.position = "right",
                                               title = "Mean SHAP Value",
                                               title.theme = element_text(angle = 90,
                                                                          hjust = 0.5))) +
    theme_minimal() +
    theme(axis.title.y = element_blank(), panel.grid = element_blank(),
          axis.line.y = element_line(), axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 11),
          axis.text.x = element_text(size = 10),
          axis.ticks = element_line(),
          legend.position = legend.position) +
    xlim(xlim) +
    labs(x = "Feature Contribution (%)")

  if (show.mean) {
    p = p + geom_text(label = round(agg$Mean, mean.round),
                      hjust = -0.4)
  }

  p

}

plot_shap_interaction <- function(shaps, shap_feature, intX_feature,
                                  intZ_feature = NULL, type = c("b", "p", "s"),
                                  se = TRUE, smooth_color = "blue", smooth_fill = "lightblue",
                                  smooth_lwd = 0.5, smooth_level = 0.95,
                                  low = "blue", high = "red") {

  type = match.arg(type)

  int_shaps = shaps[,intX_feature,shap_feature]
  Xtruth = shaps[,intX_feature,"Truth"]
  if (!is.null(intZ_feature)) {
    Ztruth = shaps[,intZ_feature,"Truth"]
  } else {
    Ztruth = Xtruth
  }
  data = data.frame(SHAPS = int_shaps, XTruth = Xtruth, ZTruth = Ztruth)

  p = ggplot(data, aes(x = XTruth, y = SHAPS, color = ZTruth)) +
    scale_color_gradient(low = low, high = high, n.breaks = 3,
                         guide = guide_colorbar(title = intZ_feature,
                                                title.position = "right",
                                                title.theme = element_text(angle = 90,
                                                                           hjust = 0.5))) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line.y = element_line(), axis.line.x = element_line(),
          axis.ticks = element_line()) +
    labs(x = intX_feature,
         y = paste("SHAP Interaction Values\n", shap_feature, "X", intX_feature))

  if (type == "p" | type == "b") {
    p = p + geom_point()
  }
  if (type == "s" | type == "b") {
    p = p + geom_smooth(se = se, color = smooth_color, fill = smooth_fill,
                        lwd = smooth_lwd, level = smooth_level)
  }

  p

}

# library(mlr3proba)
# library(mlr3pipelines)
#
# set.seed(1)
# task = po("encode", method = "treatment")$train(list(tsk("lung")))$output
# lrn = lrn("surv.xgboost")
# lrn$train(task)
#
# shaps <- get_shaps(lrn$model, task)
# plot_shap_feature(shaps, "age", type = "s", se = TRUE, xlab = "Height")
# plot_shap_feature(shaps, "inst", type = "b", se = TRUE, xlab = "Weight")
# plot_shap_feature(shaps, "sex", type = "p", se = TRUE, xlab = "Treatment")
# plot_shap_summary(shaps)
# plot_shap_importance(shaps, legend.position = "right")
#
# shap_ints <- get_interaction_shaps(lrn$model, task)
# plot_shap_interaction(shap_ints, "ph.ecog", "pat.karno", "sex")
