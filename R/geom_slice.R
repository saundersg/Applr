
#' StatSlice
#'
#' A custom 'ggproto' object designed to take a high dimensional model,
#' pull the aesthetics from the global environment, and then create predictions
#' to display a slice of the high dimensional model.
#'
#' @format An object of class \code{ggproto}, inheriting from \code{Stat}
#'
#' @section Parameters:
#' \describe{
#' \item{model}{A fitted linear model object from 'lm()'}
#' \item{predict_vars}{A list of specified variables and values that specify the displayed slice}
#' \item{n}{The number of predicted points generated}
#' }
#'
#' @section Internal Methods:
#' \describe{
#' \item{get_default_value}{An internal function that determines the mean of numeric columns or most common factor in fator/character columns and holds that value constant}
#' \item{compute_panel}{Generates prediction data with for each facet of a 'ggplot' graph}
#' }
#'
#' @import ggplot2
#' @import rlang
#'
#' @export
StatSlice <- ggproto(
  "StatSlice",
  StatIdentity,
  required_aes = c('x','y'),
  setup_data = function(self, data, params){
    data
  },
  get_default_value = function(model, varname) {
    fdata <- model$model[[varname]]

    if (is.numeric(fdata)) {
      mean(fdata, na.rm = TRUE)
    } else if (is.factor(fdata) || is.character(fdata)) {
      freq <- table(fdata)

      val <- names(freq)[which.max(freq)]
      if (is.factor(fdata))
        val <- factor(val, levels = levels(fdata))
      val
    } else {
      stop("Unsupported column type: ", class(data))
    }
  },
  compute_layer = function(self, data, params, layout){
    params$facet_layout <- layout$layout
    mapping <- params$mapping
    ggproto_parent(Stat,self)$compute_layer(data,params,layout)
  },
  compute_panel = function(data, scales, params, n = 100,facet_layout,predict_vars, model, mapping) {

    facet_vars <- setdiff(names(facet_layout),
                          c("ROW",'COL', "SCALE_X", "SCALE_Y", "COORD"))

    data <- facet_layout |>
      select(!!!facet_vars) |>
      left_join(x = data, y = _, by = "PANEL")

    xvar <- rlang::as_name(rlang::quo_get_expr(mapping$x))
    yvar <- rlang::as_name(rlang::quo_get_expr(mapping$y))

    vars <- names(model$model) #Identify col names from model
    oldnames <- names(data) #Identify & save names from ggplot data

    names(data)[1:2] <- c(xvar,yvar)
    othervars <- setdiff(vars,c(xvar,yvar,facet_vars))

    for (var in othervars) {
        if (!is.null(predict_vars[[var]])) {
          data[[var]] <- predict_vars[[var]]
        } else {
          data[[var]] <- StatSlice$get_default_value(model, var)
        }
      }

    for (facet_var in facet_vars) {
      if (is.factor(model$model[[facet_var]])) {
        data[[facet_var]] <- factor(data[[facet_var]], levels = levels(model$model[[facet_var]]))
      } else if (is.character(model$model[[facet_var]])) {
        data[[facet_var]] <- as.character(data[[facet_var]])
      } else if (is.numeric(model$model[[facet_var]])) {
        data[[facet_var]] <- as.numeric(data[[facet_var]])
      }
    }


    tmp <- predict(model, newdata = data) #Predict new y col for displaying the model

    names(data)<- c(oldnames,othervars) #Revert col names to old names

    data$y<- tmp
    data
  }
  )

#'GeomSlice
#'
#'A custom 'ggproto' object that provides the graphing aesthetics
#'for the 'StatSlice' object
#'
#'@export
GeomSlice <- ggproto(
  'GeomSlice',
  GeomLine,
  default_aes = aes(
    color = "skyblue",
    linewidth = 1,
    linetype = "solid",
    alpha = 1)
)

#' Geom_slice
#'
#' A 'ggplot' layer that takes a high dimensional model and displays the predicted
#' model across each facet of a graph
#'
#' @param model A linear model provided by 'lm()'
#' @param n The number of predicted points across the graph - default 100
#' @param inherit.aes Inherits the aes provided by 'ggplot' object
#' @param predict_vars A list of specified variables and values that specify the slice - not required
#' @param ... Other variables (color, alpha, etc.)
#'
#' @returns A ggplot layer that displays the slice of the high dimensional model
#' @examples
#' \dontrun{
#' # Example: Using geom_slice without specifying 'predict_vars'
#' model <- lm(mpg ~ disp + hp, data = mtcars)
#' ggplot(mtcars, aes(x=disp,y=mpg))+
#' geom_point()+
#' facet_wrap(~hp)+
#' geom_slice(model=model)
#'
#' # Example: Using geom_slice with 'predict_vars'
#' model <- lm(mpg ~ disp + hp + gear, data = mtcars)
#' ggplot(mtcars, aes(x=disp, y=mpg))+
#' geom_point()+
#' facet_wrap(~hp)+
#' geom_slice(model=model, predict_vars=list(gear=4))
#'}
#'
#' @export
geom_slice <- function(
    model,
    n = 100,
    inherit.aes = TRUE,
    predict_vars = list(),
    ...
    )
  {
  my_layer <- layer(
    stat = StatSlice,
    geom = GeomSlice,
    position = 'identity',
    inherit.aes = inherit.aes,
    params = list(model = model,predict_vars = predict_vars, n = n,...)
  )

  ggproto(
    NULL, my_layer,
    compute_statistic = function(self, data, layout){
      params <- self$stat$setup_params(data, self$stat_params)
      self$computed_stat_params <- params[['mapping']] <- self$computed_mapping

      data <- self$stat$setup_data(data,params)

      self$stat$compute_layer(data,params,layout)

    }
  )
}
