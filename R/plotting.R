### plotting functions



# plot behavioral error --------------------------------------------------
#' Plot response error of behavioural data
#'
#'Function to plot the behavioural error data. Requires a data frame
#'that (at least) has target value data and participant response data.
#'
#' @param data A data frame with columns containing: participant identification
#' number ('id_var'); the participants' response per trial ('response_var');
#' the  target value ('target_var'); and, if applicable, the condition
#' ('condition_var').
#'@param unit The unit of measurement in the data frame: "degrees"
#'(measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#'degrees, but limited to 0 to 180); "radians" (measurement is in radians,
#'from pi to 2 * pi); "wrapped_radians" (measurement is in radians, but
#'wrapped from -pi to pi)
#'@param id_var The column name coding for partiicpant id.
#'@param response_var The column name coding for the participants' responses
#'@param target_var The column name coding for the target value
#'@param set_size_var The column name (if applicable) coding for the set
#'size of each response
#'@param condition_var If multiple conditions were presented, the column name
#'that codes for the current condition
#' @export
plot_error <- function(data,
                       unit = "degrees",
                       id_var = "id",
                       response_var = "response",
                       target_var = "target",
                       set_size_var = "set_size",
                       condition_var = "condition"){





}
