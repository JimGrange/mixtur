#' Data set from Berry et al. (2019)
#'
#' A data set including data from 30 participants in a continuous report
#' visual short-term memory experiment. The stimuli were oriented bars within
#' the range 1-180 degrees. The experiment had a set size of 3.
#'
#' @format A data frame with 3600 rows and 6 variables:
#' \describe{
#'   \item{id}{participant identification}
#'   \item{condition}{condition of experiment: whether the task was completed
#'   under single-task or dual-task conditions}
#'   \item{target_ori}{the orientation of the target in degrees (1-180)}
#'   \item{response_ori}{the participant's recollection of the target
#'   orientation in degrees (1-180)}
#'   \item{non_target_1}{the orinetation of the first non-target in degrees
#'   (1-180)}
#'   \item{non_target_2}{the orientation of the second non-target in degrees
#'   (1-180)}
#' }
#' @source
#' The data set is publicly available on the Open Science Framework:
#' https://osf.io/59c4g/
#' @references
#'    Berry. E.D.J., Allen, R.J., Waterman, A.H., & Logie, R.H. (2019). The
#'    effect of a verbal concurrent task on visual precision in working
#'    memory. Experimental Psychology, 66, (77-85).
"berry_2019"


#' Full data set from Bays et al. (2009)
#'
#' A full data set including data from 12 participants in a continuous report
#' visual short-term memory experiment. The stimuli were coloured squares
#' in the range radians -pi to pi. The experiment had various set sizes and
#' an additional manipulation of delay of retention interval.
#'
#' @format A data frame with 7271 rows and 10 variables:
#' \describe{
#'   \item{id}{participant identification}
#'   \item{delay}{the delay of the retention interval (in milliseconds, ms),
#'   with levels 100ms, 500ms, 2000ms}
#'   \item{response}{the participant's recollection of the target orientation
#'   in radians (-pi to pi)}
#'   \item{target}{the feature value of the target in radians (-pi to pi)}
#'   \item{non_target_1}{the feature value of the first non-target in radians
#'   (-pi to pi)}
#'   \item{non_target_2}{the feature value of the second non-target in radians
#'   (-pi to pi)}
#'   \item{non_target_3}{the feature value of the third non-target in radians
#'   (-pi to pi)}
#'   \item{non_target_4}{the feature value of the fourth non-target in radians
#'   (-pi to pi)}
#'   \item{non_target_5}{the feature value of the fifth non-target in radians
#'   (-pi to pi)}
#' }
#' @references
#'    Bays, P.M., Catalao, R.F.G., & Husain, M. (2009). The precision of visual
#'    working memory is set by allocation of a shared resource. Journal of
#'    Vision, 9(10), Article 7.
#' @source
#' The data set is publicly available on the Open Science Framework, with thanks
#' to Paul Bays: https://osf.io/c2yx5/
"bays2009_full"


#' Sample data set from Bays et al. (2009)
#'
#' A sample data set including data from 12 participants in a continuous report
#' visual short-term memory experiment. The stimuli were coloured squares
#' in the range radians -pi to pi. The sample data set only consists of trials
#' with a set size of 4 and a retention interval delay of 500ms.
#'
#' @format A data frame with 7271 rows and 10 variables:
#' \describe{
#'   \item{id}{participant identification}
#'   \item{response}{the participant's recollection of the target orientation
#'   in radians (-pi to pi)}
#'   \item{target}{the feature value of the target in radians (-pi to pi)}
#'   \item{non_target_1}{the feature value of the first non-target in radians
#'   (-pi to pi)}
#'   \item{non_target_2}{the feature value of the second non-target in radians
#'   (-pi to pi)}
#'   \item{non_target_3}{the feature value of the third non-target in radians
#'   (-pi to pi)}
#' }
#' @references
#'    Bays, P.M., Catalao, R.F.G., & Husain, M. (2009). The precision of visual
#'    working memory is set by allocation of a shared resource. Journal of
#'    Vision, 9(10), Article 7.
#' @source
#' The data set is publicly available on the Open Science Framework, with thanks
#' to Paul Bays: https://osf.io/c2yx5/
"bays2009_sample"
