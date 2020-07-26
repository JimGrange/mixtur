
# fit model ---------------------------------------------------------------



JV10_fit <- function(X, Tg, NT = replicate(NROW(X), 0), return.ll = TRUE) {
  if(NCOL(X) > 2 | NCOL(Tg) > 1 | NROW(X) != NROW(Tg) | (any(NT != 0) & NROW(NT) != NROW(X) | NROW(NT) != NROW(Tg))) {
    stop("Error: Input not correctly dimensioned", call. = FALSE)
  }
  n = NROW(X)

  nn = ifelse(any(NT != 0), NCOL(NT), 0)

  # Start parameters
  K = c(1, 10, 100)
  N = c(0.01, 0.1, 0.4)
  U = c(0.01, 0.1, 0.4)

  if(nn == 0) {N = 0}

  loglik = -Inf

  # Parameter estimates
  for(i in seq_along(K)) {
    for(j in seq_along(N)) {
      for(k in seq_along(U)) {
        est_list = JV10_function(X = X, Tg = Tg, NT = NT, B_start = c(K[i], 1-N[j]-U[k], N[j], U[k]))
        if (est_list$ll > loglik & !is.nan(est_list$ll) ) {
          loglik = est_list$ll
          B = est_list$b
        }
      }
    }
  }

  if(return.ll == TRUE) {
    return(list(B = B, LL = loglik))
  } else {
    return(B)
  }
}




# Returns a list where the first element in a single row data frame
# of parameter estimates and the second is a single log likelihood value

JV10_function <- function(X, Tg,
                          NT = replicate(NROW(X), 0),
                          B_start = NULL) {

  if(NCOL(X) > 2 | NCOL(Tg) > 1 | NROW(X) != NROW(Tg) | (any(NT != 0) & NROW(NT) != NROW(X) | NROW(NT) != NROW(Tg))) {
    stop("Error: Input not correctly dimensioned", call. = FALSE)
  }

  if((!(is.null(B_start))) & (any(B_start[1] < 0, B_start[2:4] < 0, B_start[2:4] > 1, abs(sum(B_start[2:4]) - 1) > 10^-6))) {
    stop("Error: Invalid model parameters", call. = FALSE)
  }

  max_iter = 10^4; max_dLL = 10^-4

  n = NROW(X)

  nn = ifelse(any(NT != 0), NCOL(NT), 0)

  # Default starting parameter

  if(is.null(B_start)) {
    K = 5; Pt = 0.5
    Pn = ifelse(nn > 0, 0.3, 0)
    Pu = 1 - Pt - Pn
  } else {
    K = B_start[1]; Pt = B_start[2]
    Pn = B_start[3]; Pu = B_start[4]
  }

  E = wrap(X - Tg)

  if(nn > 0){
    NE = wrap(repmat(X, nn) - NT)
  } else {
    NE = repmat(X, nn)
  }

  LL = 0; dLL = 1; iter = 1

  while(TRUE) {
    iter = iter + 1

    Wt = Pt * vonmisespdf(E, 0, K)
    Wg = Pu * replicate(n, 1) / (2 * pi)

    if(nn == 0){
      Wn = matrix(nrow = NROW(NE), ncol = NCOL(NE))
    } else {
      Wn = Pn/nn * vonmisespdf(NE, 0, K)
    }

    W = rowSums(cbind(Wt, Wg, Wn))

    dLL = LL - sum(log(W))
    LL = sum(log(W))

    if(abs(dLL) < max_dLL | iter > max_iter | is.nan(dLL)) {
      break
    }

    Pt = sum(Wt / W) / n
    Pn = sum(rowSums(Wn) / W) / n
    Pu = sum(Wg / W) / n

    rw = c((Wt / W), (Wn / repmat(W, nn)))

    S = c(sin(E), sin(NE)) ; C = c(cos(E), cos(NE))
    r = c(sum(sum(S * rw)), sum(sum(C * rw)))

    if(sum(sum(rw, na.rm = T)) == 0) {
      K = 0
    } else {
      R = sqrt(sum(r^2)) / sum(sum(rw))
      K = A1inv(R)
    }

    if(n <= 15) {
      if(K < 2) {
        K = max(K - 2 / (n * K), 0)
      } else {
        K = K * (n - 1)^3 / (n^3 + n)
      }
    }
  }


  if(iter > max_iter) {
    warning('JV10_function:MaxIter','Maximum iteration limit exceeded.', call. = FALSE)
    B = c(NaN, NaN, NaN, NaN); LL = NaN
  } else {
    B = data.frame(K = K, Pt = Pt, Pn = Pn, Pu = Pu)
  }

  return(list(b = B, ll = LL))

}




JV10_likelihood <- function(B, X, Tg, NT = replicate(NROW(X), 0)) {
  if(NCOL(X) > 2 | NCOL(Tg) > 1 | NROW(X) != NROW(Tg) | (any(NT != 0) & NROW(NT) != NROW(X) | NROW(NT) != NROW(Tg))) {
    stop("Error: Input not correctly dimensioned", call. = FALSE)
  }
  if(B[1] < 0 | any(B[2:4] < 0) | any(B[2:4] > 1) | abs(sum(B[2:4]) - 1) > 10^-6) {
    stop("Error: Invalid model parameters")
  }

  n = NROW(X)

  E = wrap(X - Tg)

  Wt = B[2] * vonmisespdf(E, 0, B[1])
  Wu = B[4] * matrix(1, nrow = n, ncol = 1) / (2 * pi)

  if(any(NT != 0)) {
    L = rowSums(cbind(Wt, Wu))
  } else {
    nn = NCOL(NT)
    NE = wrap(repmat(X, nn) - NT)
    Wn = B[3] / nn * vonmisespdf(NE, 0, B[1])
    L = rowSums(cbind(Wt, Wn, Wu))
  }

  LL = sum(log(L))

  return(data.frame(LL = LL, L = L))

}



# This (admittedly ugly) function takes a dataframe and returns parameters
# estimates for each level of the id variable and the given
# target and response variables.

# If there are non-target variables then nt.vars should be either a list column
# names or a character vectorsi.e. nt.vars = c("nt1", "nt2")

# This function is by Ed Berry, I attribute none of its ugliness to Paul Bayes :')

JV10_df <- function(d, id.var = "id", tar.var = "target", res.var = "response", nt.vars = NULL){
  id <- d[, id.var]

  l <- split(d, id)

  paras <- data.frame(id = FALSE, K = FALSE, Pt = FALSE, Pn = FALSE, Pu = FALSE)

  for(i in seq_along(l)) {
    df <- as.data.frame.list(l[i], col.names = colnames(l[i]))

    X <- as.matrix(df[, res.var])
    Tg <- as.matrix(df[tar.var])

    if(is.null(nt.vars)) {
      B <- JV10_fit(X, Tg, return.ll = FALSE)
    } else {
      NT = as.matrix(df[,nt.vars])
      B <- JV10_fit(X, Tg, NT, FALSE)
    }
    id <- as.character(df[1, id.var])
    paras[i, 1] <- id
    paras[i,2:5] <- B
  }
  return(paras)
}




