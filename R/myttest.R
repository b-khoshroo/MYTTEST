
#' Title
#'
#' @param x a vector
#' @param y a vector
#' @param alpha the parameter used in the t.test
#' @param paired whether the input data are paired or not
#' @importFrom stats t.test
#'
#' @return an object of type Rttest
#' @export
#'
#' @examples
#' \dontrun{myttest(x,y, 0.05, True)}
myttest <- function(x,y,alpha, paired) # This is our constructor
{
  # handeling unequal length of x and y if needed:
  if (length(x) > length(y))
  {
    y <- c(y, rep(NA,(length(x)-length(y))))
  } else if (length(y) > length(x))
  {
    x <- c(x, rep(NA,(length(y)-length(x))))
  }

  if(paired = FALSE)
  {
    v = var.test(x,y)

    if(v$p.value > 0.05)
    {
      #We fail to reject the NULL:
      t <- t.test(x, y, var.equal = TRUE)
      t_type <- "T-test"
    }

    else
    {
      #We reject the NULL:
      t <- t.test(x,y, var.equal = FALSE)
      t_type <- "Welch"
    }
  }

  else   #paired is TRUE and we want to do a paired test
  {
    t <- t.test(x,y,paired=TRUE)
    t_type <- "Paired"
  }

  if(t$p.value > alpha)
    rej_null <- "NO"
  else
    rej_null <- "YES"


  #creating the object/list:
  object = list(
    test_type = t_type,
    reject_null = rej_null,
    summary_stats = t,
    #data = data.frame(x,y),
    data = list(x,y),
    #test = t,
    #alpha = alpha,
    #confidence_interval = t$conf.int,
    #p_value = t$p.value
  )

  class(object) <- "Rttest" #attributing the class Rttest to the object

  invisible(object) #returning the object in an invisible way
}
