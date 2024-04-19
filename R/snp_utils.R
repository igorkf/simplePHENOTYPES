make_numeric <- function(a,
                         method = NULL,
                         ref = NULL,
                         model = NULL,
                         hets = NULL,
                         homo = NULL,
                         AA = NULL,
                         Aa = NULL,
                         aa = NULL) {
  
  a[!na.omit(a) %in% c(homo, hets)] <- NA
  a <- as.factor(a)

  if (method == "frequency") {
    count <- tabulate(a)
    names(count) <- levels(a)
    count_homo <- count[names(count) %in% homo]
    mode_homo <- names(which.max(count))
    if (model == "Add") {
      a <- data.table::fifelse(a %in% hets, Aa, data.table::fifelse(a == mode_homo, AA, aa))
    } else if (model == "Dom") {
      a <- data.table::fifelse(a %in% hets, Aa, AA)
    } else if (model == "Left") {
      a <- data.table::fifelse(a %in% hets | a == mode_homo, AA, aa)
    } else if (model == "Right") {
      a <- data.table::fifelse(a %in% hets | a != mode_homo, aa, AA)
    }
  } else if (method == "reference")  {
    if (any(homo %in% c("0/0", "0|0", "1/1", "1|1"))) {
      a_is_one <- (a == "1/1" | a == "1|1")
      if (model == "Add") {
        a <- data.table::fifelse(a_is_one, AA, data.table::fifelse(a %in% hets, Aa, aa))
      } else if (model == "Dom") {
        a <- data.table::fifelse(a_is_one, AA, Aa)
      } else if (model == "Left") {
        a <- data.table::fifelse(a_is_one, AA, aa)
      } else if (model == "Right") {
        a <- data.table::fifelse(a_is_one, aa, AA)
      }
    } else {
      if (model == "Add") {
        a <- data.table::fifelse(a == ref, AA, data.table::fifelse(a %in% hets, Aa, aa))
      } else if (model == "Dom") {
        a <- data.table::fifelse(a == ref, AA, Aa)
      } else if (model == "Left") {
        a <- data.table::fifelse(a == ref, AA, aa)
      } else if (model == "Right") {
        a <- data.table::fifelse(a == ref, aa, AA)
      }
    }
  }

  if (length(unique(na.omit(a))) > 3) {
    print('here')
    message("Non-biallelic SNP. Returning NA.")
    a <- rep(NA, length(a))
  }

  return(a)
}

impute <- function(x, method, AA = 1, Aa = 0, aa = -1) {
  msg <- "Please consider specialized software for more accurate genotype imputation."
  rlang::inform(msg, .frequency = "once", .frequency_id = msg)
  na <- is.na(x)
  if (any(na)) {
    if (method == "Middle") {
      x[na] <- Aa
    } else if (method == "Minor") {
      x[na] <- aa
    } else if (method == "Major") {
      x[na] <- AA
    }
  }
  return(x)
}
