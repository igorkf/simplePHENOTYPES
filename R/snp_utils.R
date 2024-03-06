make_numeric <- function(a,
                         method = NULL,
                         ref = NULL,
                         model = NULL,
                         hets = NULL,
                         homo = NULL,
                         AA = NULL,
                         Aa = NULL,
                         aa = NULL) {
  
  if (method == "frequency") {
    a[!a %in% c(homo, hets)] <- "[OTHER]"
    a <- as.factor(a)
    count <- tabulate(a)
    names(count) <- levels(a)
    if (length(count) > 3 | "[OTHER]" %in% a) {
      message("non-biallelic SNP set to NA")
      a <- NA
      return(a)
    }
    count <- count[!names(count) %in% hets]
    if (model == "Add") {
      a <- data.table::fifelse(a %in% hets, Aa, data.table::fifelse(a == names(which.max(count)), AA,aa))
    } else if (model == "Dom") {
      a <- data.table::fifelse(a == "Aa", Aa, AA)
    } else if (model == "Left") {
      a <- data.table::fifelse(a == "Aa" | a == names(which.max(count)), AA,aa)
    } else if (model == "Right") {
      a <- data.table::fifelse(a == "Aa" | a != names(which.max(count)), aa, AA)
    }
  } else if (method == "reference")  {
    a[!a %in% c(homo, hets)] <- "[OTHER]"
    count <- length(unique(a))
    if (count > 3 | "[OTHER]" %in% a) {
      message("non-biallelic SNP set to NA")
      a <- NA
      return(a)
    }
    if (any(homo %in% c("0/0", "0|0", "1/1", "1|1"))) {
      if (model == "Add") {
        a <- data.table::fifelse(a == "1/1" | a == "1|1", AA, data.table::fifelse(a %in% hets, Aa, aa))
      } else if (model == "Dom") {
        a <- data.table::fifelse(a == "1/1" | a == "1|1", AA, Aa)
      } else if (model == "Left") {
        a <- data.table::fifelse(a == "1/1" | a == "1|1", AA, aa)
      } else if (model == "Right") {
        a <- data.table::fifelse(a == "1/1" | a == "1|1", aa, AA)
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
  return(a)
}

impute <- function(x, method, AA = 1, Aa = 0, aa = 1) {
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
