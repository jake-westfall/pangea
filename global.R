textInputMini <- function (inputId, label, value = ""){
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,
                 class = "input-mini", style="width: 100%"))
}

EMS <- function(design, nested=NULL, random=NULL){
  # modify design formula based on nested factors specified
  if(!is.null(nested)){
    terms <- attr(terms(design), "term.labels")
    # for each nested, get indices of all terms not involving their interaction 
    keeps <- lapply(strsplit(nested, "/"), function(x){
      which(apply(sapply(x, grepl, terms), 1, function(x) !all(x)))
    })
    terms <- terms[Reduce(intersect, keeps)]
    formula <- paste(c(as.character(design)[2:1], paste(terms, collapse="+")), collapse="")
    design <- eval(parse(text=formula))
  }
  
  # build two-way table
  mat <- t(attr(terms(design), "factors"))
  terms <- tolower(as.character(attr(terms(design), "variables"))[-1])
  
  # resolve fixed/random dummies
  if (!is.null(random)){
    random <- unlist(strsplit(random,split=""))
    mat[,which(colnames(mat) %in% random)][mat[,
      which(colnames(mat) %in% random)]==1] <- ""
    mat[,which(!colnames(mat) %in% random)][mat[,
      which(!colnames(mat) %in% random)]==1] <- "fix"
  }
  
  # insert 1 in nested rows
  subs <- strsplit(rownames(mat), split=":")
  if(!is.null(nested)){
    nested <- strsplit(nested, split="/")
    for(term in nested){
      rows <- unlist(lapply(subs, function(x) term[2] %in% x))
      cols <- colnames(mat)==term[1]
      mat[rows,cols] <- "1"
    }
  }
  mat <- rbind(mat, error=rep("1", ncol(mat)))
  
  # insert numbers of levels for remaining cells
  for(row in seq(nrow(mat))){
    mat[row,][mat[row,]=="0"] <- tolower(colnames(mat)[mat[row,]=="0"])
  }
  
  # construct EMS table
  ems <- matrix(nrow=nrow(mat), ncol=nrow(mat),
                dimnames=list(Effect=rownames(mat),
                              VarianceComponent=rev(rownames(mat))))
  # add nesting information to subscripts
  if (!is.null(nested)){
    subs <- lapply(subs, function(x){
      new <- x
      for (nest in seq(length(nested))){
        if (nested[[nest]][2] %in% x) new <- c(new, nested[[nest]][1])
      }
      return(new)
    })
  }
  subs[["error"]] <- colnames(mat)[-1]
  names(subs) <- rownames(mat)
  # rename error variable to 'error' invisibly
  colnames(mat)[1] <- "error"
  # fill in EMS table
  for(effect in rownames(ems)){
    for(varcomp in colnames(ems)){
      effectVec <- unlist(strsplit(effect, ":"))
      ans <- mat[varcomp,-1*which(colnames(mat) %in% effectVec)]
      if ("fix" %in% ans) ans <- ""
      if (all(ans=="1")) ans <- "1"
      if (("1" %in% ans | "2" %in% ans) & !all(ans=="1")){
        ans <- ans[!ans %in% c("1","2")]
      }
      varcompVec <- unlist(strsplit(varcomp, ":"))
      if (!all(effectVec %in% subs[[varcomp]])) ans <- ""
      if (effect=="error" & varcomp=="error") ans <- "1"
      ems[effect,varcomp] <- paste(ans, collapse="")
    }
  }
  attr(ems, "terms") <- terms
  
  # change replicate character to "#" before returning
  repChar <- rownames(attr(terms(design), "factors"))[1]
  ems <- gsub(repChar, "#", ems)
  
  # change ':' to 'x' before returning
  colnames(ems) <- gsub(":", "*", colnames(ems))
  rownames(ems) <- gsub(":", "*", rownames(ems))
  return(ems)
}
