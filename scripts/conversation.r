# Conversation Class

Conversation.constructor <- function(d, start_id) {
  len <- 1
  
  walk_tree <- function(d, curr) {
    # print(paste0("curr = ", curr))
    
    if (any(d$tweet_id == curr)) {
      after <- d$after_id[d$tweet_id == curr][[1]]
    } else {
      # print(paste0(curr, " does not exist"))
      return(0)
    }
    
    if (any(is.na(after))) {
      # print(paste0("reached end, len = ", len))
      return(0)
    } else {
      # print(paste0(length(after), " options"))
      for (i in seq_along(after)) {
        len <<- len + 1
        # print(paste0("reached ", after[i], ", len = ", len))
        walk_tree(d, after[i])
      }
    }
  }
  
  walk_tree(d, start_id)
  
  conv <- vector(mode = "integer", length = len)
  conv[1] <- start_id
  len <- 1
  
  make_tree <- function(d, curr) {
    # print(paste0("curr = ", curr))
    
    if (any(d$tweet_id == curr)) {
      after <- d$after_id[d$tweet_id == curr][[1]]
    } else {
      # print(paste0(curr, " does not exist"))
      return(0)
    }
    
    if (any(is.na(after))) {
      # print(paste0("reached end, len = ", len))
      return(0)
    } else {
      # print(paste0(length(after), " options"))
      for (i in seq_along(after)) {
        len <<- len + 1
        # print(paste0("reached ", after[i], ", len = ", len))
        conv[len] <<- after[i]
        make_tree(d, after[i])
      }
    }
  }
  
  make_tree(d, start_id)
  
  comp = ""
  for (j in seq_along(conv)) {
    # print(paste("tweet_id: ", conv[j], ", seq: ", j))
    if(!any(d$tweet_id == conv[j])) { next }
    if (d$inbound[d$tweet_id == conv[j]] == FALSE) {
      comp <- d$author_id[d$tweet_id == conv[j]]
      break
    }
  }
  
  dc <- data.frame(
    comp_id = comp,
    conv = I(list(conv))
  )
  
  return(dc)
}

Conversation.constructor2 <- function(d, start_id) {
  len <- 1
  
  walk_tree <- function(d, curr) {
    # print(paste0("curr = ", curr))
    
    if (any(d$tweet_id == curr)) {
      after <- d$after_id[d$tweet_id == curr][[1]]
    } else {
      # print(paste0(curr, " does not exist"))
      return(0)
    }
    
    if (any(is.na(after))) {
      # print(paste0("reached end, len = ", len))
      return(0)
    } else {
      # print(paste0(length(after), " options"))
      for (i in seq_along(after)) {
        len <<- len + 1
        # print(paste0("reached ", after[i], ", len = ", len))
        walk_tree(d, after[i])
      }
    }
  }
  
  walk_tree(d, start_id)
  
  conv <- vector(mode = "integer", length = len)
  conv[1] <- start_id
  len <- 1
  
  make_tree <- function(d, curr) {
    # print(paste0("curr = ", curr))
    
    if (any(d$tweet_id == curr)) {
      after <- d$after_id[d$tweet_id == curr][[1]]
    } else {
      # print(paste0(curr, " does not exist"))
      return(0)
    }
    
    if (any(is.na(after))) {
      # print(paste0("reached end, len = ", len))
      return(0)
    } else {
      # print(paste0(length(after), " options"))
      for (i in seq_along(after)) {
        len <<- len + 1
        # print(paste0("reached ", after[i], ", len = ", len))
        conv[len] <<- after[i]
        make_tree(d, after[i])
      }
    }
  }
  
  make_tree(d, start_id)
  
  comp = ""
  for (j in seq_along(conv)) {
    # print(paste("tweet_id: ", conv[j], ", seq: ", j))
    if(!any(d$tweet_id == conv[j])) { next }
    if (d$inbound[d$tweet_id == conv[j]] == FALSE) {
      comp <- d$author_id[d$tweet_id == conv[j]]
      break
    }
  }
  
  dc <- data.frame(
    comp_id = comp,
    conv = I(list(conv))
  )
  
  return(dc)
}

Conversation.constructor3 <- function(start_id, d) {
  len <- 1
  
  walk_tree <- function(d, curr) {
    # print(paste0("curr = ", curr))
    after <- d$after_id[curr][[1]]
    
    if (any(is.na(after))) {
      # print(paste0("reached end, len = ", len))
      return(0)
    } else {
      # print(paste0(length(after), " options"))
      for (i in seq_along(after)) {
        len <<- len + 1
        # print(paste0("reached ", after[i], ", len = ", len))
        walk_tree(d, after[i])
      }
    }
  }
  
  walk_tree(d, start_id)
  
  conv <- vector(mode = "integer", length = len)
  conv[1] <- start_id
  len <- 1
  
  make_tree <- function(d, curr) {
    # print(paste0("curr = ", curr))
    after <- d$after_id[curr][[1]]
    
    if (any(is.na(after))) {
      # print(paste0("reached end, len = ", len))
      return(0)
    } else {
      # print(paste0(length(after), " options"))
      for (i in seq_along(after)) {
        len <<- len + 1
        # print(paste0("reached ", after[i], ", len = ", len))
        conv[len] <<- after[i]
        make_tree(d, after[i])
      }
    }
  }
  
  make_tree(d, start_id)
  
  comp = ""
  for (j in seq_along(conv)) {
    # print(paste("tweet_id: ", conv[j], ", seq: ", j))
    # print(d$inbound[conv[j]])
    if (d$inbound[conv[j]] == FALSE) {
      comp <- d$author_id[conv[j]]
      break
    }
  }
  
  dc <- data.frame(
    comp_id = comp,
    conv = I(list(conv))
  )
  
  return(dc)
}







