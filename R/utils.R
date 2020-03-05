# Utility functions for InteractiveTradeR

# ib_connect "error" messages
err_msg <- function(fun_name, msg){
  usethis::ui_line(
    paste0(
      "\n\nIn ",
      crayon::bold(fun_name),
      "():"
    )
  )
  usethis::ui_oops(msg)
  usethis::ui_oops("No new sockets created.\n\n")
}

# Supress cat, by Hadley Wickham: http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# no prefixes or suffixes
simple_encode <- function(msg){
  unlist(
    lapply(
      as.character(msg),
      function(msg_element){
        pack::pack(paste0("a", nchar(msg_element) + 1), msg_element)
      }
    )
  )
}

# single low-level message llmsg
ib_decode_low_level_msg <- function(llmsg){
  
  to   <- which(llmsg == as.raw(0))
  from <- as.integer(c(1, (to + 1)[-length(to)]))
  
  unlist(
    Map(
      function(to, from){
        llmsg[from:to] %>%
          pack::unpack(paste0("a", length(.)), .)
      },
      to,
      from
    )
  )
  
}

# Format an Outgoing IB Message
ib_encode_raw_msg <- function(msg){
  simple_encode(msg) %>%
    c(rev(pack::pack("V", length(.))), .)
}

# Read a low-level message's size prefix
ib_read_incoming_message_size_bytes <- function(sock){
  readBin(
    sock,
    what   = raw(),
    n      = InteractiveTradeR::functionary$ib_raw_header_size_bytes,
    endian = "little"
  ) %>%
    rev() %>%
    pack::unpack(template = "V", .) %>%
    unlist()
}

read_channel_raw <- function(channel){
  
  if(inherits(channel, "sockconn")){
    channel <- list(channel)
  }
  
  ready_to_read <- which(socketSelect(channel, timeout = 1))
  
  if(identical(ready_to_read, integer(0))){
    return(invisible())
  }
  
  response_list_raw <- NULL
  
  while(!identical(ready_to_read, integer(0))){
    response_list_raw <- c(
      response_list_raw,
      lapply(
        ready_to_read,
        function(ready_sock){
          readBin(
            channel[[ready_sock]],
            what   = raw(),
            n      = ib_read_incoming_message_size_bytes(
              channel[[ready_sock]]
            ),
            endian = "little"
          )
        }
      )
    )
    
    ready_to_read <- which(socketSelect(channel, timeout = 0.1))
  }
  
  response_list_raw
  
}

print_async <- function(sock){
  
  cat(
    paste0(
      "A socket connection on ",
      usethis::ui_value(
        showConnections()%>% {
          .[rownames(.) == attr(sock, "conn_row"), "description"]
        }
      ),
      ".\n",
      crayon::bold("Client ID: "),
      attr(sock, "client_id"),
      crayon::bold("\tServer Version: "),
      attr(sock, "handshake")$server_version,
      crayon::bold("\tConnection time: "),
      attr(sock, "handshake")$start_time,
      crayon::bold("\nSubscriptions:\n")
    )
  )
  
  if(length(attr(sock, "subscriptions")) > 0){
    
    subscriptions <- lapply(attr(sock, "subscriptions"), function(x){names(x)})
    
    for(i in 1:length(subscriptions) ){
      cat(
        paste0(
          crayon::italic(names(subscriptions)[i]), ":\n\t",
          paste(subscriptions[[i]], collapse = ", "), "\n"
        )
      )
    }
  } else {
    cat(crayon::italic("None\n"))
  }
  
}

print.sock_drawer <- function(sock_drawer){
  cli::cat_rule()
  for(i in 1:length(sock_drawer)){
    cli::cat_line(cat(crayon::bold(names(sock_drawer[i]))))
    print_async(sock_drawer[[i]])
  }
}

active_itr_option <- function(opt_name){
  getOption(
    paste0(
      "interactivetrader.",
      tolower(getOption("interactivetrader.platform")),
      if(getOption("interactivetrader.paper")){
        ".paper."
      } else {
        ".live."
      },
      as.character(substitute(opt_name))
    )
  )
}

make_field_handle_empty <- function(field_var){
  if(
    is.null(field_var) ||
    isTRUE(
      any(
        c(
          functionary$common_params$UNSET_INTEGER,
          package_state$UNSET_DOUBLE
        ) == suppressWarnings(as.numeric(field_var))
      )
    ) ||
    length(field_var) == 0 ||
    is.na(field_var)
  ){
    return("")
  }
  field_var
}

make_numeric_handle_empty <- function(field_var){
  if(
    is.null(field_var) || isTRUE(
      any(
        c(
          functionary$common_params$UNSET_INTEGER,
          package_state$UNSET_DOUBLE
        ) == suppressWarnings(as.numeric(field_var))
      )
    )
  ){
    return(as.numeric(NA))
  }
  as.numeric(field_var)
}

get_conId <- function(contract){
  if(is.na(contract['conId']) || is.na(contract['conId']) == 0){
    return(
      req_contract_details(contract = contract)$conId
    )
  }
  contract['conId']
}

fetch_master_w_msg <- function(){
  
  mcid <- active_itr_option("master")
  
  if(is.null(mcid)){
    usethis::ui_oops(
      "You must set a Master Client ID before using a master socket."
    )
    usethis::ui_info(
      paste0(
        "--->\tPlease see ",
        crayon::bold("?set_ib_options"),
        " for instructions."
      )
    )
  }
  
  mcid
  
}

print.active_conn_params <- function(x){
  cat(
    crayon::bold(
      crayon::inverse(
        "InteractiveTradeR set to connect using the following parameters:\n"
      )
    )
  )
  pad_length <- max(
    vapply(
      c(colnames(x), unlist(x, use.names = FALSE)),
      function(xx){nchar(trimws(xx))},
      numeric(1),
      USE.NAMES = FALSE
    )
  )
  pad <- function(xx){
    vapply(
      xx,
      function(xxx){
        paste0(xxx, paste(rep(" ", pad_length - nchar(xxx)), collapse = ""))
      },
      character(1),
      USE.NAMES = FALSE
    )
  }
  
  cat(crayon::bold(pad(colnames(x))))
  cat("\n")
  cat(pad(unlist(x, use.names = FALSE)))
}

fetch_and_bump <- function(fun_name){
  get0(fun_name, envir = package_state, ifnotfound = 0) %>% {
    assign(
      fun_name,
      . + 1,
      envir = package_state
    )
    .
  }
}

clean_slate <- function(
  what = c("archives", "sock_drawer", "subscriptions", "treasury")
){
  if(any(what == "sock_drawer")){
    purrr::walk(mget(ls(sock_drawer), sock_drawer), close)
  }
  if(length(what) > 0){
    purrr::walk(
      what,
      function(slate){
        eval(parse(text = slate)) %>%
          rm(list = ls(.), envir = .)
      }
    )
  }
}

clear_cols <- function(x){
  x[,names(
    which(
      !vapply(
        colnames(x),
        function(col_name){
          unlist(unique(x[,col_name]), use.names = FALSE) %>% {
            isTRUE(all(. == "")) ||
              all(is.na(.)) ||
              isTRUE(all(. == 0)) ||
              isTRUE(
                all(
                  . == InteractiveTradeR::functionary$common_params$NO_VALID_ID
                ) && !any(c("position", "clientId") == col_name)
              ) ||
              isTRUE(all(. == "?")) ||
              isTRUE(all(. == "None"))
          }
        },
        logical(1)
      )
    )
  )]
}

format_contract <- function(){
  
  contract <- get0("contract", envir = parent.frame())
  
  if(length(contract) == 1){
    c(conId = unlist(contract, use.names = FALSE))
  } else if(length(contract) == 2){
    suppressWarnings(as.numeric(contract)) %>% {
      c(
        conId    = contract[!is.na(.)],
        exchange = contract[is.na(.)]
      )
    }
  } else if(is.null(names(contract))){
    usethis::ui_oops(
      paste0(
        "The ",
        crayon::bold("contract"),
        " argument must have names. See ",
        crayon::bold("?contract"),
        " for details."
      )
    )
    stop("Bad contract object.")
  } else {
    contract
  }
  
}

format_market_data_type <- function(){
  
  marketDataType <- if(
    eval(expression(missing(mktDataType)), envir = parent.frame())
  ){
    if(exists("sock", envir = parent.frame())){
      attr(get0("sock", envir = parent.frame()), "marketDataType")
    }
  } else {
    get0("mktDataType", envir = parent.frame())
  }
  
  tryCatch(
    as.numeric(
      match.arg(
        as.character(
          if(is.null(marketDataType)){
            package_state$default_market_data_type
          } else {
            marketDataType
          }
        ),
        choices = as.character(
          c(1:4, "REALTIME", "FROZEN", "DELAYED", "DELAYED_FROZEN")
        )
      )
    ),
    warning = function(w){
      switch(
        marketDataType,
        "REALTIME"       = 1,
        "FROZEN"         = 2,
        "DELAYED"        = 3,
        "DELAYED_FROZEN" = 4
      )
    }
  )
  
}

check_for_saved_params <- function(){
  
  names(options()) %>% {
    .[grepl("interactivetrader", .)]
  } %>% 
    gsub("(.*)\\.", "", .) %>%
    unique() %>%
    sort() %>%
    identical(., c("host", "master", "paper", "platform", "port"))
  
}
