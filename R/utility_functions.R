get_argstring <- function(args_list){
  out_string <- ""
  for(i in seq_along(args_list)){
    if(!is.null(args_list[[i]])){
      out_string <- paste0(out_string, "&", names(args_list)[i],
                                "=", args_list[[i]])
    }
  }
  out_string
}
