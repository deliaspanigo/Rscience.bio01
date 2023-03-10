MedidasPosicion <- function(base, selected_columns = c(1:ncol(base)), digits = 2){
  
  dummy_case01 <- psych::describe(mtcars[1])
  dummy_case01 <- as.data.frame(dummy_case01)
  dummy_case01[1,] <- rep("No Data", ncol(dummy_case01))
  dummy_case01[1,"n"] <- 0
  rownames(dummy_case01)[1] <- ""
  
  dummy_case02 <- psych::describe(mtcars[1])
  dummy_case02 <- as.data.frame(dummy_case02)
  dummy_case02[1,] <- rep("Not Numeric", ncol(dummy_case02))
  # dummy_case02[1,"n"] <- 0
  rownames(dummy_case02)[1] <- ""

    
  if(length(selected_columns) == 1){
    out <- list()  
    dt_numeric <- is.numeric(base[,selected_columns])
    n_data <- sum(!is.na(base[,selected_columns]))
    dt_n <- n_data > 0
    
    if(dt_n && dt_numeric) new_step <- round2(psych::describe(base[,selected_columns]), digits) else
      if(dt_n && !dt_numeric) new_step <- dummy_case02 else
        if(!dt_n) new_step <- dummy_case01
    
    
    new_step <- new_step[c("vars", "n", "min", "max", "mean", "median")]

    new_step[1,1] <- colnames(base)[selected_columns]
    # rownames(new_step) <- 1
    
    out[[1]] <- new_step
    # names(out)[1] <- colnames(base)[selected_columns
    
    
  } else
    if(length(selected_columns) > 1){
      
      out <- list()
      for (k in 1:length(selected_columns)){
        out[[k]] <- MedidasPosicion(base = base, 
                        selected_columns = selected_columns[k], 
                        digits = digits)
      }
    }
  
  the_table <- do.call(rbind.data.frame, out)
  rownames(the_table) <- c(1:nrow(the_table))
  return(the_table)
  
  
  
  
}

MedidasDispersion <- function(base, selected_columns = c(1:ncol(base)), digits = 2){
  
  dummy_case01 <- psych::describe(mtcars[1])
  dummy_case01 <- as.data.frame(dummy_case01)
  dummy_case01[1,] <- rep("No Data", ncol(dummy_case01))
  dummy_case01[1,"n"] <- 0
  rownames(dummy_case01)[1] <- ""
  
  dummy_case02 <- psych::describe(mtcars[1])
  dummy_case02 <- as.data.frame(dummy_case02)
  dummy_case02[1,] <- rep("Not Numeric", ncol(dummy_case02))
  # dummy_case02[1,"n"] <- 0
  rownames(dummy_case02)[1] <- ""
  
  
  if(length(selected_columns) == 1){
    out <- list()  
    dt_numeric <- is.numeric(base[,selected_columns])
    n_data <- sum(!is.na(base[,selected_columns]))
    dt_n <- n_data > 0
    
    if(dt_n && dt_numeric) new_step <- round2(psych::describe(base[,selected_columns]), digits) else
      if(dt_n && !dt_numeric) new_step <- dummy_case02 else
        if(!dt_n) new_step <- dummy_case01
    
    
    new_step <- new_step[c("vars", "n", "range", "sd", "se")]
    
    if(dt_n && dt_numeric) {
    # standard_deviance <- as.vector(as.matrix(new_step["sd"]))
    standard_deviance <- sd(na.omit(base[,selected_columns]))
    the_mean <- mean(na.omit(base[,selected_columns]))
    coef_variability <- standard_deviance/the_mean
    coef_variability <- round2(coef_variability, digits = digits)
    coef_var_porcen <- paste0(coef_variability*100, "%")
    
    the_variance <- var(na.omit(base[,selected_columns]))
    the_variance <- round2(the_variance, digits = digits)
    
   
    } else
      if(dt_n && !dt_numeric){ 
        the_variance <- "Not Numeric" 
        coef_variability <- "Not Numeric" 
        coef_var_porcen <- "Not Numeric" 
        }else
        if(!dt_n){ 
          the_variance <- "No Data"
          coef_variability <- "No Data"
          coef_var_porcen <- "No Data" 
        }
    armado <- data.frame(the_variance, coef_variability, coef_var_porcen)
    colnames(armado) <- c("var", "cv", "cv porcentual")
    
    
    new_step <- cbind(new_step, armado)
    
    new_step <- new_step[c("vars", "n", "range", "sd", "var", "se", "cv", "cv porcentual")]
    
    
    new_step[1,1] <- colnames(base)[selected_columns]
    # rownames(new_step) <- 1
    
    out[[1]] <- new_step
    # names(out)[1] <- colnames(base)[selected_columns
    
    
  } else
    if(length(selected_columns) > 1){
      
      out <- list()
      for (k in 1:length(selected_columns)){
        out[[k]] <- MedidasDispersion(base = base, 
                                    selected_columns = selected_columns[k], 
                                    digits = digits)
      }
    }
  
  the_table <- do.call(rbind.data.frame, out)
  rownames(the_table) <- c(1:nrow(the_table))
  return(the_table)
  
  
  
  # out <- list()
  # for(k in 1:length(selected_columns)){
  #   
  #   n_data <- sum(!is.na(base[,selected_columns[k]]))
  #   
  #   if(n_data > 0) new_step <- round2(psych::describe(base[,selected_columns[k]]), digits) else
  #     if(n_data == 0) new_step <- dummy_case
  #     
  #   new_step <- new_step[c("vars", "n", "min", "max", "mean", "median")]
  #   
  #     new_step[1,1] <- colnames(base)[selected_columns[k]]
  #     rownames(new_step) <- k
  #     
  #     out[[k]] <- new_step
  #     names(out)[k] <- colnames(base)[selected_columns[k]]
  # }
  # 
  # the_table <- do.call(rbind.data.frame, out)
  # rownames(the_table) <- c(1:nrow(the_table))
  # 
  # return(the_table)
  
}


MedidasResumen <- function(base, selected_columns = c(1:ncol(base)), digits = 2){
  
 
  tabla_posicion <- MedidasPosicion(base, selected_columns, digits)
  tabla_dispersion <- MedidasDispersion(base, selected_columns, digits)
  tabla_dispersion <- tabla_dispersion[-c(1,2)]
  
  tabla_resumen <- cbind.data.frame(tabla_posicion, tabla_dispersion)
 
  return(tabla_resumen)
  
  
  
  
  
}


Graficos.1C <- function(base, selected_columns, only_plot = TRUE){
  
  dummy01_boxplot_case01 <- "plot(x = 1:10, y = 1:10, col = 'white', axes = F, 
                                  ylab = '', xlab = '_selected_colname_',
                                  main = 'Grafico 01 de 05\nBoxplot')
                              text(5, 5, 'No Data')"
  
  dummy01_boxplot_case02 <- "plot(1:10, 1:10, col = 'white', axes = F, 
                                  ylab = '', xlab = '_selected_colname_',
                                  main = 'Grafico 01 de 05\nBoxplot')
                            text(5, 5, 'Not Numeric')"
  
  
  dummy02_hist_case01 <- "plot(1:10, 1:10, col = 'white', axes = F, 
                               ylab = '', xlab = '_selected_colname_',
                                  main = 'Grafico 02 de 05\nHistograma')
                              text(5, 5, 'No Data')"
  
  dummy02_hist_case02 <- "plot(1:10, 1:10, col = 'white', axes = F, 
                                ylab = '', xlab = '_selected_colname_',
                                  main = 'Grafico 02 de 05\nHistograma')
                            text(5, 5, 'Not Numeric')"
  
  selected_colname <- colnames(base)[selected_columns]
  
  graph_01_boxplot <- paste0("boxplot(x = base[,'_selected_colname_'],
                              main = 'Grafico 01 de 05\nBoxplot', 
                              xlab = '_selected_colname_',
                              col = 'red')")
  
  
  graph_02_hist <- paste0("hist(x = base[,'_selected_colname_'],
                          main = 'Grafico 02 de 05\nHistograma', 
                          xlab = '_selected_colname_',
                          col = 'red')")
  
  
 
  
  if(length(selected_columns) == 1){
    the_graph <- list()
    
    dt_numeric <- is.numeric(base[,selected_columns])
    n_data <- sum(!is.na(base[,selected_columns]))
    dt_n <- n_data > 0
    
    if(dt_n && dt_numeric) the_graph[[1]] <- graph_01_boxplot else
      if(dt_n && !dt_numeric) the_graph[[1]] <- dummy01_boxplot_case02 else
        if(!dt_n) the_graph[[1]] <- dummy01_boxplot_case01
    
    if(dt_n && dt_numeric) the_graph[[2]] <- graph_02_hist else
      if(dt_n && !dt_numeric) the_graph[[2]] <- dummy02_hist_case02 else
        if(!dt_n) the_graph[[2]] <- dummy02_hist_case01
  
  
  
    # General Replace
    for (k in 1:length(the_graph)) the_graph[[k]] <- gsub(pattern = "_selected_colname_", 
                                                          replacement = selected_colname,
                                                          x = the_graph[[k]])
    
    
  if(only_plot) for (k in 1:length(the_graph)) eval(parse(text = the_graph[[k]]))
    else return(the_graph)
  
  }else
    if(length(selected_columns) > 1){
      
      the_graph <- list()
      for (k in 1:length(selected_columns)){
        the_graph[[k]] <- Graficos.1C(base = base, 
                                      selected_columns = selected_columns[k],
                                      only_plot = FALSE)
      }
    }
  
  if(only_plot){
        for (k1 in 1:length(the_graph)){
          for (k2 in 1:length(the_graph[[k1]])){
            eval(parse(text = the_graph[[k1]][[k2]]))
          }
        }
  } else return(the_graph)
}

###########


chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            tags$select(class="left", size=size, multiple=multiple, leftChoices)
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-right", "right-arrow fa-3x"),
            tags$br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            tags$select(class="right", size=size, multiple=multiple, rightChoices)
        )
    )
  )
}

# registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
# if (is.null(data))
#    NULL
#  else
#    list(left=as.character(data$left), right=as.character(data$right))
#}, force = TRUE)


