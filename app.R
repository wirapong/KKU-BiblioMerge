# Load required libraries
library(shiny)
library(bibliometrix)
library(openxlsx)
library(shinythemes)
library(DT)  # Added for better table displays
library(dplyr)  # Added for data manipulation
library(plotly)  # Added for interactive plots
library(microbenchmark)
library(tictoc)
library(fontawesome)

# Global constants
REQUIRED_PACKAGES <- c("shiny", "bibliometrix", "openxlsx", "shinythemes", "DT", "dplyr", "plotly")
KEY_FIELDS <- c("AU", "TI", "SO", "PY", "DT", "TC")
FILE_TYPES <- list(
  scopus = list(ext = ".bib", format = "bibtex"),
  wos = list(ext = ".txt", format = "plaintext")
)

# Helper functions
source_exists <- function() {
  for (pkg in REQUIRED_PACKAGES) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }
}

# UI Components
headerUI <- function() {
  titlePanel(
    div(
      class = "header",
      tags$h1("KKU-BiblioMerge V.2.0 ---24122024---", class = "title"),
      tags$p("Bibliometric Data Integration and Analysis Tool", class = "subtitle")
    )
  )
}


# Performance monitoring functions
library(microbenchmark)
library(pryr)
library(tictoc)

# Function to monitor system performance
monitorSystemPerformance <- function() {
  # Initialize performance metrics list
  perf_metrics <- list(
    memory_usage = mem_used(),
    cpu_time = system.time({}),
    gc_stats = gc(),
    objects_in_memory = length(ls())
  )
  return(perf_metrics)
}

# Function to track file processing time
trackFileProcessing <- function(file_path, dbsource) {
  # Start performance monitoring
  start_metrics <- monitorSystemPerformance()
  tic("file_processing")
  
  # Process file and track performance
  result <- list(
    success = FALSE,
    data = NULL,
    metrics = list()
  )

  
  # Custom WoS/ISI parser implementation
  parseWoS <- function(filepath) {
    # Read file content as binary
    rawContent <- readBin(filepath, "raw", n = file.info(filepath)$size)
    content <- rawToChar(rawContent)
    
    # Split into records
    records <- strsplit(content, "\nER\n")[[1]]
    
    # Initialize lists to store field data
    all_records <- list()
    field_names <- character()
    
    # Process each record
    for (record in records) {
      if (nchar(trimws(record)) == 0) next
      
      # Split record into lines
      lines <- strsplit(record, "\n")[[1]]
      current_record <- list()
      current_field <- NULL
      
      # Process each line
      for (line in lines) {
        # Check if line starts with a field marker (2 uppercase letters)
        if (grepl("^[A-Z]{2} ", line)) {
          # Extract field tag and content
          field_tag <- substr(line, 1, 2)
          field_content <- trimws(substr(line, 4, nchar(line)))
          
          # Store field
          if (field_tag %in% names(current_record)) {
            # If field exists, append content
            current_record[[field_tag]] <- c(current_record[[field_tag]], field_content)
          } else {
            # New field
            current_record[[field_tag]] <- field_content
            # Track new field names
            if (!(field_tag %in% field_names)) {
              field_names <- c(field_names, field_tag)
            }
          }
        } else if (nchar(trimws(line)) > 0) {
          # Continuation of previous field
          if (length(names(current_record)) > 0) {
            last_field <- names(current_record)[length(names(current_record))]
            current_record[[last_field]][length(current_record[[last_field]])] <- 
              paste(current_record[[last_field]][length(current_record[[last_field]])], 
                    trimws(line))
          }
        }
      }
      
      # Add record to collection
      if (length(current_record) > 0) {
        all_records[[length(all_records) + 1]] <- current_record
      }
    }
    
    # Convert to data frame
    result <- data.frame(matrix(NA, nrow = length(all_records), 
                                ncol = length(field_names)))
    colnames(result) <- field_names
    
    # Fill data frame
    for (i in seq_along(all_records)) {
      record <- all_records[[i]]
      for (field in names(record)) {
        result[i, field] <- paste(record[[field]], collapse = "; ")
      }
    }
    
    return(result)
  }
  
  # Modified WoS file observer
  observeEvent(input$wosFile, {
    req(input$wosFile)
    
    withProgress(message = 'Processing WoS file...', value = 0, {
      tryCatch({
        # Process file with custom parser
        rv$wos_data <- parseWoS(input$wosFile$datapath)
        
        # Update UI
        if (!is.null(rv$wos_data) && nrow(rv$wos_data) > 0) {
          output$statusOutput <- renderUI({
            HTML(sprintf(
              "<div style='color: green;'>Successfully processed %d records from WoS file</div>",
              nrow(rv$wos_data)
            ))
          })
        }
        
      }, error = function(e) {
        output$statusOutput <- renderUI({
          HTML(sprintf(
            "<div style='color: red;'>Error processing WoS file: %s</div>",
            e$message
          ))
        })
      })
    })
  })
  
  
  
  
  
  
  
  return(result)
}

# Function to measure merging performance
trackMerging <- function(wos_data, scopus_data) {
  tic("merging")
  
  result <- list(
    success = FALSE,
    data = NULL,
    metrics = list()
  )
  
  tryCatch({
    # Record initial state
    start_metrics <- monitorSystemPerformance()
    
    # Merge with timing
    merge_time <- system.time({
      result$data <- mergeDbSources(wos_data, scopus_data, remove.duplicated = TRUE)
    })
    
    # Stop timing
    merge_proc_time <- toc(log = TRUE, quiet = TRUE)
    
    # Calculate metrics
    end_metrics <- monitorSystemPerformance()
    
    result$success <- TRUE
    result$metrics <- list(
      input_records = nrow(wos_data) + nrow(scopus_data),
      output_records = nrow(result$data),
      merge_time_sec = merge_time[3],
      memory_used_mb = (end_metrics$memory_usage - start_metrics$memory_usage) / (1024 * 1024),
      records_per_second = nrow(result$data) / merge_time[3],
      timestamp = Sys.time()
    )
    
  }, error = function(e) {
    result$error <- e$message
  })
  
  return(result)
}

# Function to track analysis generation performance
trackAnalysis <- function(combined_data) {
  tic("analysis")
  
  result <- list(
    success = FALSE,
    metrics = list()
  )
  
  tryCatch({
    # Record initial state
    start_metrics <- monitorSystemPerformance()
    
    # Perform analysis with timing
    analysis_time <- system.time({
      # Your existing analysis functions
      metrics <- calculateMetrics(combined_data)
      critical_metrics <- generateMetricsReport(combined_data)
    })
    
    # Stop timing
    analysis_proc_time <- toc(log = TRUE, quiet = TRUE)
    
    # Calculate metrics
    end_metrics <- monitorSystemPerformance()
    
    result$success <- TRUE
    result$metrics <- list(
      analysis_time_sec = analysis_time[3],
      memory_used_mb = (end_metrics$memory_usage - start_metrics$memory_usage) / (1024 * 1024),
      records_analyzed = nrow(combined_data),
      analysis_speed = nrow(combined_data) / analysis_time[3],
      timestamp = Sys.time()
    )
    
  }, error = function(e) {
    result$error <- e$message
  })
  

  return(result)
}

# Function to generate performance report
generatePerformanceReport <- function(file_metrics, merge_metrics, analysis_metrics) {
  report <- list(
    summary = data.frame(
      operation = c("File Processing", "Data Merging", "Analysis Generation"),
      time_seconds = c(
        sum(sapply(file_metrics, function(x) x$metrics$processing_time_sec)),
        merge_metrics$metrics$merge_time_sec,
        analysis_metrics$metrics$analysis_time_sec
      ),
      memory_mb = c(
        sum(sapply(file_metrics, function(x) x$metrics$memory_used_mb)),
        merge_metrics$metrics$memory_used_mb,
        analysis_metrics$metrics$memory_used_mb
      ),
      records_processed = c(
        sum(sapply(file_metrics, function(x) x$metrics$records_processed)),
        merge_metrics$metrics$output_records,
        analysis_metrics$metrics$records_analyzed
      ),
      speed_records_per_sec = c(
        mean(sapply(file_metrics, function(x) x$metrics$processing_speed)),
        merge_metrics$metrics$records_per_second,
        analysis_metrics$metrics$analysis_speed
      )
    ),
    details = list(
      file_processing = file_metrics,
      merging = merge_metrics,
      analysis = analysis_metrics
    )
  )
  
  return(report)
}


sidebarUI <- function() {
  sidebarPanel(
    fileInput("scopusFile", "Upload Scopus File (.bib)", accept = c(".bib")),
    fileInput("wosFile", "Upload Web of Science File (.txt)", accept = c(".txt")),
    actionButton("processButton", "Process and Combine Data", 
                 class = "btn-primary btn-lg btn-block"),
    div(
      style = "margin-top: 20px;",
      uiOutput("downloadButtonUI")
    ),
    
    tags$hr(),
    uiOutput("downloadButtonUI"),
    tags$hr(),
    htmlOutput("statusOutput"),
    
  )
}

mainPanelUI <- function() {
  mainPanel(
    tabsetPanel(
      type = "tabs",
      # Add this UI component update for the Overview tab:
      tabPanel(
        "Overview",
        div(class = "container",
            style = "padding: 20px;",
            
            # Status Cards
            fluidRow(
              column(4,
                     div(class = "well", style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                         h4("Scopus Data", style = "color: #2C3E50;"),
                         textOutput("scopusStatus"),
                         uiOutput("scopusStats")
                     )
              ),
              column(4,
                     div(class = "well", style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                         h4("WoS Data", style = "color: #2C3E50;"),
                         textOutput("wosStatus"),
                         uiOutput("wosStats")
                     )
              ),
              column(4,
                     div(class = "well", style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                         h4("Combined Data", style = "color: #2C3E50;"),
                         textOutput("combinedStatus"),
                         uiOutput("combinedStats")
                     )
              )
            ),
            
            # Processing Information
            fluidRow(
              column(12,
                     div(class = "well", 
                         style = "background-color: #fff; padding: 20px; margin-top: 20px; border-radius: 5px; border: 1px solid #ddd;",
                         h4("Processing Information", style = "color: #2C3E50;"),
                         verbatimTextOutput("processingInfo")
                     )
              )
            )
        )
      ),
      
      tabPanel("Dataset Analysis",
               fluidRow(
                 column(6, 
                        h3("Scopus Dataset"),
                        DTOutput("scopusSummaryTable"),
                        plotlyOutput("scopusPlot")),
                 column(6, 
                        h3("WoS Dataset"),
                        DTOutput("wosSummaryTable"),
                        plotlyOutput("wosPlot"))
               )),
      
      tabPanel(
        "Combined Analysis",
        fluidRow(
          column(12,
                 h3("Combined Dataset Analysis"),
                 verbatimTextOutput("combinedSummary")
          )
        ),
        fluidRow(
          column(6,
                 h4("Publication Trend"),
                 plotOutput("combinedPlot")
          ),
          column(6,
                 h4("Document Types Distribution"),
                 verbatimTextOutput("documentTypes")
          )
        ),
        fluidRow(
          column(12,
                 h4("Sample of Combined Dataset (First 10 Records)"),
                 DTOutput("dataSample")
          )
        )
      ),
      
      tabPanel("Quality Metrics",
               h3("Data Quality Assessment"),
               fluidRow(
                 column(4, 
                        h4("Completeness Metrics"),
                        DTOutput("completenessTable")),
                 column(4,
                        h4("Consistency Metrics"),
                        DTOutput("consistencyTable")),
                 column(4,
                        h4("Mapping Accuracy"),
                        DTOutput("mappingTable"))
               )),
      tabPanel("System Performance",
        h3("System Performance Metrics"),
        verbatimTextOutput("performanceMetrics")
      )
    )
  )
}

# Enhanced UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .header { text-align: center; padding: 20px 0; }
      .title { font-weight: bold; color: #2C3E50; }
      .subtitle { color: #7F8C8D; }
      .btn-lg { margin: 10px 0; }
      .status-success { color: green; }
      .status-error { color: red; }
    "))
  ),
  headerUI(),
  sidebarLayout(
    sidebarUI(),
    mainPanelUI()
  ),
  tags$footer(
    tags$div(
      class = "footer text-center",
      style = "padding: 20px; border-top: 1px solid #ddd; margin-top: 30px;",
      tags$p(
        "Developed by Wirapong Chansanam",
        tags$br(),
        "Khon Kaen University, Thailand, 2024"
      )
    )
  )
)



# Server logic
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    scopus_data = NULL,
    wos_data = NULL,
    combined_data = NULL,
    performance = list(
      file_processing = list(),
      merge_time = NULL,
      analysis_time = NULL
    )
  )
  
  options(shiny.maxRequestSize = 1000*1024^2)  # Sets max size to 1000MB (1GB)
  
  rv$performance_metrics <- list()
  
  # Enhanced data processing functions
  processDataset <- function(file, type) {
    if (is.null(file)) return(NULL)
    
    withProgress(message = paste("Processing", type, "data..."), {
      tryCatch({
        data <- convert2df(file$datapath, 
                           dbsource = type,
                           format = FILE_TYPES[[type]]$format)
        
        # Data cleaning and standardization
        data <- data %>%
          mutate(
            PY = as.numeric(PY),
            TC = as.numeric(TC),
            DT = toupper(trimws(DT)),
            SO = trimws(SO)
          )
        
        return(data)
      }, error = function(e) {
        rv$processing_status <- list(
          type = "error",
          message = paste("Error processing", type, "file:", e$message)
        )
        return(NULL)
      })
    })
  }

  # Add these metric calculation functions to your script
  calculateMetrics <- function(data) {
    if (is.null(data)) return(NULL)
    
    # Extract and process author names with error handling
    tryCatch({
      # Check if AF column exists and contains data
      if ("AF" %in% names(data) && !all(is.na(data$AF))) {
        # For Scopus format
        author_counts <- table(trimws(unlist(strsplit(as.character(paste(data$AF, collapse = ";")), ";"))))
      } else {
        # For WoS format - use AU if AF is not available
        author_counts <- table(trimws(unlist(strsplit(as.character(paste(data$AU, collapse = ";")), ";"))))
      }
      # Sort in descending order and get top 10
      top_authors <- sort(author_counts, decreasing = TRUE)[1:10]
    }, error = function(e) {
      # If there's an error, create an empty table
      top_authors <- table(character(0))
    })
    
    metrics <- list(
      total_docs = nrow(data),
      year_range = paste(min(data$PY), "to", max(data$PY)),
      unique_sources = length(unique(data$SO)),
      unique_authors = length(unique(unlist(strsplit(data$AU, ";")))),
      doc_types = table(data$DT),
      top_authors = top_authors,
      languages = table(data$LA),
      avg_citations = mean(as.numeric(data$TC), na.rm = TRUE)
    )
    return(metrics)
  }
  
  generateMetricsReport <- function(scopus_data, wos_data, combined_data) {
    completeness <- list(
      total_docs = nrow(combined_data),
      year_range = paste(min(combined_data$PY), "to", max(combined_data$PY)),
      field_stats = list()
    )
    
    # Check field completeness
    fields <- c("AU", "TI", "SO", "PY", "DT", "TC")
    for (field in fields) {
      completeness$field_stats[[field]] <- list(
        complete = sum(!is.na(combined_data[[field]])),
        missing = sum(is.na(combined_data[[field]])),
        completeness_rate = mean(!is.na(combined_data[[field]])) * 100
      )
    }
    
    # Check data consistency
    consistency <- list(
      duplicate_titles = sum(duplicated(combined_data$TI)),
      year_range_valid = all(grepl("^\\d{4}$", as.character(combined_data$PY))),
      citation_completeness = mean(!is.na(combined_data$TC)) * 100
    )
    
    report <- list(
      completeness = completeness,
      consistency = consistency,
      summary = list(
        total_records = nrow(combined_data),
        unique_sources = length(unique(combined_data$SO)),
        unique_authors = length(unique(unlist(strsplit(combined_data$AU, ";")))),
        document_types = length(unique(combined_data$DT))
      )
    )
    
    return(report)
  }
  
  
  # Enhanced metrics calculation
  calculateEnhancedMetrics <- function(data) {
    if (is.null(data)) return(NULL)
    
    metrics <- list(
      basic = list(
        total_docs = nrow(data),
        year_range = range(data$PY, na.rm = TRUE),
        unique_sources = n_distinct(data$SO),
        unique_authors = n_distinct(unlist(strsplit(data$AU, ";")))
      ),
      quality = list(
        completeness = mean(!is.na(data[KEY_FIELDS])) * 100,
        consistency = mean(sapply(KEY_FIELDS, function(f) {
          length(unique(data[[f]])) / nrow(data)
        })) * 100
      ),
      temporal = table(data$PY),
      document_types = table(data$DT)
    )
    
    return(metrics)
  }
  
  # Reactive processors
  # Modify your file upload handlers to use performance tracking
  # Track Scopus file processing
  observeEvent(input$scopusFile, {
    req(input$scopusFile)
    
    # Start timing
    tic("scopus_processing")
    
    tryCatch({
      # Record start time and file size
      start_time <- Sys.time()
      file_size <- file.info(input$scopusFile$datapath)$size / (10240 * 10240) # Size in MB
      
      # Process file
      rv$scopus_data <- convert2df(input$scopusFile$datapath, 
                                   dbsource = "scopus", 
                                   format = "bibtex")
      
      # Record performance metrics
      end_time <- Sys.time()
      processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      rv$performance$file_processing$scopus <- list(
        file_size_mb = file_size,
        processing_time_sec = processing_time,
        records = nrow(rv$scopus_data),
        records_per_second = nrow(rv$scopus_data) / processing_time
      )
      
      # Update status
      output$statusOutput <- renderUI({
        HTML("<div style='color: green;'>Scopus file processed successfully!</div>")
      })
      
    }, error = function(e) {
      output$statusOutput <- renderUI({
        HTML(paste("<div style='color: red;'>Error processing Scopus file:", e$message, "</div>"))
      })
    })
    
    # Stop timing
    toc(log = TRUE, quiet = TRUE)
  })
  
  # Track WoS file processing
  observeEvent(input$wosFile, {
    req(input$wosFile)
    
    # Start timing
    tic("wos_processing")
    
    tryCatch({
      # Record start time and file size
      start_time <- Sys.time()
      file_size <- file.info(input$wosFile$datapath)$size / (1024 * 1024) # Size in MB
      
      # Create a connection to read the file in chunks
      con <- file(input$wosFile$datapath, "r")
      chunk_size <- 1024 * 1024 # 1MB chunks
      
      # Initialize empty list to store chunks
      chunks <- list()
      chunk_counter <- 1
      
      # Process file in chunks
      while (TRUE) {
        # Read chunk
        chunk <- readLines(con, n = chunk_size)
        
        if (length(chunk) == 0) break  # End of file
        
        # Process chunk
        chunk_df <- tryCatch({
          convert2df(textConnection(chunk),
                     dbsource = "isi",
                     format = "plaintext")
        }, error = function(e) NULL)
        
        if (!is.null(chunk_df)) {
          chunks[[chunk_counter]] <- chunk_df
          chunk_counter <- chunk_counter + 1
        }
        
        # Garbage collection to free memory
        gc()
      }
      
      # Close connection
      close(con)
      
      # Combine all chunks efficiently
      rv$wos_data <- do.call(rbind, chunks)
      
      # Clear chunks to free memory
      rm(chunks)
      gc()
      
      # Record performance metrics
      end_time <- Sys.time()
      processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      rv$performance$file_processing$wos <- list(
        file_size_mb = file_size,
        processing_time_sec = processing_time,
        records = nrow(rv$wos_data),
        records_per_second = nrow(rv$wos_data) / processing_time
      )
      
      # Update status with file size info
      output$statusOutput <- renderUI({
        HTML(sprintf(
          "<div style='color: green;'>WoS file (%.1f MB) processed successfully!</div>",
          file_size
        ))
      })
      
    }, error = function(e) {
      output$statusOutput <- renderUI({
        HTML(paste("<div style='color: red;'>Error processing WoS file:", e$message, "</div>"))
      })
    }, finally = {
      # Ensure connection is closed even if there's an error
      if (exists("con") && isOpen(con)) {
        close(con)
      }
    })
    
    # Stop timing
    toc(log = TRUE, quiet = TRUE)
  })
  
  # Track data processing and merging
  observeEvent(input$processButton, {
    req(input$processButton)
    
    if (is.null(rv$scopus_data) && is.null(rv$wos_data)) {
      output$statusOutput <- renderUI({
        HTML("<div style='color: red;'>Please upload at least one dataset</div>")
      })
      return()
    }
    
    # Start timing
    tic("data_processing")
    
    tryCatch({
      start_time <- Sys.time()
      
      # Combine data
      if (is.null(rv$scopus_data)) {
        rv$combined_data <- rv$wos_data
      } else if (is.null(rv$wos_data)) {
        rv$combined_data <- rv$scopus_data
      } else {
        rv$combined_data <- mergeDbSources(rv$wos_data, rv$scopus_data, 
                                           remove.duplicated = TRUE)
      }
      
      # Record merge performance
      end_time <- Sys.time()
      rv$performance$merge_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      # Start analysis timing
      analysis_start <- Sys.time()
      
      # Calculate metrics and generate report
      combined_metrics <- calculateMetrics(rv$combined_data)
      metrics_report <- generateMetricsReport(rv$scopus_data, rv$wos_data, rv$combined_data)
      
      # Record analysis performance
      analysis_end <- Sys.time()
      rv$performance$analysis_time <- as.numeric(difftime(analysis_end, analysis_start, units = "secs"))
      
      # Display performance metrics
      output$performanceMetrics <- renderPrint({
        cat("=== SYSTEM PERFORMANCE METRICS ===\n\n")
        
        cat("1. FILE PROCESSING PERFORMANCE\n")
        cat("---------------------------------\n")
        if (!is.null(rv$performance$file_processing$scopus)) {
          cat("Scopus Processing:\n")
          cat(sprintf("File Size: %.2f MB\n", rv$performance$file_processing$scopus$file_size_mb))
          cat(sprintf("Processing Time: %.2f seconds\n", rv$performance$file_processing$scopus$processing_time_sec))
          cat(sprintf("Records Processed: %d\n", rv$performance$file_processing$scopus$records))
          cat(sprintf("Processing Speed: %.2f records/second\n\n", rv$performance$file_processing$scopus$records_per_second))
        }
        
        if (!is.null(rv$performance$file_processing$wos)) {
          cat("WoS Processing:\n")
          cat(sprintf("File Size: %.2f MB\n", rv$performance$file_processing$wos$file_size_mb))
          cat(sprintf("Processing Time: %.2f seconds\n", rv$performance$file_processing$wos$processing_time_sec))
          cat(sprintf("Records Processed: %d\n", rv$performance$file_processing$wos$records))
          cat(sprintf("Processing Speed: %.2f records/second\n\n", rv$performance$file_processing$wos$records_per_second))
        }
        
        cat("2. DATA MERGING PERFORMANCE\n")
        cat("---------------------------\n")
        cat(sprintf("Merging Time: %.2f seconds\n", rv$performance$merge_time))
        cat(sprintf("Total Records After Merge: %d\n\n", nrow(rv$combined_data)))
        
        cat("3. ANALYSIS PERFORMANCE\n")
        cat("-----------------------\n")
        cat(sprintf("Analysis Time: %.2f seconds\n", rv$performance$analysis_time))
        cat(sprintf("Records Analyzed: %d\n", nrow(rv$combined_data)))
      })
      
      # Update status
      output$statusOutput <- renderUI({
        HTML("<div style='color: green;'>Data processed successfully!</div>")
      })
      
    }, error = function(e) {
      output$statusOutput <- renderUI({
        HTML(paste("<div style='color: red;'>Error processing data:", e$message, "</div>"))
      })
    })
    
    # Stop timing
    toc(log = TRUE, quiet = TRUE)
  })
  
  # Process and combine data
  observeEvent(input$processButton, {
    req(input$processButton)
    
    if (is.null(rv$scopus_data) && is.null(rv$wos_data)) {
      output$statusOutput <- renderUI({
        HTML("<div style='color: red;'>Please upload at least one dataset</div>")
      })
      return()
    }
    
    tryCatch({
      # Calculate total documents before merging
      total_before <- sum(
        if(!is.null(rv$scopus_data)) nrow(rv$scopus_data) else 0,
        if(!is.null(rv$wos_data)) nrow(rv$wos_data) else 0
      )
      
      # Combine data
      if (is.null(rv$scopus_data)) {
        rv$combined_data <- rv$wos_data
      } else if (is.null(rv$wos_data)) {
        rv$combined_data <- rv$scopus_data
      } else {
        rv$combined_data <- mergeDbSources(rv$wos_data, rv$scopus_data, 
                                           remove.duplicated = TRUE)
      }
      
      # Calculate metrics for combined data
      combined_metrics <- calculateMetrics(rv$combined_data)
      
      # Calculate duplicates
      duplicates <- total_before - nrow(rv$combined_data)
      
      # Update Combined Summary
      output$combinedSummary <- renderPrint({
        cat("Combined Dataset Summary:\n")
        cat("------------------------\n")
        cat("Total documents before merging:", total_before, "\n")
        cat("Total documents after merging:", nrow(rv$combined_data), "\n")
        cat("Duplicated records removed:", duplicates, "\n")
        cat("Duplication rate:", sprintf("%.2f%%", (duplicates/total_before)*100), "\n")
        cat("\nDataset Overview:\n")
        cat("----------------\n")
        cat("Year range:", combined_metrics$year_range, "\n")
        cat("Number of sources:", combined_metrics$unique_sources, "\n")
        cat("Number of unique authors:", combined_metrics$unique_authors, "\n")
        cat("Average citations per document:", sprintf("%.2f", combined_metrics$avg_citations), "\n")
      })
      
      # Update Document Types Distribution
      output$documentTypes <- renderPrint({
        cat("Document Types Distribution:\n")
        cat("--------------------------\n")
        doc_types_df <- as.data.frame(combined_metrics$doc_types)
        names(doc_types_df) <- c("Type", "Count")
        print(doc_types_df)
      })
      
      # Display sample of combined dataset
      output$dataSample <- renderDT({
        # Select important columns
        cols_to_show <- c("AU", "TI", "SO", "PY", "DT", "TC")
        sample_data <- head(rv$combined_data[, cols_to_show], 10)
        
        # Rename columns for better display
        colnames(sample_data) <- c("Authors", "Title", "Source", "Year", 
                                   "Document Type", "Times Cited")
        
        # Create interactive table
        datatable(sample_data,
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    dom = 'tp'
                  ),
                  rownames = FALSE,
                  class = 'cell-border stripe'
        )
      })
      
      # Create publication trend plot
      output$combinedPlot <- renderPlot({
        # Create publication trend over years
        pub_years <- table(rv$combined_data$PY)
        barplot(pub_years,
                main = "Publication Trend Over Years",
                xlab = "Year",
                ylab = "Number of Publications",
                col = "steelblue",
                border = "white",
                las = 2)  # Rotate x-axis labels
      })
      
      # Update status
      output$statusOutput <- renderUI({
        HTML("<div style='color: green;'>Data processed successfully!</div>")
      })
      
      # Show download button
      output$downloadButtonUI <- renderUI({
        downloadButton("downloadData", "Download Combined Dataset",
                       class = "btn btn-success")
      })
      
    }, error = function(e) {
      output$statusOutput <- renderUI({
        HTML(paste("<div style='color: red;'>Error processing data:", 
                   e$message, "</div>"))
      })
    })
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("combined-bibliometric-data-", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      write.xlsx(rv$combined_data, file)
    }
  )

  
  
  
  
  # Quality metrics outputs
  output$completenessTable <- renderDT({
    req(rv$combined_data)
    completeness_data <- analyzeDataCompleteness(rv$combined_data)
    datatable(completeness_data,
              options = list(dom = 't', ordering = FALSE),
              rownames = FALSE)
  })
  
  output$consistencyTable <- renderDT({
    req(rv$combined_data)
    consistency_data <- analyzeDataConsistency(rv$combined_data)
    datatable(consistency_data,
              options = list(dom = 't', ordering = FALSE),
              rownames = FALSE)
  })
  
  output$mappingTable <- renderDT({
    req(rv$combined_data, rv$scopus_data, rv$wos_data)
    mapping_data <- analyzeMapping(rv$scopus_data, rv$wos_data, rv$combined_data)
    datatable(mapping_data,
              options = list(dom = 't', ordering = FALSE),
              rownames = FALSE)
  })
  
  # Add these functions to your server logic:
  
  # Function to create summary data frame
  createDatasetSummary <- function(data, source) {
    if (is.null(data)) return(NULL)
    
    summary_data <- data.frame(
      Metric = c(
        "Total Documents",
        "Publication Years",
        "Unique Sources",
        "Unique Authors",
        "Average Citations",
        "Document Types"
      ),
      Value = c(
        nrow(data),
        paste(min(data$PY, na.rm = TRUE), "-", max(data$PY, na.rm = TRUE)),
        length(unique(data$SO)),
        length(unique(unlist(strsplit(data$AU, ";")))),
        round(mean(as.numeric(data$TC), na.rm = TRUE), 2),
        length(unique(data$DT))
      ),
      stringsAsFactors = FALSE
    )
    
    return(summary_data)
  }
  
  # Function to create publication trend plot
  createPublicationTrend <- function(data, source) {
    if (is.null(data)) return(NULL)
    
    # Create year-wise publication counts
    pub_trend <- as.data.frame(table(data$PY))
    names(pub_trend) <- c("Year", "Publications")
    pub_trend$Year <- as.numeric(as.character(pub_trend$Year))
    
    # Create plotly plot
    plot_ly(pub_trend, x = ~Year, y = ~Publications, type = "scatter", mode = "lines+markers",
            line = list(color = ifelse(source == "Scopus", "#00BFA5", "#2196F3")),
            marker = list(color = ifelse(source == "Scopus", "#00BFA5", "#2196F3"))) %>%
      layout(
        title = paste(source, "Publications by Year"),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Publications"),
        showlegend = FALSE
      )
  }
  
  # Update the server function:
  # Scopus Status
  output$scopusStatus <- renderText({
    if(is.null(rv$scopus_data)) {
      "Status: Not loaded"
    } else {
      "Status: Loaded ✓"
    }
  })
  
  output$scopusStats <- renderUI({
    if(!is.null(rv$scopus_data)) {
      HTML(paste0(
        "<div style='margin-top: 10px;'>",
        "<strong>Documents:</strong> ", nrow(rv$scopus_data), "<br/>",
        "<strong>Years:</strong> ", min(rv$scopus_data$PY), "-", max(rv$scopus_data$PY), "<br/>",
        "<strong>Sources:</strong> ", length(unique(rv$scopus_data$SO)),
        "</div>"
      ))
    }
  })
  
  # WoS Status
  output$wosStatus <- renderText({
    if(is.null(rv$wos_data)) {
      "Status: Not loaded"
    } else {
      "Status: Loaded ✓"
    }
  })
  
  output$wosStats <- renderUI({
    if(!is.null(rv$wos_data)) {
      HTML(paste0(
        "<div style='margin-top: 10px;'>",
        "<strong>Documents:</strong> ", nrow(rv$wos_data), "<br/>",
        "<strong>Years:</strong> ", min(rv$wos_data$PY), "-", max(rv$wos_data$PY), "<br/>",
        "<strong>Sources:</strong> ", length(unique(rv$wos_data$SO)),
        "</div>"
      ))
    }
  })
  
  # Combined Status
  output$combinedStatus <- renderText({
    if(is.null(rv$combined_data)) {
      "Status: Not processed"
    } else {
      "Status: Processed ✓"
    }
  })
  
  output$combinedStats <- renderUI({
    if(!is.null(rv$combined_data)) {
      HTML(paste0(
        "<div style='margin-top: 10px;'>",
        "<strong>Total Documents:</strong> ", nrow(rv$combined_data), "<br/>",
        "<strong>Year Range:</strong> ", min(rv$combined_data$PY), "-", max(rv$combined_data$PY), "<br/>",
        "<strong>Unique Sources:</strong> ", length(unique(rv$combined_data$SO)),
        "</div>"
      ))
    }
  })
  
  # Processing Information
  output$processingInfo <- renderPrint({
    cat("Processing Log:\n\n")
    
    # Scopus Info
    cat("1. Scopus Data:\n")
    if(!is.null(rv$scopus_data)) {
      cat("   - Successfully loaded\n")
      cat(sprintf("   - Contains %d documents\n", nrow(rv$scopus_data)))
    } else {
      cat("   - Not loaded\n")
    }
    cat("\n")
    
    # WoS Info
    cat("2. Web of Science Data:\n")
    if(!is.null(rv$wos_data)) {
      cat("   - Successfully loaded\n")
      cat(sprintf("   - Contains %d documents\n", nrow(rv$wos_data)))
    } else {
      cat("   - Not loaded\n")
    }
    cat("\n")
    
    # Combined Info
    cat("3. Combined Dataset:\n")
    if(!is.null(rv$combined_data)) {
      total_input <- sum(
        if(!is.null(rv$scopus_data)) nrow(rv$scopus_data) else 0,
        if(!is.null(rv$wos_data)) nrow(rv$wos_data) else 0
      )
      duplicates <- total_input - nrow(rv$combined_data)
      
      cat("   - Successfully processed\n")
      cat(sprintf("   - Total input documents: %d\n", total_input))
      cat(sprintf("   - Final unique documents: %d\n", nrow(rv$combined_data)))
      cat(sprintf("   - Duplicates removed: %d\n", duplicates))
      cat(sprintf("   - Duplication rate: %.2f%%\n", (duplicates/total_input)*100))
    } else {
      cat("   - Not processed\n")
    }
  })
  # Scopus summary
  output$scopusSummaryTable <- renderDT({
    req(rv$scopus_data)
    summary_data <- createDatasetSummary(rv$scopus_data, "Scopus")
    datatable(summary_data,
              options = list(dom = 't', 
                             ordering = FALSE,
                             pageLength = nrow(summary_data)),
              rownames = FALSE) %>%
      formatStyle(columns = 1:2,
                  backgroundColor = "#f8f9fa",
                  borderRadius = "4px")
  })
  
  # Scopus plot
  output$scopusPlot <- renderPlotly({
    req(rv$scopus_data)
    createPublicationTrend(rv$scopus_data, "Scopus")
  })
  
  # WoS summary
  output$wosSummaryTable <- renderDT({
    req(rv$wos_data)
    summary_data <- createDatasetSummary(rv$wos_data, "Web of Science")
    datatable(summary_data,
              options = list(dom = 't', 
                             ordering = FALSE,
                             pageLength = nrow(summary_data)),
              rownames = FALSE) %>%
      formatStyle(columns = 1:2,
                  backgroundColor = "#f8f9fa",
                  borderRadius = "4px")
  })
  
  # WoS plot
  output$wosPlot <- renderPlotly({
    req(rv$wos_data)
    createPublicationTrend(rv$wos_data, "Web of Science")
  })
  
  # Update the UI part for Dataset Analysis tab:
  tabPanel(
    "Dataset Analysis",
    fluidRow(
      column(6,
             h3("Scopus Dataset Analysis"),
             div(style = "margin: 20px;",
                 DTOutput("scopusSummaryTable")
             ),
             div(style = "margin: 20px; height: 400px;",
                 plotlyOutput("scopusPlot")
             )
      ),
      column(6,
             h3("WoS Dataset Analysis"),
             div(style = "margin: 20px;",
                 DTOutput("wosSummaryTable")
             ),
             div(style = "margin: 20px; height: 400px;",
                 plotlyOutput("wosPlot")
             )
      )
    )
  )
  
  
  # Show/Hide download button based on combined data availability
  output$downloadButtonUI <- renderUI({
    if (!is.null(rv$combined_data)) {
      div(
        style = "width: 100%;",
        downloadButton(
          "downloadData",
          label = "Download Combined Dataset",
          icon = icon("download"),
          class = "btn-success btn-block",
          style = "width: 100%; margin-bottom: 10px;"
        ),
        div(
          style = "text-align: center; font-size: 12px; color: #666;",
          paste("Total records:", nrow(rv$combined_data))
        )
      )
    }
  })
  
  # Download handler with multiple format options
  output$downloadData <- downloadHandler(
    filename = function() {
      # Create filename with timestamp
      paste0("bibliometric-data-", format(Sys.time(), "%Y%m%d-%H%M"), ".xlsx")
    },
    content = function(file) {
      withProgress(
        message = 'Preparing download',
        detail = 'This may take a while...',
        value = 0,
        {
          # Update progress
          incProgress(0.3, detail = "Processing data...")
          
          # Create workbook
          wb <- createWorkbook()
          
          # Add combined data sheet
          incProgress(0.3, detail = "Creating combined dataset...")
          addWorksheet(wb, "Combined Data")
          writeData(wb, "Combined Data", rv$combined_data)
          
          # Add metadata sheet
          incProgress(0.2, detail = "Adding metadata...")
          addWorksheet(wb, "Metadata")
          metadata <- data.frame(
            Field = c("Total Records", "Date Generated", "Source Files",
                      "Scopus Records", "WoS Records", "Year Range"),
            Value = c(
              nrow(rv$combined_data),
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              paste(input$scopusFile$name, input$wosFile$name, sep = "; "),
              if(!is.null(rv$scopus_data)) nrow(rv$scopus_data) else 0,
              if(!is.null(rv$wos_data)) nrow(rv$wos_data) else 0,
              paste(min(rv$combined_data$PY), "-", max(rv$combined_data$PY))
            )
          )
          writeData(wb, "Metadata", metadata)
          
          # Style the workbook
          incProgress(0.1, detail = "Applying formatting...")
          # Style for headers
          headerStyle <- createStyle(
            textDecoration = "bold",
            fgFill = "#E2E2E2"
          )
          
          # Apply styles
          addStyle(wb, "Combined Data", headerStyle, rows = 1, cols = 1:ncol(rv$combined_data))
          addStyle(wb, "Metadata", headerStyle, rows = 1, cols = 1:2)
          
          # Auto-size columns
          setColWidths(wb, "Combined Data", cols = 1:ncol(rv$combined_data), "auto")
          setColWidths(wb, "Metadata", cols = 1:2, "auto")
          
          # Save workbook
          incProgress(0.1, detail = "Saving file...")
          saveWorkbook(wb, file, overwrite = TRUE)
        }
      )
    }
  )
  
  
  # Function to analyze data completeness
  analyzeDataCompleteness <- function(data) {
    if (is.null(data)) return(NULL)
    
    fields <- c("AU", "TI", "SO", "PY", "DT", "TC")
    completeness <- sapply(fields, function(field) {
      if (field %in% names(data)) {
        complete <- sum(!is.na(data[[field]]) & data[[field]] != "") / nrow(data) * 100
        return(round(complete, 2))
      } else {
        return(0)
      }
    })
    
    data.frame(
      Field = fields,
      Completeness = paste0(completeness, "%"),
      stringsAsFactors = FALSE
    )
  }
  
  # Function to analyze data consistency
  analyzeDataConsistency <- function(data) {
    if (is.null(data)) return(NULL)
    
    # Check various consistency metrics
    year_format <- sum(grepl("^\\d{4}$", as.character(data$PY))) / nrow(data) * 100
    author_format <- sum(grepl("^[^0-9]+$", unlist(strsplit(as.character(data$AU), ";")))) / 
      length(unlist(strsplit(as.character(data$AU), ";"))) * 100
    title_dupls <- sum(duplicated(tolower(trimws(data$TI))))
    
    data.frame(
      Metric = c("Year Format", "Author Names", "Title Duplicates"),
      Value = c(
        paste0(round(year_format, 2), "% valid"),
        paste0(round(author_format, 2), "% valid"),
        paste(title_dupls, "found")
      ),
      stringsAsFactors = FALSE
    )
  }
  
  # Function to analyze mapping accuracy
  analyzeMapping <- function(scopus_data, wos_data, combined_data) {
    if (is.null(combined_data)) return(NULL)
    
    # Calculate mapping metrics
    title_match <- sum(combined_data$TI %in% c(scopus_data$TI, wos_data$TI)) / nrow(combined_data) * 100
    source_match <- sum(combined_data$SO %in% c(scopus_data$SO, wos_data$SO)) / nrow(combined_data) * 100
    year_match <- sum(combined_data$PY %in% c(scopus_data$PY, wos_data$PY)) / nrow(combined_data) * 100
    
    data.frame(
      Field = c("Titles", "Sources", "Years"),
      Accuracy = paste0(round(c(title_match, source_match, year_match), 2), "%"),
      stringsAsFactors = FALSE
    )
  }
  
  # Update displays
  updateMetricsDisplays <- function(type) {
    if (type == "combined") {
      metrics <- calculateEnhancedMetrics(rv$combined_data)
      output$combinedSummaryTable <- renderDT({
        datatable(
          data.frame(
            Metric = names(metrics$basic),
            Value = unlist(metrics$basic)
          ),
          options = list(dom = 't')
        )
      })
      
      # Update plots
      output$combinedYearPlot <- renderPlotly({
        plot_ly(
          x = names(metrics$temporal),
          y = as.numeric(metrics$temporal),
          type = "bar",
          name = "Publications"
        ) %>%
          layout(title = "Publications by Year",
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Number of Publications"))
      })
    }
  }
  
  # Status output
  output$statusOutput <- renderUI({
    if (!is.null(rv$processing_status)) {
      div(
        class = paste0("status-", rv$processing_status$type),
        rv$processing_status$message
      )
    }
  })
  
  # Download handler
  output$downloadButtonUI <- renderUI({
    if (!is.null(rv$combined_data)) {
      downloadButton(
        "downloadData",
        "Download Combined Dataset",
        class = "btn-success btn-block"
      )
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("combined-bibliometric-data-", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      write.xlsx(rv$combined_data, file)
    }
  )
}



# Run the application
shinyApp(ui = ui, server = server)