## Version for viewing speed, position, and course errors during the survey....
library(shiny)
library(NeAtlIBTS64)
library(geosphere)

# Helper to normalize column names
normalize_names <- function(df) {
    if (is.null(df)) return(NULL)
    nms <- names(df); low <- tolower(nms)
    repl <- function(target, new) {
        i <- which(low == tolower(target))
        if (length(i) == 1) names(df)[i] <<- new
    }
    repl("survey","Survey"); repl("year","Year"); repl("quarter","Quarter")
    repl("haulno","HaulNo"); repl("haullong","HaulLong"); repl("haullat","HaulLat")
    repl("shootlong","ShootLong"); repl("shootlat","ShootLat")
    repl("country","Country"); repl("towdir","TowDir")
    repl("distance","Distance"); repl("groundspeed","GroundSpeed"); repl("hauldur","HaulDur")
    df
}

# Calculate error columns as in qcHaulsDist
compute_qc_errors <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    numify <- function(x) suppressWarnings(as.numeric(x))
    df$HaulNo      <- numify(df$HaulNo)
    df$Year        <- numify(df$Year)
    df$Quarter     <- numify(df$Quarter)
    df$Distance    <- numify(df$Distance)
    df$GroundSpeed <- numify(df$GroundSpeed)
    df$HaulDur     <- numify(df$HaulDur)
    df$TowDir      <- numify(df$TowDir)
    df$ShootLong   <- numify(df$ShootLong)
    df$ShootLat    <- numify(df$ShootLat)
    df$HaulLong    <- numify(df$HaulLong)
    df$HaulLat     <- numify(df$HaulLat)

    dist_vel <- (df$HaulDur/60) * df$GroundSpeed * 1852
    ok_hf <- !is.na(df$ShootLong) & !is.na(df$ShootLat) & !is.na(df$HaulLong) & !is.na(df$HaulLat)
    dist_hf <- rep(NA_real_, nrow(df))
    if (any(ok_hf)) {
        dist_hf[ok_hf] <- distHaversine(as.matrix(df[ok_hf, c("ShootLong","ShootLat")]),
                                        as.matrix(df[ok_hf, c("HaulLong","HaulLat")]))
    }
    vel_dist <- (dist_hf/1852) / (df$HaulDur/60)

    err_dist <- ifelse(!is.na(df$Distance) & df$Distance != 0,
                       (dist_hf - df$Distance) * 100 / df$Distance, NA_real_)
    err_vel  <- ifelse(!is.na(df$Distance) & df$Distance != 0,
                       (dist_vel - df$Distance) * 100 / df$Distance, NA_real_)

    rumb <- rep(NA_real_, nrow(df))
    if (any(ok_hf)) {
        rumb[ok_hf] <- bearingRhumb(as.matrix(df[ok_hf, c("ShootLong","ShootLat")]),
                                    as.matrix(df[ok_hf, c("HaulLong","HaulLat")]))
    }
    err_rumb <- ifelse(!is.na(rumb) & !is.na(df$TowDir), rumb - df$TowDir, NA_real_)

    df$dist.vel   <- dist_vel
    df$dist.hf    <- round(dist_hf)
    df$vel.dist   <- round(vel_dist, 1)
    df$error.dist <- round(err_dist, 2)
    df$error.vel  <- round(err_vel, 2)
    df$rumb       <- round(rumb, 1)
    df$error.rumb <- round(err_rumb, 1)
    df
}

# ====== GENERAL SETTINGS ======
# Columns to display in each table (you can easily edit them)
cols_dist <- c("Survey","Year","Quarter","Country","HaulNo",
               "Distance","GroundSpeed","HaulDur","TowDir",
               "dist.hf","dist.vel","vel.dist",
               "error.dist","error.vel","rumb","error.rumb")

cols_posit <- c("Survey","Year","Quarter","Country","HaulNo",
                "ShootLat","ShootLong","HaulLat","HaulLong",
                "Distance","GroundSpeed","HaulDur","TowDir")

cols_map <- c("Survey","Year","Quarter","Country","HaulNo",
              "SweepLngt","HaulLong","HaulLat",
              "Distance","GroundSpeed","HaulDur","TowDir")

# ====== AUXILIAR FUNCTION ======
format_table_data <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    # convert and round types
    num_to_int <- c("Year","Quarter","HaulNo","HaulDur","dist.hf","TowDir")
    num_round2 <- c("error.dist","error.vel")
    num_round1 <- c("rumb","error.rumb")

    for (v in intersect(num_to_int, names(df)))
        df[[v]] <- as.integer(round(as.numeric(df[[v]])))
    for (v in intersect(num_round2, names(df)))
        df[[v]] <- round(as.numeric(df[[v]]), 2)
    for (v in intersect(num_round1, names(df)))
        df[[v]] <- round(as.numeric(df[[v]]), 1)

    df
}

ui <- tagList(

  # ====== Corporate header with logo ======
  div(
    style = "
            display: flex;
            align-items: center;
            justify-content: flex-start;
            background-color: #e8f6f3;
            border-bottom: 3px solid #117864;
            padding: 12px 20px;
        ",

    # ---- LOGO (use file in  www/logo.png) ----
    tags$img(
      src = "IMBUS_logo.png",
      height = "80px",
      style = "margin-right: 25px;"
    ),

    # ---- Tittle and subtittle ----
    div(
      h2(
        "Quality Control — Upload HH data from current survey",
        style = "margin: 0; color: #0b5345; font-weight: bold;"
      ),
      p(
        "Distance, speed, course and position checking tools",
        style = "margin: 0; font-size: 16px; color: #117864;"
      )
    )
  ),

  # ====== Main body with tabs ======
  navbarPage(
    title = div(tags$span("QC Tools")),
    id = "mainnav",
    theme = bslib::bs_theme(version = 5, bootswatch = "minty"),

    tabPanel("Upload data",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_global", "Load a CSV file (HH)", accept = ".csv"),
                 helpText("Load your HH file. Then go to QC Dist / QC Posit / Map Hist.")
               ),
               mainPanel(
                 verbatimTextOutput("info_global")
               )
             )
    ),

    tabPanel("QC Dist",
             sidebarLayout(
               sidebarPanel(
                 selectInput("errorType_dist", "Error type:",
                             choices = c("Dist", "Speed", "Course"), selected = "Dist"),
                 numericInput("pcerror_dist", "Acceptable error (%):", value = 2, min = 0, step = 0.1)
               ),
               mainPanel(
                 verbatimTextOutput("info_dist"),
                 plotOutput("plot_dist", click = "click_dist", width = "100%", height = "700px"),
                 DT::dataTableOutput("dist_click_info")
               )
             )
    ),

    tabPanel("QC Posit",
             sidebarLayout(
               sidebarPanel(
                 selectInput("graf", "Type:", c("Tracks","Points"), "Tracks")
               ),
               mainPanel(
                 verbatimTextOutput("info_posit"),
                 plotOutput("plot_posit", click = "click_posit", width = "100%", height = "700px"),
                 DT::dataTableOutput("posit_click_info")
               )
             )
    ),

    tabPanel("Map Hist",
             sidebarLayout(
               sidebarPanel(
                 selectInput("type_map", "Type:", c("Sweeps","Country"), "Sweeps")
               ),
               mainPanel(
                 verbatimTextOutput("info_map"),
                 plotOutput("plot_map", click = "click_map", width = "100%", height = "700px"),
                 DT::dataTableOutput("map_click_info")
               )
             )
    )
  )
)

server <- function(input, output, session) {

    datos_global <- reactiveVal(NULL)

    observeEvent(input$file_global, {
        req(input$file_global)
        df <- read.csv(input$file_global$datapath, stringsAsFactors = FALSE)
        df <- normalize_names(df)
        datos_global(df)
    })

    output$info_global <- renderPrint({
        df <- datos_global()
        if (is.null(df)) cat("⚠️ No file uploaded yet.")
        else list(
            N_registros = nrow(df),
            Surveys = unique(df$Survey),
            Years = unique(df$Year),
            Quarters = unique(df$Quarter)
        )
    })

    # ----- QC DIST -----
    selected_dist <- reactiveVal(NULL)

    # ====== QC DIST ======
    output$dist_click_info <- DT::renderDT({
        sel <- selected_dist()
        if (is.null(sel) || nrow(sel) == 0) return(NULL)
        sel <- format_table_data(sel)
        keep <- intersect(cols_dist, names(sel))
        DT::datatable(sel[, keep, drop = FALSE],
                      options = list(dom = 't', paging = FALSE, scrollX = TRUE),
                      rownames = FALSE)
    })

    output$plot_dist <- renderPlot({
        df <- datos_global()
        validate(need(!is.null(df), "⚠️ Upload a file in 'Upload data' first."))

        # Take first Survey/Year/Quarter found in the file
        sr <- unique(na.omit(df$Survey))[1]
        yr <- unique(na.omit(df$Year))[1]
        qt <- unique(na.omit(df$Quarter))[1]

        df_sub <- subset(df, Survey == sr & Year == yr & Quarter == qt)
        validate(need(nrow(df_sub) > 0, "No records found for the selection (Survey/Year/Quarter detected)."))

        df_plot <- compute_qc_errors(df_sub)

        # call the official function with the data already processed
        NeAtlIBTS64::qcHaulsDist(
            Survey = df_plot,
            years = yr,
            quarter = qt,
            getICES = FALSE,
            error = input$errorType_dist,
            pc.error = input$pcerror_dist
        )

        # Stress the selection if it exists
        sel <- selected_dist()
        if (!is.null(sel) && nrow(sel) >= 1) {
            ev <- switch(input$errorType_dist,
                         "Dist" = "error.dist", "Speed" = "error.vel", "Course" = "error.rumb")
            if (ev %in% names(sel) && !is.na(sel[[ev]]) && !is.na(sel$HaulNo)) {
                points(as.numeric(sel$HaulNo), as.numeric(sel[[ev]]), pch = 21, bg = "green", cex = 2.4)
            }
        }
    })

    observeEvent(input$click_dist, {
        df <- datos_global()
        validate(need(!is.null(df), "⚠️ Upload a file in 'Upload data' first."))
        sr <- unique(na.omit(df$Survey))[1]
        yr <- unique(na.omit(df$Year))[1]
        qt <- unique(na.omit(df$Quarter))[1]
        df_sub <- subset(df, Survey == sr & Year == yr & Quarter == qt)
        df_plot <- compute_qc_errors(df_sub)

        ev <- switch(input$errorType_dist,
                     "Dist" = "error.dist", "Speed" = "error.vel", "Course" = "error.rumb")
        # Ensure data are numerical
        if (!("HaulNo" %in% names(df_plot)) || !(ev %in% names(df_plot))) {
            selected_dist(NULL); return()
        }
        df_plot$HaulNo <- suppressWarnings(as.numeric(df_plot$HaulNo))
        df_plot[[ev]]  <- suppressWarnings(as.numeric(df_plot[[ev]]))

        df_click <- df_plot[!is.na(df_plot$HaulNo) & !is.na(df_plot[[ev]]), ]
        near <- nearPoints(df_click, input$click_dist, xvar = "HaulNo", yvar = ev,
                           maxpoints = 1, addDist = TRUE, threshold = 20)
        if (nrow(near) == 1) selected_dist(near) else selected_dist(NULL)
    })


    # ----- QC POSIT -----
    selected_posit <- reactiveVal(NULL)
    output$info_posit <- renderPrint({
        df <- datos_global(); validate(need(!is.null(df), "⚠️ Upload a file in 'Upload data' first."))
        list(Surveys = unique(df$Survey), Years = unique(df$Year), Quarters = unique(df$Quarter))
    })

    output$plot_posit <- renderPlot({
        df <- datos_global(); validate(need(!is.null(df), "⚠️ Upload a file in 'Upload data' first."))
        NeAtlIBTS64::qcHaulsPosit(Survey = df, years = unique(df$Year), quarter = unique(df$Quarter), getICES = FALSE, Hpoints = (input$graf == "Points"), ti = FALSE)
        sel <- selected_posit()
        if (!is.null(sel) && nrow(sel) >= 1) points(sel$HaulLong, sel$HaulLat, pch = 21, bg = "green", cex = 2)
    })

    observeEvent(input$click_posit, {
        df <- datos_global(); validate(need(!is.null(df), "⚠️ Upload a file in 'Upload data' first."))
        df$HaulLong <- suppressWarnings(as.numeric(df$HaulLong)); df$HaulLat <- suppressWarnings(as.numeric(df$HaulLat))
        df2 <- df[!is.na(df$HaulLong) & !is.na(df$HaulLat), ]
        near <- nearPoints(df2, input$click_posit, xvar = "HaulLong", yvar = "HaulLat", maxpoints = 1, addDist = TRUE, threshold = 12)
        if (nrow(near) == 1) selected_posit(near)
    })

    output$posit_click_info <- DT::renderDT({
        sel <- selected_posit()
        if (is.null(sel) || nrow(sel) == 0) return(NULL)
        sel <- format_table_data(sel)
        keep <- intersect(cols_posit, names(sel))
        DT::datatable(sel[, keep, drop = FALSE],
                      options = list(dom = 't', paging = FALSE, scrollX = TRUE),
                      rownames = FALSE)
    })

    # ----- MAP HIST -----
    selected_map <- reactiveVal(NULL)
    output$info_map <- renderPrint({
        df <- datos_global(); validate(need(!is.null(df), "⚠️ Upload a file in 'Upload data' first."))
        list(Surveys = unique(df$Survey), Years = unique(df$Year), Quarters = unique(df$Quarter))
    })

    output$plot_map <- renderPlot({
        df <- datos_global()
        validate(need(!is.null(df), "⚠️ Upload a file in 'Upload data' first."))

        sr <- unique(na.omit(df$Survey))[1]
        yr <- unique(na.omit(df$Year))[1]
        qt <- unique(na.omit(df$Quarter))[1]

        df$HaulLong <- suppressWarnings(as.numeric(df$HaulLong))
        df$HaulLat  <- suppressWarnings(as.numeric(df$HaulLat))

        df_sub <- subset(df, Survey == sr & Year == yr & Quarter == qt)
        validate(need(nrow(df_sub) > 0, "No records found for selected Survey/Year/Quarter."))

        NeAtlIBTS64::SurveyMap.IBTS(
            Survey = df_sub,     # 👈 aquí el data.frame, no el texto
            Year = yr,
            Quarter = qt,
            sweeplngt = (input$type_map == "Sweeps"),
            country = (input$type_map == "Country"),
            getICES = FALSE
        )

        sel <- selected_map()
        if (!is.null(sel)) points(sel$HaulLong, sel$HaulLat, pch = 21, bg = "green", cex = 2)
    })

    observeEvent(input$click_map, {
        df <- datos_global(); df$HaulLong <- suppressWarnings(as.numeric(df$HaulLong)); df$HaulLat <- suppressWarnings(as.numeric(df$HaulLat))
        df2 <- df[!is.na(df$HaulLong) & !is.na(df$HaulLat), ]
        near <- nearPoints(df2, input$click_map, xvar = "HaulLong", yvar = "HaulLat", maxpoints = 1, addDist = TRUE, threshold = 12)
        if (nrow(near) == 1) selected_map(near)
    })

    # ====== MAP HIST ======
    output$map_click_info <- DT::renderDT({
        sel <- selected_map()
        if (is.null(sel) || nrow(sel) == 0) return(NULL)
        sel <- format_table_data(sel)
        keep <- intersect(cols_map, names(sel))
        DT::datatable(sel[, keep, drop = FALSE],
                      options = list(dom = 't', paging = FALSE, scrollX = TRUE),
                      rownames = FALSE)
    })

}

shinyApp(ui, server)
