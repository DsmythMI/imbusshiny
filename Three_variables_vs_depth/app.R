library(shiny)
library(bslib)
library(DT)
library(dplyr)

ui <- fluidPage(

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
        tags$img(
            src = "IMBUS_logo.png",
            height = "80px",
            style = "margin-right: 25px;"
        ),
        div(
            h2(
                "HH Gear Quality Control",
                style = "margin: 0; color: #0b5345; font-weight: bold;"
            ),
            p(
                "Warp length, net opening, wingspread, doorspread vs depth",
                style = "margin: 0; font-size: 16px; color: #117864;"
            )
        )
    ),

    # ====== HEADING (this ALWAYS goes inside fluidPage, not inside the div) ======
    theme = bs_theme(version = 5, bootswatch = "minty"),

    # ======  Principal layout ======
    sidebarLayout(

        sidebarPanel(
            h4("Upload HH files"),
            fileInput("hh_hist", "Time series (HH)", accept = c(".csv", ".txt")),
            fileInput("hh_current", "Current survey (HH)", accept = c(".csv", ".txt")),
            hr(),
            selectInput("yvar", "Variable to plot vs Depth:",
                        choices = c("WingSpread", "DoorSpread", "Netopening", "Warplngt"),
                        selected = "WingSpread"),
            helpText("Regression is y ~ log(Depth). Regression is plotted only within the range of Depth for each SweepLngt.")
        ),

        mainPanel(
            h3("Depth vs selected variable (Time series + Current survey)"),
            plotOutput("gearplot", width = "100%", height = "600px", click = "plot_click"),
            h4("Selected point:"),
            DTOutput("selected_table"),
            hr(),
            h4("Preview current survey"),
            DTOutput("current_table")
        )
    )
)

server <- function(input, output, session) {

    # read time series HH file
    hist_data <- reactive({
        req(input$hh_hist)
        df <- read.csv(input$hh_hist$datapath, stringsAsFactors = FALSE)
        # normalize variable names
        names(df) <- make.names(names(df))
        validate(
            need("Depth" %in% names(df), "Time series file does not have Depth column")
        )
        df <- dplyr::filter(df, Depth > 0 & HaulVal == "V")
        df
    })

    # Read current HH file
    current_data <- reactive({
        req(input$hh_current)
        df <- read.csv(input$hh_current$datapath, stringsAsFactors = FALSE)
        names(df) <- make.names(names(df))
        validate(
            need("Depth" %in% names(df), "Current file does not have Depth column")
        )
        df <- dplyr::filter(df, Depth > 0 & HaulVal == "V")
        df
    })

    selected_row <- reactiveVal(NULL)

    output$gearplot <- renderPlot({
        req(hist_data())
        hist <- hist_data()
        yvar <- input$yvar

        # check column exists in time series file
        validate(need(yvar %in% names(hist), paste0("Time series file does not have column '", yvar, "'.") ))

        # check numeric
        hist[[yvar]] <- suppressWarnings(as.numeric(hist[[yvar]]))
        hist$Depth <- suppressWarnings(as.numeric(hist$Depth))

        # filter positive values for yvar (discard -9/NA/<=0)
        hist <- hist[!is.na(hist[[yvar]]) & hist[[yvar]] > 0 & !is.na(hist$Depth) & hist$Depth > 0, ]

        # current data (if there are)
        cur <- NULL
        if (!is.null(input$hh_current) && !is.null(current_data())) {
            cur <- current_data()
            # Warn if the variable does not exist in the current file.
            validate(need(yvar %in% names(cur), paste0("Current file does not have column '", yvar, "'.") ))
            cur[[yvar]] <- suppressWarnings(as.numeric(cur[[yvar]]))
            cur$Depth <- suppressWarnings(as.numeric(cur$Depth))
            cur <- cur[!is.na(cur[[yvar]]) & cur[[yvar]] > 0 & !is.na(cur$Depth) & cur$Depth > 0, ]
        }

        # Joint ranges for axes: we want Depth on X and yvar on Y
        all_x <- c(hist$Depth, if (!is.null(cur)) cur$Depth)
        all_y <- c(hist[[yvar]], if (!is.null(cur)) cur[[yvar]])
        xlim <- if (length(all_x)>0) range(all_x, na.rm = TRUE) else c(0,1)
        ylim <- if (length(all_y)>0) range(all_y, na.rm = TRUE) else c(0,1)

        # Draw empty frame: Depth en x, variable en y
        plot(NA, xlim = xlim, ylim = ylim,
             xlab = "Depth (m)", ylab = yvar,
             main = paste("Depth vs", yvar),
             cex.lab = 1.4, cex.axis = 1.2, cex.main = 1.6)

        # If there is SweepLngt in time series, separate by sweep and draw points + regression in the actual range.
        if ("SweepLngt" %in% names(hist)) {
            tipos <- sort(unique(na.omit(hist$SweepLngt)))
            # assign colors (we support up to 2 types; if there are more, colors are rotated)
            pal <- c("steelblue", "darkorange", "darkgreen", "purple")
            cols <- pal[1:length(tipos)]
            labels <- character(0)
            for (i in seq_along(tipos)) {
                t <- tipos[i]
                sub <- subset(hist, SweepLngt == t & !is.na(Depth) & !is.na(hist[[yvar]]) )
                if (nrow(sub) == 0) next
                # historic points with these sweeps
                points(sub$Depth, sub[[yvar]], pch = 21, bg = cols[i], col = "black")
                # Calculate depth range used with this sweeps
                rango <- range(sub$Depth, na.rm = TRUE)
                # if there are enough data, fit y ~ log(Depth or ) only plot in the real range
                # calcular el modelo según la variable
                if (nrow(sub) > 5 && (rango[2] - rango[1]) > 0) {
                    # The formula depends on the type of variable selected
                    if (yvar == "Warplngt") {
                        fmla <- as.formula(paste(yvar, "~ Depth"))
                    } else {
                        fmla <- as.formula(paste(yvar, "~ log(Depth)"))
                    }

                    mod <- tryCatch(lm(fmla, data = sub), error = function(e) NULL)
                    if (!is.null(mod)) {
                        dseq <- seq(rango[1], rango[2], length.out = 250)
                        preds <- predict(mod, newdata = data.frame(Depth = dseq))
                        lines(dseq, preds, col = cols[i], lwd = 4)

                        labels <- c(labels, paste0("SweepLngt=", t,
                                                   " (", round(rango[1]), "–", round(rango[2]), " m)",
                                                   if (yvar == "Warplngt") " [linear]" else " [log]"))
                    }
                } else {
                    labels <- c(labels, paste0("SweepLngt=", t, " (", nrow(sub), " pts)"))
                }            }
            if (length(labels) > 0) {
                legend("topright", legend = labels, col = cols[seq_along(tipos)], lwd = 4,
                       pt.bg = cols[seq_along(tipos)], pch = 21, bty = "n", cex = 0.95)
            }
        } else {
            # if there's no SweepLngt, draw all in grey
            points(hist$Depth, hist[[yvar]], pch = 21, bg = "grey", col = "black")
        }

        # Overlay current survey points (red)
        if (!is.null(cur) && nrow(cur) > 0) {
            points(cur$Depth, cur[[yvar]], pch = 21, bg = "red", col = "red", cex = 1.4)
        }

        # Highlight selected point (large green)
        sel <- selected_row()
        if (!is.null(sel)) {
            sx <- suppressWarnings(as.numeric(sel$Depth))
            sy <- suppressWarnings(as.numeric(sel[[yvar]]))
            if (!is.na(sx) && !is.na(sy)) points(sx, sy, pch = 21, bg = "green", col = "black", cex = 2.2)
        }

        # Add text with n of points
        ntxt <- paste0("Hist: ", nrow(hist), " pts",
                       if (!is.null(cur)) paste0("  |  Current: ", nrow(cur), " pts") else "")
        mtext(ntxt, side = 1, line = -1.2, adj = 0.01, cex = 0.9)
    })

    # Detect clicks on points in the current file
    observeEvent(input$plot_click, {
        req(current_data())
        cur <- current_data()
        yvar <- input$yvar
        validate(need(yvar %in% names(cur), "Column not available in this file."))
        cur[[yvar]] <- suppressWarnings(as.numeric(cur[[yvar]]))
        cur$Depth <- suppressWarnings(as.numeric(cur$Depth))
        cur <- cur[!is.na(cur[[yvar]]) & cur[[yvar]] > 0 & !is.na(cur$Depth) & cur$Depth > 0, ]

        near <- nearPoints(cur, input$plot_click,
                           xvar = "Depth", yvar = yvar,
                           threshold = 8, maxpoints = 1, addDist = TRUE)
        if (nrow(near) == 1) selected_row(near) else selected_row(NULL)
    })

    output$selected_table <- renderDT({
        sel <- selected_row()
        if (is.null(sel) || nrow(sel) == 0) return(NULL)
        datatable(sel, options = list(dom = 't', paging = FALSE))
    })

    output$current_table <- renderDT({
        req(current_data())
        datatable(current_data(), options = list(pageLength = 8, scrollX = TRUE))
    })
}

shinyApp(ui, server)
