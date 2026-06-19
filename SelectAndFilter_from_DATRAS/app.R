library(shiny)
library(icesDatras)
library(NeAtlIBTS64)
library(bslib)
library(DT)

# ----- UI -----
ui <- fluidPage(

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

    theme = bs_theme(version = 5, bootswatch = "minty"),
    titlePanel("HH selector — download from DATRAS, inspect, remove outliers, save"),

    sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput("survey", "Survey:", choices = c("EVHOE","FR-CGFS","FR-WCGFS","IE-IGFS","IE-IAMS",
                                                         "NIGFS","NS-IBTS","PT-IBTS","SCOROC","SCOWCGFS",
                                                         "SP-ARSA","SP-NORTH","SP-PORC"), selected = "SP-PORC"),
            sliderInput("years", "Years (range):", min = 1983, max = 2025, value = c(2015, 2022), step = 1, sep = ""),
            checkboxGroupInput("quarters", "Quarters:", choices = 1:4, selected = 4, inline = TRUE),
            actionButton("btn_download", "⬇️ Download HH from DATRAS"),
            hr(),
            actionButton("btn_delete", "🗑️ Delete selected rows", class = "btn-danger"),
            actionButton("btn_restore", "↺ Restore original"),
            hr(),
            downloadButton("download_clean", "Download cleaned CSV"),
            hr(),
            helpText("Notes: -9 values are converted to NA automatically. Selection toggles on click.")
        ),

        mainPanel(
            tabsetPanel(id = "plots_tabs",
                        tabPanel("WarpLength vs Depth",
                                 value = "warp",
                                 plotOutput("plot_warp", height = "550px", click = "click_plot"),
                                 h4("Selected rows (current dataset):"),
                                 DTOutput("table_selected")
                        ),
                        tabPanel("WingSpread vs Depth",
                                 value = "wing",
                                 plotOutput("plot_wing", height = "550px", click = "click_plot"),
                                 h4("Selected rows (current dataset):"),
                                 DTOutput("table_selected2")
                        ),
                        tabPanel("DoorSpread vs Depth",
                                 value = "door",
                                 plotOutput("plot_door", height = "550px", click = "click_plot"),
                                 h4("Selected rows (current dataset):"),
                                 DTOutput("table_selected3")
                        ),
                        tabPanel("Netopening vs Depth",
                                 value = "net",
                                 plotOutput("plot_net", height = "550px", click = "click_plot"),
                                 h4("Selected rows (current dataset):"),
                                 DTOutput("table_selected4")
                        ),
                        tabPanel("All data preview",
                                 DTOutput("table_all")
                        )
            )
        )
    )
)


# ----- SERVER -----
server <- function(input, output, session) {

    # store original downloaded data and current (cleaned) data
    original_df <- reactiveVal(NULL)
    current_df  <- reactiveVal(NULL)

    # selected row ids (vector of .rowid)
    selected_ids <- reactiveVal(integer(0))

    # helper: convert -9 -> NA across numeric columns
    convert_neg9_to_na <- function(df) {
        if (is.null(df)) return(df)
        for (nm in names(df)) {
            if (is.numeric(df[[nm]])) {
                df[[nm]][df[[nm]] == -9] <- NA
            } else {
                # sometimes numeric columns are character; try coercion safely
                suppressWarnings({
                    tmp <- as.numeric(df[[nm]])
                })
                if (!all(is.na(tmp))) {
                    tmp[tmp == -9] <- NA
                    df[[nm]] <- tmp
                }
            }
        }
        df
    }

    # Download / assemble data from DATRAS when user clicks
    observeEvent(input$btn_download, {
        req(input$survey, input$years, input$quarters)
        years_seq <- seq(input$years[1], input$years[2])
        quarters <- unique(as.numeric(input$quarters))

        showNotification("Starting download...", type = "message", duration = 2)

        all_list <- list()
        i <- 1
        for (y in years_seq) {
            for (q in quarters) {
                # attempt download (quietly handle errors)
                msg <- paste0("Downloading: ", input$survey, " ", y, " Q", q)
                message(msg)
                tmp <- tryCatch({
                    icesDatras::getDATRAS("HH", input$survey, y, q)
                }, error = function(e) {
                    message("  -> failed: ", e$message)
                    NULL
                })
                if (!is.null(tmp) && is.data.frame(tmp) && nrow(tmp) > 0) {
                    # ensure Year/Quarter present and numeric
                    tmp$Year <- as.numeric(y)
                    tmp$Quarter <- as.numeric(q)
                    all_list[[length(all_list) + 1]] <- tmp
                }
            }
        }

        if (length(all_list) == 0) {
            showNotification("❌ No HH data downloaded for the selection.", type = "error")
            original_df(NULL); current_df(NULL)
            return()
        }

        df <- do.call(rbind, all_list)

        # normalize names: replace spaces/specials with simple names (consistent with make.names)
        names(df) <- make.names(names(df))

        # convert -9 -> NA
        df <- convert_neg9_to_na(df)

        # keep only rows with Depth>0 (and HaulVal 'V' if present)
        if ("Depth" %in% names(df)) df$Depth <- suppressWarnings(as.numeric(df$Depth))
        if ("HaulVal" %in% names(df)) {
            df <- df[!(df$HaulVal %in% c("I")), , drop=FALSE]  # remove invalid hauls if 'I'
        }
        keep_idx <- rep(TRUE, nrow(df))
        if ("Depth" %in% names(df)) keep_idx <- keep_idx & !is.na(df$Depth) & (df$Depth > 0)
        df <- df[keep_idx, , drop=FALSE]

        # create a stable row id
        df$.rowid <- seq_len(nrow(df))

        # save
        original_df(df)
        current_df(df)
        selected_ids(integer(0))
        showNotification(paste0("✅ Downloaded ", nrow(df), " rows."), type = "message")
    })


    # helper to get dataset used for plotting (current_df)
    df_for_plot <- reactive({
        df <- current_df()
        if (is.null(df)) return(NULL)
        df
    })

    # toggle selection on click: find nearest point in current_df for chosen plot
    observeEvent(input$click_plot, {
        df <- df_for_plot()
        if (is.null(df) || nrow(df) == 0) return()

        # determine active tab (which plot)
        tab <- isolate(input$plots_tabs)
        # default mapping: x = Depth, y = variable
        if (is.null(tab)) tab <- "warp"

        if (tab == "warp") {
            xvar <- "Depth"; yvar <- "Warplngt"
        } else if (tab == "wing") {
            xvar <- "Depth"; yvar <- "WingSpread"
        } else if (tab == "door") {
            xvar <- "Depth"; yvar <- "DoorSpread"
        } else if (tab== "net") {
            xvar <- "Depth"; yvar <- "Netopening"
        } else
            {
            xvar <- "Depth"; yvar <- "Warplngt"
        }

        # ensure variables exist and numeric
        if (!(xvar %in% names(df)) || !(yvar %in% names(df))) return()
        df[[xvar]] <- suppressWarnings(as.numeric(df[[xvar]]))
        df[[yvar]] <- suppressWarnings(as.numeric(df[[yvar]]))
        df2 <- df[!is.na(df[[xvar]]) & !is.na(df[[yvar]]), , drop=FALSE]
        if (nrow(df2) == 0) return()

        near <- nearPoints(df2, input$click_plot, xvar = xvar, yvar = yvar, threshold = 8, maxpoints = 1)
        if (nrow(near) == 0) return()

        rid <- near$.rowid[1]

        cur_sel <- selected_ids()
        if (rid %in% cur_sel) {
            # deselect
            cur_sel <- setdiff(cur_sel, rid)
        } else {
            # select
            cur_sel <- unique(c(cur_sel, rid))
        }
        selected_ids(cur_sel)
    })


    # Delete selected rows from current_df
    observeEvent(input$btn_delete, {
        df <- current_df()
        if (is.null(df)) return()
        sel <- selected_ids()
        if (length(sel) == 0) {
            showNotification("No rows selected to delete.", type = "warning")
            return()
        }
        keep <- !(df$.rowid %in% sel)
        df2 <- df[keep, , drop=FALSE]
        # reassign .rowid to keep uniqueness
        df2$.rowid <- seq_len(nrow(df2))
        current_df(df2)
        selected_ids(integer(0))
        showNotification(paste0("Deleted ", sum(!keep), " rows. Remaining: ", nrow(df2)), type = "message")
    })

    # Restore original
    observeEvent(input$btn_restore, {
        df <- original_df()
        if (is.null(df)) return()
        current_df(df)
        selected_ids(integer(0))
        showNotification("Restored original downloaded data.", type = "message")
    })


    # plotting helpers: draw points colored by SweepLngt (max 2 colors)
    draw_by_sweep <- function(df, yvar, regression_type = c("log","linear")) {
        regression_type <- match.arg(regression_type)
        # prepare sweep groups
        if ("SweepLngt" %in% names(df)) {
            tipos <- sort(unique(na.omit(df$SweepLngt)))
        } else {
            tipos <- character(0)
        }
        pal <- c("steelblue","darkorange","darkgreen","purple")
        if (length(tipos) == 0) {
            # simple grey points
            points(df$Depth, df[[yvar]], pch = 21, bg = "grey", col = "black")
            return(invisible(NULL))
        }

        labels <- c()
        for (i in seq_along(tipos)) {
            t <- tipos[i]
            sub <- df[df$SweepLngt == t & !is.na(df$Depth) & !is.na(df[[yvar]]), , drop=FALSE]
            if (nrow(sub) == 0) next
            points(sub$Depth, sub[[yvar]], pch = 21, bg = pal[i], col = "black")
            # depth range
            rng <- range(sub$Depth, na.rm = TRUE)
            if (nrow(sub) > 5 && diff(rng) > 0) {
                if (regression_type == "log") {
                    fmla <- as.formula(paste(yvar, "~ log(Depth)"))
                } else {
                    fmla <- as.formula(paste(yvar, "~ Depth"))
                }
                mod <- tryCatch(lm(fmla, data = sub), error = function(e) NULL)
                if (!is.null(mod)) {
                    dseq <- seq(rng[1], rng[2], length.out = 200)
                    newd <- data.frame(Depth = dseq)
                    preds <- tryCatch(predict(mod, newdata = newd), error = function(e) NULL)
                    if (!is.null(preds)) lines(dseq, preds, col = pal[i], lwd = 2)
                }
            }
            labels <- c(labels, paste0("Sweep=", t, " (", round(rng[1]), "–", round(rng[2]), "m, n=", nrow(sub), ")"))
        }
        legend("topright", legend = labels, bty = "n", cex = 0.85)
    }

    # render Warp plot
    output$plot_warp <- renderPlot({
        df <- df_for_plot()
        if (is.null(df) || nrow(df) == 0) {
            plot.new(); title("No data. Click 'Download HH from DATRAS' to load data.")
            return()
        }
        if (!("Warplngt" %in% names(df))) {
            plot.new(); title("Warplngt not present in data.")
            return()
        }

        # numeric
        df$Depth <- suppressWarnings(as.numeric(df$Depth))
        df$Warplngt <- suppressWarnings(as.numeric(df$Warplngt))
        dfp <- df[!is.na(df$Depth) & !is.na(df$Warplngt), , drop=FALSE]
        if (nrow(dfp) == 0) {
            plot.new(); title("No valid Warp vs Depth points.")
            return()
        }

        xlim <- range(dfp$Depth, na.rm = TRUE)
        ylim <- range(dfp$Warplngt, na.rm = TRUE)
        plot(NA, xlim = xlim, ylim = ylim, xlab = "Depth (m)", ylab = "Warplngt (m)", main = "Warplngt vs Depth")
        # draw by sweep but regression linear
        draw_by_sweep(dfp, "Warplngt", regression_type = "linear")

        # overlay current selected rows as green triangles
        sel <- selected_ids()
        if (length(sel) > 0) {
            selrows <- df[df$.rowid %in% sel, , drop=FALSE]
            selrows$Depth <- suppressWarnings(as.numeric(selrows$Depth))
            selrows$Warplngt <- suppressWarnings(as.numeric(selrows$Warplngt))
            selrows <- selrows[!is.na(selrows$Depth) & !is.na(selrows$Warplngt), , drop=FALSE]
            if (nrow(selrows) > 0) points(selrows$Depth, selrows$Warplngt, pch = 24, bg = "green", col = "black", cex = 1.6)
        }
    })


    # render Wing plot
    output$plot_wing <- renderPlot({
        df <- df_for_plot()
        if (is.null(df) || nrow(df) == 0) {
            plot.new(); title("No data. Click 'Download HH from DATRAS' to load data.")
            return()
        }
        if (!("WingSpread" %in% names(df))) {
            plot.new(); title("WingSpread not present in data.")
            return()
        }

        df$Depth <- suppressWarnings(as.numeric(df$Depth))
        df$WingSpread <- suppressWarnings(as.numeric(df$WingSpread))
        dfp <- df[!is.na(df$Depth) & !is.na(df$WingSpread), , drop=FALSE]
        if (nrow(dfp) == 0) {
            plot.new(); title("No valid WingSpread vs Depth points.")
            return()
        }
        xlim <- range(dfp$Depth, na.rm = TRUE)
        ylim <- range(dfp$WingSpread, na.rm = TRUE)
        plot(NA, xlim = xlim, ylim = ylim, xlab = "Depth (m)", ylab = "WingSpread (m)", main = "WingSpread vs Depth")
        draw_by_sweep(dfp, "WingSpread", regression_type = "log")

        sel <- selected_ids()
        if (length(sel) > 0) {
            selrows <- df[df$.rowid %in% sel, , drop=FALSE]
            selrows$Depth <- suppressWarnings(as.numeric(selrows$Depth))
            selrows$WingSpread <- suppressWarnings(as.numeric(selrows$WingSpread))
            selrows <- selrows[!is.na(selrows$Depth) & !is.na(selrows$WingSpread), , drop=FALSE]
            if (nrow(selrows) > 0) points(selrows$Depth, selrows$WingSpread, pch = 24, bg = "green", col = "black", cex = 1.6)
        }
    })

    # render Door plot
    output$plot_door <- renderPlot({
        df <- df_for_plot()
        if (is.null(df) || nrow(df) == 0) {
            plot.new(); title("No data. Click 'Download HH from DATRAS' to load data.")
            return()
        }
        if (!("DoorSpread" %in% names(df))) {
            plot.new(); title("DoorSpread not present in data.")
            return()
        }

        df$Depth <- suppressWarnings(as.numeric(df$Depth))
        df$DoorSpread <- suppressWarnings(as.numeric(df$DoorSpread))
        dfp <- df[!is.na(df$Depth) & !is.na(df$DoorSpread), , drop=FALSE]
        if (nrow(dfp) == 0) {
            plot.new(); title("No valid DoorSpread vs Depth points.")
            return()
        }
        xlim <- range(dfp$Depth, na.rm = TRUE)
        ylim <- range(dfp$DoorSpread, na.rm = TRUE)
        plot(NA, xlim = xlim, ylim = ylim, xlab = "Depth (m)", ylab = "DoorSpread (m)", main = "DoorSpread vs Depth")
        draw_by_sweep(dfp, "DoorSpread", regression_type = "log")

        sel <- selected_ids()
        if (length(sel) > 0) {
            selrows <- df[df$.rowid %in% sel, , drop=FALSE]
            selrows$Depth <- suppressWarnings(as.numeric(selrows$Depth))
            selrows$DoorSpread <- suppressWarnings(as.numeric(selrows$DoorSpread))
            selrows <- selrows[!is.na(selrows$Depth) & !is.na(selrows$DoorSpread), , drop=FALSE]
            if (nrow(selrows) > 0) points(selrows$Depth, selrows$DoorSpread, pch = 24, bg = "green", col = "black", cex = 1.6)
        }
    })

    # render Net plot
    output$plot_net <- renderPlot({
        df <- df_for_plot()
        if (is.null(df) || nrow(df) == 0) {
            plot.new(); title("No data. Click 'Download HH from DATRAS' to load data.")
            return()
        }
        if (!("Netopening" %in% names(df))) {
            plot.new(); title("Netopening not present in data.")
            return()
        }

        df$Depth <- suppressWarnings(as.numeric(df$Depth))
        df$Netopening <- suppressWarnings(as.numeric(df$Netopening))
        dfp <- df[!is.na(df$Depth) & !is.na(df$Netopening), , drop=FALSE]
        if (nrow(dfp) == 0) {
            plot.new(); title("No valid Netopening vs Depth points.")
            return()
        }
        xlim <- range(dfp$Depth, na.rm = TRUE)
        ylim <- range(dfp$Netopening, na.rm = TRUE)
        plot(NA, xlim = xlim, ylim = ylim, xlab = "Depth (m)", ylab = "Netopening (m)", main = "Net opening vs Depth")
        draw_by_sweep(dfp, "Netopening", regression_type = "log")

        sel <- selected_ids()
        if (length(sel) > 0) {
            selrows <- df[df$.rowid %in% sel, , drop=FALSE]
            selrows$Depth <- suppressWarnings(as.numeric(selrows$Depth))
            selrows$Netopening <- suppressWarnings(as.numeric(selrows$Netopening))
            selrows <- selrows[!is.na(selrows$Depth) & !is.na(selrows$Netopening), , drop=FALSE]
            if (nrow(selrows) > 0) points(selrows$Depth, selrows$Netopening, pch = 24, bg = "green", col = "black", cex = 1.6)
        }
    })

    # tables: selected rows (same content in 3 tabs)
    selected_table_df <- reactive({
        df <- current_df()
        if (is.null(df)) return(NULL)
        sel <- selected_ids()
        if (length(sel) == 0) return(NULL)
        dfsel <- df[df$.rowid %in% sel, , drop=FALSE]
        # restrict columns to requested subset for saving/preview:
        keepcols <- c("Survey","Year","Quarter","Country","Ship","HaulNo","ShootLat","ShootLong",
                      "HaulLat","HaulLong","Depth","HaulVal","Distance","Warplngt","SweepLngt","DoorSpread",
                      "WingSpread","Netopening","TowDir")
        keepcols <- intersect(keepcols, names(dfsel))
        dfsel[, keepcols, drop = FALSE]
    })

    # Render the selected-table output (explicit renderDT for each UI element)
    output$table_selected <- renderDT({
        dat <- selected_table_df()
        if (is.null(dat)) return(NULL)
        datatable(dat, options = list(dom = 't', paging = FALSE), rownames = FALSE)
    })

    output$table_selected2 <- renderDT({
        dat <- selected_table_df()
        if (is.null(dat)) return(NULL)
        datatable(dat, options = list(dom = 't', paging = FALSE), rownames = FALSE)
    })

    output$table_selected3 <- renderDT({
        dat <- selected_table_df()
        if (is.null(dat)) return(NULL)
        datatable(dat, options = list(dom = 't', paging = FALSE), rownames = FALSE)
    })
    output$table_selected4 <- renderDT({
        dat <- selected_table_df()
        if (is.null(dat)) return(NULL)
        datatable(dat, options = list(dom = 't', paging = FALSE), rownames = FALSE)
    })
    # all data preview (restricted columns)
    output$table_all <- renderDT({
        df <- current_df()
        if (is.null(df)) return(NULL)
        keepcols <- c("Survey","Year","Quarter","Country","Ship","HaulNo","ShootLat","ShootLong",
                      "HaulLat","HaulLong","Depth","HaulVal","Distance","Warplngt","SweepLngt","DoorSpread",
                      "WingSpread","Netopening","TowDir")
        keepcols <- intersect(keepcols, names(df))
        datatable(df[, keepcols, drop = FALSE], options = list(pageLength = 10, scrollX = TRUE))
    })


    # download cleaned CSV
    output$download_clean <- downloadHandler(
        filename = function() {
            sr <- input$survey
            yrs <- paste0(input$years[1], "-", input$years[2])
            qstr <- paste(input$quarters, collapse = "")
            paste0("HH_", sr, "_", yrs, "_Q", qstr, "_cleaned.csv")
        },
        content = function(file) {
            df <- current_df()
            if (is.null(df)) {
                write.csv(data.frame(), file, row.names = FALSE)
            } else {
                # restrict to useful columns (if present)
                keepcols <- c("Survey","Year","Quarter","Country","Ship","HaulNo","ShootLat","ShootLong",
                              "HaulLat","HaulLong","Depth","HaulVal","Distance","Warplngt","SweepLngt","DoorSpread",
                              "WingSpread","Netopening","TowDir")
                keepcols <- intersect(keepcols, names(df))
                write.csv(df[, keepcols, drop = FALSE], file, row.names = FALSE)
            }
        }
    )

} # server

shinyApp(ui, server)
