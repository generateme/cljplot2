# Datasets of the lattice gallery (notebooks/galleries/lattice.clj) exported from local R packages to notebooks/data.
# Run from the repository root: Rscript dev/lattice_data.R
# Every file is CSV (write.csv, no row names) with kebab-case columns; missing values are empty fields; matrices and
# tables become long rows, time series `time, value` rows (`time, series, value` for several series). All rows are
# kept. Re-running reproduces the files byte for byte (fixed package versions).
# levels.edn: the level order of every factor column (CSV keeps no factor levels) - {dataset {column ["level" ..]}},
# read by notebook-utils/levels.

suppressMessages({
  library(lattice)
  library(latticeExtra)
  library(flowCore)
  library(hexbin)
})

out <- "notebooks/data"

kebab <- function(names) gsub("[._ ]+", "-", tolower(gsub("([a-z])([A-Z])", "\\1-\\2", names)))

factor_levels <- list()

# remember the levels of `df`'s factor columns under dataset `key` (kebab-case column names)
remember_levels <- function(df, key) {
  fs <- Filter(is.factor, df)
  if (length(fs) > 0) factor_levels[[key]] <<- setNames(lapply(fs, levels), kebab(names(fs)))
}

export <- function(df, file) {
  remember_levels(df, sub("\\.csv$", "", file))
  names(df) <- kebab(names(df))
  write.csv(df, file.path(out, file), row.names = FALSE, na = "")
  cat(sprintf("%-26s %8d rows %3d columns\n", file, nrow(df), ncol(df)))
}

series <- function(x) data.frame(time = as.numeric(time(x)), value = as.numeric(x))

data(Oats, package = "MEMSS")
export(as.data.frame(Oats), "oats.csv")

data(gvhd10, package = "latticeExtra")
export(gvhd10, "gvhd10.csv")

data(postdoc, package = "latticeExtra")
export(as.data.frame.table(postdoc, responseName = "count"), "postdoc.csv")

export(as.data.frame.table(VADeaths, responseName = "rate", dnn = NULL) |>
         setNames(c("age", "group", "rate")), "va-deaths.csv")

data(Earthquake, package = "MEMSS")
export(as.data.frame(Earthquake), "earthquake.csv")

data(SeatacWeather, package = "latticeExtra")
export(SeatacWeather, "seatac-weather.csv")

export(data.frame(state = rownames(USArrests), USArrests), "us-arrests.csv")

export(data.frame(name = state.name, abb = state.abb, region = state.region, division = state.division,
                  long = state.center$x, lat = state.center$y, state.x77, check.names = FALSE), "states.csv")

export(as.data.frame.table(USPersonalExpenditure, responseName = "expenditure") |>
         setNames(c("category", "year", "expenditure")), "us-personal-expenditure.csv")

# Fig 7.3: which graphical parameters (columns) each lattice setting (row) has, as in the book's code
pdf(NULL)  # trellis.par.get opens a device; a null one leaves no Rplots.pdf
settings <- trellis.par.get()
invisible(dev.off())
for (u in c("grid.pars", "fontsize", "clip", "axis.components", "layout.heights", "layout.widths")) settings[[u]] <- NULL
export(do.call(rbind, lapply(names(settings), function(n)
  data.frame(setting = rep(n, length(names(settings[[n]]))), parameter = names(settings[[n]])))),
  "lattice-settings.csv")

export(environmental, "environmental.csv")

export(data.frame(row = as.vector(row(volcano)), column = as.vector(col(volcano)), height = as.vector(volcano)),
       "volcano.csv")

data(Cars93, package = "MASS")
export(Cars93, "cars93.csv")

data(USAge.df, package = "latticeExtra")
export(USAge.df, "us-age.csv")

data(biocAccess, package = "latticeExtra")
export(transform(biocAccess, time = format(time, "%Y-%m-%dT%H:%M:%S")), "bioc-access.csv")

export(series(nhtemp), "nhtemp.csv")

export(do.call(rbind, lapply(colnames(EuStockMarkets), function(s)
  data.frame(time = as.numeric(time(EuStockMarkets)), series = s, value = as.numeric(EuStockMarkets[, s])))),
  "eu-stock-markets.csv")

export(series(sunspot.year), "sunspot-year.csv")

beavers <- make.groups(beaver1, beaver2)
export(data.frame(beaver = beavers$which, beavers[c("day", "time", "temp", "activ")]), "beavers.csv")

data(Gcsemv, package = "mlmRev")
export(Gcsemv, "gcsemv.csv")

data(USCancerRates, package = "latticeExtra")
export(data.frame(USCancerRates[c("state", "county")], USCancerRates[1:6]), "us-cancer-rates.csv")

# Fig 14.3 reads FSC-H by patient and visit: every event, those columns
data(GvHD, package = "flowCore")
export(do.call(rbind, lapply(sampleNames(GvHD), function(s) {
  info <- pData(GvHD)[s, ]
  data.frame(patient = info$Patient, visit = info$Visit, `fsc-h` = exprs(GvHD[[s]])[, "FSC-H"], check.names = FALSE)
})), "gvhd.csv")

data(NHANES, package = "hexbin")
export(NHANES, "nhanes.csv")

# levels of the datasets kept from earlier sources (barley.json, titanic.csv)
remember_levels(barley, "barley")
remember_levels(as.data.frame(Titanic), "titanic")

edn_string <- function(x) paste0('"', gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", x)), '"')
edn <- c(";; Level order of the factor columns of the lattice datasets (written by dev/lattice_data.R; CSV keeps no factor",
         ";; levels): dataset -> column -> levels as strings; see notebook-utils/levels.",
         "{")
for (key in sort(names(factor_levels))) {
  columns <- factor_levels[[key]]
  edn <- c(edn, paste0(" :", key, " {"),
           sapply(sort(names(columns)), function(column)
             paste0("   :", column, " [", paste(edn_string(columns[[column]]), collapse = " "), "]")),
           "  }")
}
edn <- c(edn, "}")
writeLines(edn, file.path(out, "levels.edn"))
cat(sprintf("%-26s %8d datasets\n", "levels.edn", length(factor_levels)))
