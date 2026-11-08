makeVectorAnalysis <- function(root) {
    options <- jmvcore::Options$new(requiresData = FALSE)
    results <- jmvcore::Group$new(options = options, name = "results")
    image <- jmvcore::Image$new(options = options, name = "plot", mode = "vector")
    results$add(image)
    TestAnalysis <- R6::R6Class(inherit = jmvcore::Analysis,
        private = list(.plot = function(image, ...) {
            graphics::plot(1:3, 1:3)
            TRUE
        }))
    analysis <- TestAnalysis$new(package = "jmvcore", name = "vector-test",
        version = c(1, 0, 0), options = options, results = results)
    analysis$.setResourcesPathSource(function(name, suffix)
        list(rootPath = root, relPath = paste0(name, ".", suffix)))
    list(analysis = analysis, image = image)
}

test_that("vector plots fall back to a real PNG when SVG cannot open", {
    root <- tempfile(); dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    local_mocked_bindings(svg = function(filename, ...) {
        writeLines("partial svg", filename)
        stop("cairo unavailable")
    }, .package = "grDevices")
    a <- makeVectorAnalysis(root)
    device <- grDevices::dev.cur()
    expect_true(a$analysis$.createImage(".plot", a$image))
    expect_match(a$image$filePath, "\\.png$")
    file <- file.path(root, a$image$filePath)
    expect_identical(readBin(file, "raw", 8), as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
    expect_length(list.files(root, pattern = "\\.svg$"), 0)
    expect_identical(grDevices::dev.cur(), device)
    expect_identical(a$image$mode, "vector")
})

test_that("working SVG devices still produce vector resources", {
    skip_if_not(capabilities("cairo"))
    root <- tempfile(); dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    a <- makeVectorAnalysis(root)
    expect_true(a$analysis$.createImage(".plot", a$image))
    expect_match(a$image$filePath, "\\.svg$")
    expect_match(paste(readLines(file.path(root, a$image$filePath)), collapse = "\n"), "<svg")
})

test_that("an SVG device that only warns and does not open also falls back", {
    root <- tempfile(); dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    local_mocked_bindings(svg = function(...) warning("SVG is not supported"),
                          .package = "grDevices")
    a <- makeVectorAnalysis(root)
    expect_true(a$analysis$.createImage(".plot", a$image))
    expect_match(a$image$filePath, "\\.png$")
    expect_gt(file.info(file.path(root, a$image$filePath))$size, 100)
})
