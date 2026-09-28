# Also supports source-only verification when Docker/native packaging is unavailable.
if (Sys.getenv("JWODY_SOURCE_TESTS") == "1") {
    root <- Sys.getenv("JWODY_ROOT")
    for (f in list.files(file.path(root, "R"), "^utils-.*[.]R$", full.names = TRUE)) source(f)
    for (f in list.files(file.path(root, "R"), "[.]h[.]R$", full.names = TRUE)) source(f)
    for (f in list.files(file.path(root, "R"), "[.]b[.]R$", full.names = TRUE)) source(f)
}
