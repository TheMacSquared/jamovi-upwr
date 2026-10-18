context('fonts')

test_that('jupwrFontconfig points fontconfig at the font directory on macOS', {

    skip_if_not(Sys.info()[['sysname']] == 'Darwin')

    old <- Sys.getenv('FONTCONFIG_FILE', unset=NA)
    on.exit(if (is.na(old)) Sys.unsetenv('FONTCONFIG_FILE')
            else Sys.setenv(FONTCONFIG_FILE=old))
    Sys.unsetenv('FONTCONFIG_FILE')

    dir <- file.path(tempdir(), 'jupwr fonts & co')
    dir.create(dir, showWarnings=FALSE)

    conf <- jmvcore:::jupwrFontconfig(dir)
    expect_equal(Sys.getenv('FONTCONFIG_FILE'), conf)

    lines <- readLines(conf)
    expect_true(any(grepl('<dir>.*jupwr fonts &amp; co</dir>', lines)))

    # an existing configuration is left alone
    Sys.setenv(FONTCONFIG_FILE='/elsewhere.conf')
    expect_null(jmvcore:::jupwrFontconfig(dir))
    expect_equal(Sys.getenv('FONTCONFIG_FILE'), '/elsewhere.conf')
})
