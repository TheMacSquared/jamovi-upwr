
# Strings must reach protobuf as UTF-8 bytes. RProtoBuf copies the bytes of an
# R string as they are, so a string in the native encoding is sent as native
# bytes. On Windows the engine asks for a UTF-8 locale (enginer.cpp), but the
# C runtime honours that only with UCRT on Windows 10 >= 1803; on Windows 8.1,
# and in any R built against MSVCRT (R <= 4.1), R stays in the system code page
# (e.g. CP1250) and every string that went through format()/enc2native() (titles,
# notes, levels) arrives as invalid UTF-8. libprotobuf only logs it, the python
# server then fails ParseFromString() and restarts the engine ("resource limit"
# message in the client). Converting at the boundary is a no-op for ASCII and
# for strings already marked UTF-8.
pbstr <- function(x) {
    if (is.character(x))
        return(enc2utf8(x))
    x
}

RProtoBuf_new <- if (requireNamespace('RProtoBuf', quietly=TRUE)) {
    function(...) {
        args <- lapply(list(...), pbstr)
        do.call(RProtoBuf::new, args)
    }
}
RProtoBuf_serialize <- if (requireNamespace('RProtoBuf', quietly=TRUE)) RProtoBuf::serialize
RProtoBuf_read <- if (requireNamespace('RProtoBuf', quietly=TRUE)) RProtoBuf::read

initProtoBuf <- function() {
    if ( ! exists('jamovi.coms.Status')) {
        resultsProtoPath <- system.file("jamovi.proto", package="jmvcore")
        if (resultsProtoPath == "")
            resultsProtoPath <- system.file("inst", "jamovi.proto", package="jmvcore")
        if (resultsProtoPath == "")
            stop("jmvcore jamovi.proto not found!", call.=FALSE)

        if (requireNamespace('RProtoBuf', quietly=TRUE))
            RProtoBuf::readProtoFiles(resultsProtoPath)
        else
            stop('Could not load RProtoBuf')
    }
}
