context("protobuf strings are UTF-8 regardless of the native encoding")

# Regression test for Windows 8.1 / R built against MSVCRT, where R stays in
# the system code page (e.g. CP1250) and strings in the native encoding used to
# reach protobuf as invalid UTF-8 (server DecodeError, "resource limit" message).

utf8_bytes <- as.raw(c(0x44, 0xc5, 0x82, 0x75, 0x67, 0x6f, 0xc5, 0x9b, 0xc4, 0x87))  # Dlugosc

test_that("pbstr converts native strings to UTF-8 and leaves the rest alone", {
    u <- rawToChar(utf8_bytes)
    Encoding(u) <- "UTF-8"
    expect_identical(charToRaw(jmvcore:::pbstr(u)), utf8_bytes)
    n <- enc2native(u)
    expect_identical(charToRaw(jmvcore:::pbstr(n)), utf8_bytes)
    expect_identical(jmvcore:::pbstr(c("a", NA)), c("a", NA))
    expect_identical(jmvcore:::pbstr(3L), 3L)
    expect_identical(jmvcore:::pbstr(NULL), NULL)
})

test_that("RProtoBuf_new serialises a native-encoded title as UTF-8", {
    skip_if_not_installed("RProtoBuf")
    jmvcore:::initProtoBuf()
    u <- rawToChar(utf8_bytes)
    Encoding(u) <- "UTF-8"
    n <- enc2native(u)
    if (identical(charToRaw(n), utf8_bytes))
        skip("native encoding is UTF-8 here; nothing to convert")
    element <- jmvcore:::RProtoBuf_new(jamovi.coms.ResultsElement, name = "x", title = n)
    bytes <- element$serialize(NULL)
    # field 2 (title), length-delimited: tag 0x12, length, payload
    i <- which(bytes == as.raw(0x12))[1]
    len <- as.integer(bytes[i + 1])
    expect_identical(bytes[(i + 2):(i + 1 + len)], utf8_bytes)
})
