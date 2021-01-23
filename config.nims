task build, "Builds debug version":
    var outName : string

    when defined(windows):
      outName = "slobber.exe"
    else:
      outName = "slobber"

    setCommand "c", "src/slobber"
    --gc:arc


    # Debuggery
    --debuginfo:on
    --debugger:native
    --stackTrace:on
    --lineTrace:on

    # Profiling - uncomment import nimprof line in nif.nim.
    #--profiler:on
    #--stacktrace:on

    # memory profiling - uncomment import nimprof line in nif.nim.
    #--profiler:off
    #--define: memProfiler
    #--stacktrace:on
     
    # So go to errors works.
    --listFullPaths
    --threads:on
    --threadAnalysis:on
    --define: debug
    #--define: release

    --warnings:on
    --hints:on
    --colors:off
    --nanChecks:on
    --infChecks:on
    --overflowChecks:on  # This is expensive for what we're doing.

    switch("path", "src")
    switch("out", outName)

