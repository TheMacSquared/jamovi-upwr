# build.ps1 — natywny (dockerless) build jUPWR na Windows x64, pod R 4.6.
#
# Toolchain mieszany (jak w packaging/20-build-windows.md):
#   - jamovi.core (Cython)  -> MSVC (VS2022 Build Tools) + Boost 1.84 vc143
#   - silnik C++            -> RTools45 mingw (gcc 14, static.posix) + Boost 1.84 mingw
#   - moduly R              -> R 4.6 (+ user-lib z zaleznosciami), jmc (jamovi-compiler)
#   - Python bundla         -> python-build-standalone 3.12 (relokowalny)
#
# Wymagania na maszynie (zob. sekcja PREREQ ponizej): VS2022 BuildTools (VCTools),
# RTools45, R 4.6.0, Node 20+, cmake, internet (CRAN/posit, GitHub).
#
# Skrypt jest IDEMPOTENTNY tam gdzie to tanie (pomija gotowe kroki). Artefakty:
#   packaging/build/{stage,dist,deps}/   (ignorowane przez git)
# Wynik: packaging/build/dist/jUPWR/  + (opcjonalnie) jUPWR-<wersja>-portable-win64.zip
#
# UWAGA — ten skrypt zaklada, ze w drzewie ZRODLOWYM sa juz nasze patche:
#   * jamovi-compiler/index.js  (galaz win32 honoruje --rhome)
#   * jamovi-compiler/snapshots.js  (wpis '4.6.0')
#   * engine/Makefile.in  (blok Windows: nazwy boost layout=system, -std=gnu++17,
#                          zmienne BOOST_LIBDIR/NANOMSG_DIR/PROTOBUF_DIR, $(EXTRA_LIBS))
# Patche do plikow POBIERANYCH/VENDOROWANYCH (nanomsg, formatio) aplikuje sam skrypt.

$ErrorActionPreference = "Stop"

# ---------------------------------------------------------------------------
# Konfiguracja (DOSTOSUJ do maszyny)
# ---------------------------------------------------------------------------
$RepoRoot   = (Resolve-Path "$PSScriptRoot\..\..\..").Path
$BuildDir   = Join-Path $RepoRoot "packaging\build"
$Stage      = Join-Path $BuildDir "stage"
$Payload    = Join-Path $Stage "jamovi"
$Dist       = Join-Path $BuildDir "dist"
$Deps       = Join-Path $BuildDir "deps"
$Scratch    = Join-Path $BuildDir "dl"          # pobierane archiwa

$AppName    = "jUPWR"
$JamoviVer  = (Get-Content (Join-Path $RepoRoot "version")).Trim()         # np. 2.7.35.0
$JupwrVer   = ((Select-String -Path (Join-Path $RepoRoot "client\common\jupwr.ts") -Pattern "JUPWR_VERSION\s*=\s*'([0-9.]+)'").Matches.Groups[1].Value)
if (-not $JupwrVer) { $JupwrVer = $JamoviVer }

$RHome      = "C:\Program Files\R\R-4.6.0"                 # R 4.6 (pod ktory budujemy)
$RHomeShort = (& "$RHome\bin\x64\R.exe" RHOME).Trim()      # krotka sciezka bez spacji (mingw)
$UserLib    = "$env:LOCALAPPDATA\R\win-library\4.6"        # tu sa ciezkie pakiety (ggplot2, car...)
$RtoolsMingw= "C:\rtools45\x86_64-w64-mingw32.static.posix"
$RtoolsUsr  = "C:\rtools45\usr\bin"
$BoostRoot  = "C:\local\boost_1_84_0"                      # zrodla + prebuilt MSVC (lib64-msvc-14.3)
$Vcvars     = "C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvars64.bat"

$ElectronVer= "32.3.3"
$PbsUrl     = "https://github.com/astral-sh/python-build-standalone/releases/download/20250612/cpython-3.12.11+20250612-x86_64-pc-windows-msvc-install_only_stripped.tar.gz"
$NanomsgUrl = "https://github.com/nanomsg/nanomsg/archive/refs/tags/1.2.tar.gz"
$CranRepo   = "https://packagemanager.posit.co/cran/latest"
$Modules    = @('jmv','plots','jperm','jCI','jboot','jdistrACTION')

$ProgressPreference = 'SilentlyContinue'
function Step($m){ Write-Host "`n==> $m" -ForegroundColor Cyan }
function Info($m){ Write-Host "    $m" -ForegroundColor DarkGray }
New-Item -ItemType Directory -Force -Path $Payload,$Dist,$Deps,$Scratch | Out-Null

# Srodowisko mingw (RTools): UWAGA — wyczyscic NoDefaultCurrentDirectoryInExePath,
# bo psuje batniki Boosta/bootstrap (nie znajduja sasiednich .bat w cwd).
function Use-Mingw {
    $env:Path = "$RtoolsMingw\bin;$RtoolsUsr;" + $env:Path
    $env:NoDefaultCurrentDirectoryInExePath = $null
}

# ---------------------------------------------------------------------------
# PREREQ — weryfikacja toolchainu
# ---------------------------------------------------------------------------
Step "Weryfikacja narzedzi"
foreach ($p in @(
    @{n='R 4.6';     t="$RHome\bin\x64\R.exe"},
    @{n='RTools45';  t="$RtoolsMingw\bin\gcc.exe"},
    @{n='protoc';    t="$RtoolsMingw\bin\protoc.exe"},
    @{n='VS2022';    t=$Vcvars},
    @{n='Boost src'; t="$BoostRoot\bootstrap.bat"},
    @{n='Boost MSVC';t="$BoostRoot\lib64-msvc-14.3"}
)) { if (-not (Test-Path $p.t)) { throw "BRAK $($p.n): $($p.t)" } ; Info "OK $($p.n)" }
if (-not (Get-Command node -EA SilentlyContinue)) { throw "BRAK node" }
if (-not (Get-Command cmake -EA SilentlyContinue)) { throw "BRAK cmake" }
Info "jUPWR $JupwrVer (jamovi $JamoviVer)"

# ---------------------------------------------------------------------------
# FAZA 1 — Boost mingw (filesystem/system/nowide) z tych samych zrodel co MSVC
# ---------------------------------------------------------------------------
Step "Boost mingw (layout=system)"
if (-not (Test-Path "$BoostRoot\stage-mingw\lib\libboost_filesystem.a")) {
    Use-Mingw
    if (-not (Test-Path "$BoostRoot\b2.exe")) {
        & "$env:WINDIR\System32\cmd.exe" /c "set NoDefaultCurrentDirectoryInExePath=&& cd /d `"$BoostRoot`" && bootstrap.bat gcc" | Out-Null
    }
    & "$env:WINDIR\System32\cmd.exe" /c "set NoDefaultCurrentDirectoryInExePath=&& cd /d `"$BoostRoot`" && b2.exe toolset=gcc address-model=64 --layout=system variant=release link=static runtime-link=shared threading=multi --with-filesystem --with-system --with-nowide --stagedir=stage-mingw -j4" | Out-Null
}
if (-not (Test-Path "$BoostRoot\stage-mingw\lib\libboost_filesystem.a")) { throw "Boost mingw nieudany" }
Info "OK boost mingw"

# ---------------------------------------------------------------------------
# FAZA 2 — nanomsg (mingw, cmake). GCC14 wymaga -fpermissive (incompatible-pointer-types).
# ---------------------------------------------------------------------------
Step "nanomsg (mingw)"
if (-not (Test-Path "$Deps\nanomsg\lib\libnanomsg.dll.a")) {
    $nm = "$Scratch\nanomsg-1.2"
    if (-not (Test-Path "$nm\CMakeLists.txt")) {
        Invoke-WebRequest $NanomsgUrl -OutFile "$Scratch\nanomsg.tar.gz" -MaximumRedirection 10
        Push-Location $Scratch
        & tar -xzf "nanomsg.tar.gz" -C .
        Pop-Location
    }
    Use-Mingw
    New-Item -ItemType Directory -Force -Path "$nm\build" | Out-Null
    $mk = (Get-Command mingw32-make).Source
    & "$env:WINDIR\System32\cmd.exe" /c "set NoDefaultCurrentDirectoryInExePath=&& cd /d `"$nm\build`" && cmake .. -G `"MinGW Makefiles`" -DCMAKE_BUILD_TYPE=Release -DNN_TESTS=OFF -DNN_TOOLS=OFF -DNN_ENABLE_DOC=OFF `"-DCMAKE_C_FLAGS=-fpermissive -w`" `"-DCMAKE_MAKE_PROGRAM=$($mk -replace '\\','/')`" `"-DCMAKE_INSTALL_PREFIX=$($Deps -replace '\\','/')/nanomsg`" && `"$mk`" -j4 install" | Out-Null
}
if (-not (Test-Path "$Deps\nanomsg\lib\libnanomsg.dll.a")) { throw "nanomsg nieudany" }
Info "OK nanomsg"

# ---------------------------------------------------------------------------
# FAZA 3 — klient (vite)
# ---------------------------------------------------------------------------
Step "Klient (vite)"
Push-Location (Join-Path $RepoRoot "client")
if (-not (Test-Path node_modules)) { npm install | Out-Null }
npm run build:coms | Out-Null
node .\node_modules\vite\bin\vite.js build --outDir "$Payload\client" --emptyOutDir | Out-Null
Pop-Location
if (-not (Test-Path "$Payload\client\index.html")) { throw "klient nieudany" }
Info "OK klient"

# ---------------------------------------------------------------------------
# FAZA 4 — moduly R.  KOLEJNOSC: RProtoBuf -> jmvcore (jmvcore pieczetuje
#   RProtoBuf_serialize przy instalacji; bez RProtoBuf binding = NULL).
#   UWAGA Windows vs macOS: na macOS RProtoBuf musial byc ZE ZRODLA (wspolny
#   dynamiczny protobuf, inaczej SIGSEGV). Na Windows binarka (posit) DZIALA —
#   dwie statyczne kopie protobuf wspolistnieja (tak jak w instalce jamovi 2.7.35).
# ---------------------------------------------------------------------------
Step "Moduly R (base/R + jmc)"
$BaseR = "$Payload\modules\base\R"
New-Item -ItemType Directory -Force -Path $BaseR | Out-Null
$R = "$RHome\bin\x64\R.exe"
$env:R_LIBS = $BaseR

# 4a. RInside, Rcpp, RProtoBuf (binarne) — do base/R
if (-not (Test-Path "$BaseR\RInside"))   { & $R --vanilla --slave -e "options(repos=c(CRAN='$CranRepo')); install.packages('RInside', lib='$($BaseR -replace '\\','/')', INSTALL_opts='--no-multiarch')" | Out-Null }
if (-not (Test-Path "$BaseR\Rcpp"))      { & $R --vanilla --slave -e "options(repos=c(CRAN='$CranRepo')); install.packages('Rcpp', lib='$($BaseR -replace '\\','/')', INSTALL_opts='--no-multiarch')" | Out-Null }
if (-not (Test-Path "$BaseR\RProtoBuf")) { & $R --vanilla --slave -e "options(repos=c(CRAN='$CranRepo')); install.packages('RProtoBuf', lib='$($BaseR -replace '\\','/')', INSTALL_opts='--no-multiarch')" | Out-Null }

# 4b. zaleznosci modulow (do user-lib) — jmc oczekuje ich na sciezce
$deps13 = "multcomp emmeans vcd vcdExtra GGally BayesFactor psych GPArotation afex mvnormtest lavaan ROCR Hmisc".Split(' ')
$missing = $deps13 | Where-Object { -not (Test-Path "$UserLib\$_") }
if ($missing) { & $R --vanilla --slave -e "options(repos=c(CRAN='$CranRepo')); install.packages(c('$($missing -join "','")'), lib='$($UserLib -replace '\\','/')', dependencies=c('Depends','Imports','LinkingTo'))" | Out-Null }

# 4c. jmvcore PO RProtoBuf (zapieczetuje RProtoBuf_serialize)
& $R CMD INSTALL (Join-Path $RepoRoot "jmvcore") "--library=$BaseR" | Out-Null
$sealed = & "$RHome\bin\x64\Rscript.exe" -e "suppressMessages(library(jmvcore, lib.loc='$($BaseR -replace '\\','/')')); cat(is.function(get('RProtoBuf_serialize', asNamespace('jmvcore'))))"
if ($sealed.Trim() -ne 'TRUE') { throw "jmvcore: RProtoBuf_serialize niezapieczetowany (NULL) — sprawdz RProtoBuf" }
Info "OK jmvcore (RProtoBuf_serialize zapieczetowany)"

# 4d. kompilacja modulow przez jmc (--rhome dziala dzieki patchowi index.js)
Push-Location (Join-Path $RepoRoot "jamovi-compiler")
if (-not (Test-Path node_modules)) { npm install | Out-Null }
Pop-Location
$jmc = Join-Path $RepoRoot "jamovi-compiler\index.js"
foreach ($m in $Modules) {
    if (-not (Test-Path (Join-Path $RepoRoot $m))) { Info "pomijam $m (brak)"; continue }
    node $jmc --install (Join-Path $RepoRoot $m) --to "$Payload\modules" --rhome $RHome --rlibs "$BaseR;$UserLib" --assume-app-version $JamoviVer --patch-version --skip-deps | Out-Null
    Info "jmc $m OK"
}

# ---------------------------------------------------------------------------
# FAZA 5 — i18n
# ---------------------------------------------------------------------------
Step "i18n"
Push-Location (Join-Path $RepoRoot "i18n")
if (-not (Test-Path node_modules)) { npm install | Out-Null }
New-Item -ItemType Directory -Force -Path "$Payload\i18n\json" | Out-Null
node (Join-Path $RepoRoot "i18n\index.js") --build src --dest "$Payload\i18n\json" | Out-Null
Pop-Location

# ---------------------------------------------------------------------------
# FAZA 6 — silnik C++ (mingw). Patche w engine/Makefile.in; sciezki przez env.
#   protobuf 29.3 (RTools) wymaga C++17 + abseil (grupa --start-group) + bcrypt/dbghelp.
# ---------------------------------------------------------------------------
Step "Silnik (jamovi-engine.exe)"
# generacja Makefile (odpowiednik configure.bat: zeruje placeholdery)
$mfin = Get-Content (Join-Path $RepoRoot "engine\Makefile.in") -Raw
foreach ($ph in '%PREFIX%','%LIBDIR%','%CFLAGS%','%MFLAGS%','%CXXFLAGS%','%R_PATH%','%BASE_MODULE_PATH%','%R_HOME%') { $mfin = $mfin.Replace($ph,'') }
[System.IO.File]::WriteAllText((Join-Path $RepoRoot "engine\Makefile"), ($mfin -replace "`r`n","`n"))
# lista abseil + utf8 (grupa) z RTools
$absl = (Get-ChildItem "$RtoolsMingw\lib\libabsl_*.a" | ForEach-Object { '-l' + ($_.BaseName -replace '^lib','') }) -join ' '
Use-Mingw
$env:R_HOME          = $RHomeShort
$env:BASE_MODULE_PATH= ($BaseR -replace '\\','/')
$env:BOOST_LIBDIR    = "$($BoostRoot -replace '\\','/')/stage-mingw/lib"
$env:NANOMSG_DIR     = "$($Deps -replace '\\','/')/nanomsg"
$env:PROTOBUF_DIR    = ($RtoolsMingw -replace '\\','/')
$env:INCLUDES        = "-I$($BoostRoot -replace '\\','/') -I$($Deps -replace '\\','/')/nanomsg/include -I$($RtoolsMingw -replace '\\','/')/include"
$env:EXTRA_LIBS      = "-Wl,--start-group $absl -lutf8_range -lutf8_validity -Wl,--end-group -lbcrypt -ldbghelp -lws2_32 -lmswsock -ladvapi32"
Push-Location (Join-Path $RepoRoot "engine")
& (Get-Command mingw32-make).Source 2>&1 | Out-Null   # uzywa make z RTools (sh dla mkdir -p)
& make 2>&1 | Out-Null
Pop-Location
if (-not (Test-Path (Join-Path $RepoRoot "engine\jamovi-engine.exe"))) { throw "silnik nieudany" }
Info "OK silnik"

# ---------------------------------------------------------------------------
# FAZA 7 — Python bundla (PBS 3.12) + jamovi.core (MSVC) + server + deps
# ---------------------------------------------------------------------------
Step "Python + core + server"
if (-not (Test-Path "$Payload\python\python.exe")) {
    if (-not (Test-Path "$Scratch\pbs.tar.gz")) { Invoke-WebRequest $PbsUrl -OutFile "$Scratch\pbs.tar.gz" -MaximumRedirection 10 }
    Push-Location $Scratch
    & tar -xzf "pbs.tar.gz" -C .    # -> $Scratch\python
    Pop-Location
    Copy-Item "$Scratch\python" "$Payload\python" -Recurse -Force
}
$Py = "$Payload\python\python.exe"
& $Py -m pip install --upgrade pip wheel setuptools Cython | Out-Null
# requirements BEZ nanomsg (binding ze zrodla vendorujemy nizej; pelny -r urywa sie na nanomsg)
$reqFiltered = "$Scratch\req-no-nanomsg.txt"
Get-Content (Join-Path $RepoRoot "server\requirements.txt") | Where-Object { $_ -notmatch '^\s*nanomsg\b' } | Set-Content $reqFiltered
& $Py -m pip install -r $reqFiltered | Out-Null
& $Py -m pip install --upgrade protobuf | Out-Null
# core (MSVC) + server (protoc) -> Payload\server
$srv = "$Payload\server"; New-Item -ItemType Directory -Force -Path $srv | Out-Null
$cmd = "set NoDefaultCurrentDirectoryInExePath=&& call `"$Vcvars`" && cd /d `"$RepoRoot\server`" && `"$Py`" setup.py install --install-lib=`"$srv`" --single-version-externally-managed --record NUL"
& "$env:WINDIR\System32\cmd.exe" /c $cmd | Out-Null
if (-not (Get-ChildItem "$srv\jamovi\core*.pyd" -EA SilentlyContinue)) { throw "core nieudany" }
Info "OK core+server"

# nanomsg python binding (ctypes) ZE ZRODLA PyPI -> server, z patchami:
#   * bind/connect: str->bytes (inaczej kazda analiza WISI: nn_bind TypeError)
#   * _nanomsg_ctypes: nanoconfig opcjonalny (except OSError -> except (OSError, AttributeError))
Step "nanomsg (binding python, vendored + patch)"
if (-not (Test-Path "$srv\nanomsg")) {
    $meta = Invoke-RestMethod "https://pypi.org/pypi/nanomsg/1.0/json"
    $url = ($meta.urls | Where-Object packagetype -eq 'sdist').url
    Invoke-WebRequest $url -OutFile "$Scratch\nanomsg-py.tar.gz" -MaximumRedirection 10
    Push-Location $Scratch
    & tar -xzf "nanomsg-py.tar.gz" -C .
    Pop-Location
    $root = (Get-ChildItem $Scratch -Directory | Where-Object Name -like 'nanomsg-1.0*' | Select-Object -First 1).FullName
    foreach ($p in 'nanomsg','nanomsg_wrappers','_nanomsg_ctypes') { Copy-Item "$root\$p" "$srv\$p" -Recurse -Force }
}
# patch bind/connect str->bytes
$nmInit = "$srv\nanomsg\__init__.py"; $c = Get-Content $nmInit -Raw
$c = $c.Replace(
@"
    def bind(self, address):
        """Add a local endpoint to the socket"""
        if self.uses_nanoconfig:
            raise ValueError("Nanoconfig address must be sole endpoint")
        endpoint_id = _nn_check_positive_rtn(
"@,
@"
    def bind(self, address):
        """Add a local endpoint to the socket"""
        if self.uses_nanoconfig:
            raise ValueError("Nanoconfig address must be sole endpoint")
        if isinstance(address, str):
            address = address.encode('utf-8')
        endpoint_id = _nn_check_positive_rtn(
"@)
$c = $c.Replace(
@"
    def connect(self, address):
        """Add a remote endpoint to the socket"""
        if self.uses_nanoconfig:
            raise ValueError("Nanoconfig address must be sole endpoint")
        endpoint_id = _nn_check_positive_rtn(
"@,
@"
    def connect(self, address):
        """Add a remote endpoint to the socket"""
        if self.uses_nanoconfig:
            raise ValueError("Nanoconfig address must be sole endpoint")
        if isinstance(address, str):
            address = address.encode('utf-8')
        endpoint_id = _nn_check_positive_rtn(
"@)
[System.IO.File]::WriteAllText($nmInit, $c)
if ((Select-String -Path $nmInit -Pattern "address = address.encode\('utf-8'\)").Count -lt 2) {
    throw "nanomsg python binding patch failed: bind/connect still accept str addresses"
}
# patch nanoconfig opcjonalny
$ctInit = "$srv\_nanomsg_ctypes\__init__.py"
(Get-Content $ctInit -Raw).Replace('except OSError:','except (OSError, AttributeError):') | Set-Content $ctInit -NoNewline
# DLL: ctypes szuka 'nanomsg.dll' (Py3.8+ ignoruje PATH) -> obok python.exe ORAZ w bin
Copy-Item "$Deps\nanomsg\bin\libnanomsg.dll" "$Payload\python\nanomsg.dll" -Force

# readstat/librdata sa natywne (wymagaja iconv) — pomijane; formatio musi je tolerowac
$fmt = "$srv\jamovi\server\formatio\__init__.py"
if (Test-Path $fmt) {
    $f = Get-Content $fmt -Raw
    if ($f -notmatch 'except ImportError') {
        $f = $f -replace "(for plugin in plugins:\r?\n)(\s+)module = importlib\.import_module\(plugin, 'jamovi\.server\.formatio'\)", "`$1`$2try:`r`n`$2    module = importlib.import_module(plugin, 'jamovi.server.formatio')`r`n`$2except ImportError:`r`n`$2    continue"
        [System.IO.File]::WriteAllText($fmt, $f)
    }
}

# ---------------------------------------------------------------------------
# FAZA 8 — montaz bundla (uklad jak instalka jamovi 2.7.35)
# ---------------------------------------------------------------------------
Step "Montaz bundla"
$AppDir = "$Dist\$AppName"; $Bin = "$AppDir\bin"
if (Test-Path $AppDir) { Remove-Item $AppDir -Recurse -Force }
New-Item -ItemType Directory -Force -Path $Bin,"$AppDir\Frameworks","$AppDir\Resources\i18n" | Out-Null
# Electron
if (-not (Test-Path "$Scratch\electron.zip")) { Invoke-WebRequest "https://github.com/electron/electron/releases/download/v$ElectronVer/electron-v$ElectronVer-win32-x64.zip" -OutFile "$Scratch\electron.zip" -MaximumRedirection 10 }
Expand-Archive "$Scratch\electron.zip" -DestinationPath $Bin -Force
Rename-Item "$Bin\electron.exe" "$AppName.exe" -Force
Remove-Item "$Bin\resources\default_app.asar" -Force -EA SilentlyContinue
& "$env:WINDIR\System32\cmd.exe" /c "set NoDefaultCurrentDirectoryInExePath=&& npx --yes @electron/asar pack `"$RepoRoot\electron\app`" `"$Bin\resources\app.asar`"" | Out-Null
# silnik + DLL do bin
Copy-Item (Join-Path $RepoRoot "engine\jamovi-engine.exe") "$Bin\jamovi-engine.exe" -Force
Copy-Item "$Deps\nanomsg\bin\libnanomsg.dll" "$Bin\libnanomsg.dll" -Force
# R: payload\R = kopia R 4.6 + scalona user-lib
if (-not (Test-Path "$Payload\R\bin\x64\R.dll")) {
    & robocopy "$RHome" "$Payload\R" /E /NFL /NDL /NJH /NJS /MT:8 | Out-Null
    & robocopy "$UserLib" "$Payload\R\library" /E /XO /NFL /NDL /NJH /NJS /MT:8 | Out-Null
}
foreach ($d in 'R.dll','Rblas.dll','Rgraphapp.dll','Riconv.dll','Rlapack.dll') { Copy-Item "$Payload\R\bin\x64\$d" "$Bin\$d" -Force -EA SilentlyContinue }
# Frameworks / Resources (move = blyskawiczne na tym samym wolumenie)
Move-Item "$Payload\R"       "$AppDir\Frameworks\R"      -Force
Move-Item "$Payload\python"  "$AppDir\Frameworks\python" -Force
Move-Item "$Payload\modules" "$AppDir\Resources\modules" -Force
Move-Item "$Payload\client"  "$AppDir\Resources\client"  -Force
Move-Item "$Payload\server"  "$AppDir\Resources\server"  -Force
Copy-Item "$Payload\i18n\json\*" "$AppDir\Resources\i18n\" -Recurse -Force
Copy-Item (Join-Path $RepoRoot "version") "$AppDir\Resources\version" -Force
# nanomsg.dll obok python.exe (Py3.8+ ctypes)
Copy-Item "$Deps\nanomsg\bin\libnanomsg.dll" "$AppDir\Frameworks\python\nanomsg.dll" -Force
# env.conf (sciezki wzgledem bin; JAMOVI_R_VERSION = rVersion modulu; bez NETWORK_SANDBOX dla portable)
$rVer = (Select-String -Path "$AppDir\Resources\modules\jmv\jamovi-full.yaml" -Pattern "^rVersion:\s*(.+)$").Matches.Groups[1].Value.Trim()
@"
[ENV]

R_HOME=..\\Frameworks\\R
R_LIBS=..\\Resources\\modules\\base\\R
PATH=.;..\\bin;..\\Frameworks\\R\\bin\\x64
JAMOVI_HOME=..
JAMOVI_MODULES_PATH=../Resources/modules
JAMOVI_CLIENT_PATH=../Resources/client
JAMOVI_I18N_PATH=../Resources/i18n
JAMOVI_SERVER_CMD=../Frameworks/python/python -u -Xutf8 -m jamovi.server 0 --stdin-slave
JAMOVI_R_VERSION=$rVer
JAMOVI_VERSION_PATH=../Resources/version
PYTHONPATH=../Resources/server
"@ | ForEach-Object { [System.IO.File]::WriteAllText("$Bin\env.conf", $_) }
Info "OK montaz: $AppDir"

# ---------------------------------------------------------------------------
# FAZA 9 — pakowanie portable .zip
# ---------------------------------------------------------------------------
Step "Pakowanie portable .zip"
$zip = "$Dist\$AppName-$JupwrVer-portable-win64.zip"
if (Test-Path $zip) { Remove-Item $zip -Force }
Compress-Archive -Path $AppDir -DestinationPath $zip
Write-Host "`nGOTOWE: $zip" -ForegroundColor Green
Write-Host "Uruchom: `"$Bin\$AppName.exe`"" -ForegroundColor Green
# NSIS (opcjonalnie, gdy zainstalowany makensis): makensis packaging\scripts\windows\jUPWR.nsi
