# build.ps1 — SZKIELET buildu jUPWR na Windows x64 (DO PRZETESTOWANIA).
# Odpowiednik scripts/macos/*.sh. Uruchamiać w PowerShell z dostępem do:
#   R 4.x, Python 3.12, Node 22+, RTools 4.5, VS2022 Build Tools, Boost 1.84, NSIS.
# Patrz packaging/20-build-windows.md.

$ErrorActionPreference = "Stop"
$RepoRoot   = (Resolve-Path "$PSScriptRoot\..\..\..").Path
$BuildDir   = Join-Path $RepoRoot "packaging\build"
$Stage      = Join-Path $BuildDir "stage"
$Payload    = Join-Path $Stage "jamovi"
$Dist       = Join-Path $BuildDir "dist"
$AppName    = "jUPWR"
$Version    = (Get-Content (Join-Path $RepoRoot "version")).Trim()
$RHome      = "C:\Program Files\R\R-4.5.0"            # DOSTOSUJ
$Py         = "python"                                  # 3.12 x64 na PATH
$ElectronVer= "32.3.3"

New-Item -ItemType Directory -Force -Path $Payload,$Dist | Out-Null

function Step($m){ Write-Host "==> $m" -ForegroundColor Cyan }

# --- 1. KLIENT ---
Step "Klient (vite)"
Push-Location (Join-Path $RepoRoot "client")
if (-not (Test-Path node_modules)) { npm install }
npm run build:coms
node .\node_modules\vite\bin\vite.js build --outDir "$Payload\client" --emptyOutDir
Pop-Location

# --- 2. MODUŁY R ---
Step "Moduły R (jmvcore + jmc)"
$BaseR = "$Payload\modules\base\R"; New-Item -ItemType Directory -Force -Path $BaseR | Out-Null
& R CMD INSTALL (Join-Path $RepoRoot "jmvcore") "--library=$BaseR"
Push-Location (Join-Path $RepoRoot "jamovi-compiler")
if (-not (Test-Path node_modules)) { npm install }
Pop-Location
$jmc = Join-Path $RepoRoot "jamovi-compiler\index.js"
foreach ($m in @("jmv","plots","jperm","jCI","jboot","jdistrACTION")) {
    $src = Join-Path $RepoRoot $m
    if (Test-Path $src) {
        node $jmc --install $src --to "$Payload\modules" --rhome "$RHome" --rlibs $BaseR --patch-version --skip-deps
    }
}

# --- 3. i18n ---
Step "i18n"
Push-Location (Join-Path $RepoRoot "i18n")
if (-not (Test-Path node_modules)) { npm install }
node (Join-Path $RepoRoot "i18n\index.js") --build src --dest "$Payload\i18n\json"
Pop-Location

# --- 4. SERWER ---
Step "Serwer Python"
$PyVenv = "$Payload\python"
& $Py -m venv $PyVenv
$VPy = "$PyVenv\Scripts\python.exe"
& $VPy -m pip install --upgrade pip wheel setuptools
& $VPy -m pip install -r (Join-Path $RepoRoot "server\requirements.txt")
& $VPy -m pip install --upgrade protobuf   # dopasowanie do protoc (patrz docs)
New-Item -ItemType Directory -Force -Path "$Payload\server" | Out-Null
Push-Location (Join-Path $RepoRoot "readstat")
& $VPy setup.py install "--install-lib=$Payload\server" --single-version-externally-managed --record=NUL
Pop-Location
Push-Location (Join-Path $RepoRoot "server")
$env:SETUP_CORE_ONLY="1";   & $VPy setup.py install "--install-lib=$Payload\server" --single-version-externally-managed --record=NUL; $env:SETUP_CORE_ONLY=""
$env:SETUP_SERVER_ONLY="1"; & $VPy setup.py install "--install-lib=$Payload\server" --single-version-externally-managed --record=NUL; $env:SETUP_SERVER_ONLY=""
Pop-Location

# --- 5. SILNIK ---
Step "Silnik (engine) — przez powłokę RTools/mingw"
# UWAGA: do uruchomienia w środowisku RTools (make + mingw). Patrz Makefile.in blok Windows.
Push-Location (Join-Path $RepoRoot "engine")
& cmd /c "configure.bat --rhome=`"$RHome`""
& make
Copy-Item jamovi-engine.exe "$Payload\..\jamovi-engine.exe" -Force   # umieszczany potem obok .exe
Pop-Location

# --- 6. MONTAŻ + 7. INSTALLER: patrz jUPWR.nsi i 20-build-windows.md ---
Step "Pobierz Electron $ElectronVer (win32-x64), zmontuj katalog jUPWR\, env.conf obok .exe, makensis jUPWR.nsi"
Write-Host "Kroki montażu/NSIS — do uzupełnienia/testów na Windows (szkielet)." -ForegroundColor Yellow
