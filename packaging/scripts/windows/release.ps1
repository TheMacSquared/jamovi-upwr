# release.ps1 - jedno wejscie do pelnego wydania jUPWR na Windows x64.
#
# Buduje linie wydan wyewidencjonowana w katalogu, w ktorym lezy skrypt. Kazda linia
# (main = latest, release/X.Y = stable) ma wlasny katalog worktree z wlasnym
# packaging\build, node_modules i submodulami - patrz packaging/CHANNELS.md.
#   [opcjonalnie] Docker -> build.ps1 (klient, moduly, silnik, bundle, portable .zip, .jmo)
#   -> instalator NSIS -> release-check.sh (z artefaktami) -> RELEASE-INFO-<wersja>.txt
#
# Uzycie (w katalogu worktree danej linii):
#   pwsh -ExecutionPolicy Bypass -File packaging\scripts\windows\release.ps1   (PowerShell 7)
#   ... -Docker        # najpierw obraz Dockera (kolejnosc z CLAUDE.md: Docker przed natywnym)
#   ... -SkipNsis      # bez instalatora .exe (np. tylko portable .zip + .jmo)
#
# Wynik: packaging\build\dist\
#   jUPWR-<wersja>-portable-win64.zip, jUPWR-<wersja>-x64-setup.exe,
#   <modul>_<wersja>-win64.jmo (moduly opcjonalne pod jamovi z pliku `version` TEJ linii),
#   RELEASE-INFO-<wersja>.txt (galaz, commit, wersja jamovi, data, lista artefaktow).

[CmdletBinding()]
param(
    [switch]$Docker,
    [switch]$SkipNsis
)

$ErrorActionPreference = "Stop"
if ($PSVersionTable.PSVersion.Major -lt 7) {
    throw "Uruchom przez pwsh (PowerShell 7): Windows PowerShell 5.1 nie parsuje build.ps1"
}
$RepoRoot = (Resolve-Path "$PSScriptRoot\..\..\..").Path
Set-Location $RepoRoot

function Step($m) { Write-Host "`n==> $m" -ForegroundColor Cyan }
function Info($m) { Write-Host "    $m" -ForegroundColor DarkGray }
function Git {
    $out = & git.exe @args 2>&1
    if ($LASTEXITCODE -ne 0) { throw "git $($args -join ' ') -> kod $LASTEXITCODE`n$out" }
    $out
}

# --- identyfikacja wydania ---------------------------------------------------
$Jupwr = (Select-String (Join-Path $RepoRoot "client\common\jupwr.ts") `
          -Pattern "JUPWR_VERSION\s*=\s*'([0-9.]+)'").Matches.Groups[1].Value
if (-not $Jupwr) { throw "Nie moge odczytac JUPWR_VERSION z client\common\jupwr.ts" }
$Jamovi = (Get-Content (Join-Path $RepoRoot "version")).Trim()
$Branch = (Git rev-parse --abbrev-ref HEAD) | Select-Object -First 1
$Commit = (Git rev-parse --short HEAD) | Select-Object -First 1
$Build  = Join-Path $RepoRoot "packaging\build"
$Dist   = Join-Path $Build "dist"
$Logs   = Join-Path $Build "logs"
New-Item -ItemType Directory -Force $Dist,$Logs | Out-Null

Step "jUPWR $Jupwr (jamovi $Jamovi) - galaz $Branch @ $Commit"
if ($Branch -ne 'main' -and $Branch -notlike 'release/*') {
    throw "Galaz $Branch nie jest linia wydan (main / release/*) - patrz packaging/CHANNELS.md"
}

# --- spojnosc wersji (szybko, przed wielogodzinnym buildem) ---------------------
# Pelna kontrola (seria vs galaz, CHANNELS.md, submoduly, .jmo, MODULES.md) to
# release-check.sh na koncu - tu tylko to, co bez buildu wywrociloby wydanie.
$compose = (Select-String (Join-Path $RepoRoot "docker-compose.yaml") -Pattern 'image:\s*jupwr/jupwr:([0-9.]+)').Matches.Groups[1].Value
$nsi     = (Select-String (Join-Path $RepoRoot "packaging\scripts\windows\jUPWR.nsi") -Pattern '!define\s+VERSION\s+"([0-9.]+)"').Matches.Groups[1].Value
if ($compose -ne $Jupwr) { throw "Niespojna wersja: jupwr.ts=$Jupwr, docker-compose.yaml=$compose" }
if ($nsi     -ne $Jupwr) { throw "Niespojna wersja: jupwr.ts=$Jupwr, jUPWR.nsi=$nsi" }
Info "wersje spojne (klient = Docker = NSIS = $Jupwr)"

# --- submoduly na wlasciwych commitach ---------------------------------------
# --recursive jest konieczne (zagniezdzone submoduly); NIE uzywac `--remote`, bo
# przestawiloby submoduly na koniec sledzonej galezi zamiast na zapisany wskaznik.
Step "Submoduly"
Git submodule update --init --recursive | Out-Null
Info "OK"

# --- brudne drzewo -------------------------------------------------------------
# build.ps1 przywraca pliki, ktore przepisuje jmc, a npm ci nie zapisuje lockfile'a -
# cokolwiek tu widac, trafi do buildu bez commita.
$dirty = Git status --porcelain
if ($dirty) {
    Write-Warning "Niezacommitowane zmiany w drzewie (build ich uzyje):"
    $dirty | ForEach-Object { Write-Host "      $_" -ForegroundColor Yellow }
}

# --- Docker ------------------------------------------------------------------
if ($Docker) {
    Step "Docker (compose --profile main build)"
    & docker compose --profile main build *> (Join-Path $Logs "docker-$Jupwr.log")
    if ($LASTEXITCODE -ne 0) {
        Get-Content (Join-Path $Logs "docker-$Jupwr.log") -Tail 25
        throw "Build Dockera nieudany - natywnego nie zaczynam (kolejnosc z CLAUDE.md)"
    }
    Info "OK jupwr/jupwr:$Jupwr"
}

# --- build natywny ------------------------------------------------------------
Step "build.ps1 (klient, moduly, silnik, bundle, .zip, .jmo)"
$log = Join-Path $Logs "build-$Jupwr.log"
# ten sam host co release.ps1 (pwsh 7): Windows PowerShell 5.1 nie parsuje build.ps1
# (czyta UTF-8 bez BOM jako ANSI, a build.ps1 ma znaki spoza ASCII w napisach)
& (Get-Process -Id $PID).Path -NoProfile -ExecutionPolicy Bypass -File (Join-Path $PSScriptRoot "build.ps1") *> $log
if ($LASTEXITCODE -ne 0) {
    Get-Content $log -Tail 30
    throw "build.ps1 nieudany (pelny log: $log)"
}
Info "OK (log: $log)"

# --- drzewo po buildzie ----------------------------------------------------------
# jmc zapisuje CRLF w regenerowanych *.h.R / *.u.yaml; .gitattributes normalizuje je do
# LF, wiec tresc jest ta sama, ale wpis w indeksie bywa nieaktualny i `git status`
# pokazuje "m jmv" przy pustym diffie (update-index --refresh nie pomaga). Odswiezamy
# sam indeks submodulow - pliki na dysku bez zmian, prawdziwe zmiany zostaja widoczne.
Step "Drzewo po buildzie"
Git submodule foreach --recursive --quiet "git add --renormalize . && git reset -q" | Out-Null
$dirty = Git status --porcelain
if ($dirty) {
    Write-Warning "Zmiany w drzewie po buildzie (artefakty builda albo jmc - nie commitowac):"
    $dirty | ForEach-Object { Write-Host "      $_" -ForegroundColor Yellow }
} else {
    Info "czyste"
}

# --- instalator NSIS ----------------------------------------------------------
if (-not $SkipNsis) {
    Step "Instalator NSIS"
    $nsis = (Get-Command makensis -ErrorAction SilentlyContinue).Source
    if (-not $nsis) { $nsis = "C:\Program Files (x86)\NSIS\makensis.exe" }
    if (-not (Test-Path $nsis)) { throw "Brak makensis (zainstaluj NSIS albo uzyj -SkipNsis)" }
    & $nsis (Join-Path $PSScriptRoot "jUPWR.nsi") *> (Join-Path $Logs "nsis-$Jupwr.log")
    if ($LASTEXITCODE -ne 0) { throw "makensis nieudany (log: $Logs\nsis-$Jupwr.log)" }
    Info "OK"
}

# --- artefakty tego przebiegu ---------------------------------------------------
Step "Artefakty"
$artifacts = @()
foreach ($pat in "jUPWR-$Jupwr-portable-win64.zip", "jUPWR-$Jupwr-x64-setup.exe") {
    $f = Join-Path $Dist $pat
    if (Test-Path $f) { $artifacts += $f }
    elseif ($pat -like '*setup.exe' -and $SkipNsis) { continue }
    else { throw "Brak oczekiwanego pliku: $f" }
}
# .jmo: dokladnie te, ktore ten przebieg zbudowal (z logu), a nie wszystko co lezy w dist\
$jmos = Select-String $log -Pattern '\.jmo OK \((.+)\)' | ForEach-Object { $_.Matches.Groups[1].Value }
if (-not $jmos) { throw "Log nie zawiera zadnego '.jmo OK' - moduly opcjonalne sie nie zbudowaly" }
foreach ($j in $jmos) {
    if (-not (Test-Path $j)) { throw "Brak pliku z logu: $j" }
    $artifacts += $j
}

# --- release-check.sh (pelny, z artefaktami) -----------------------------------
# bash z Git for Windows, NIE System32\bash.exe (to WSL, inny system plikow).
Step "release-check.sh"
$gitBash = Join-Path (Split-Path (Split-Path (Get-Command git.exe).Source)) "bin\bash.exe"
if (Test-Path $gitBash) {
    & $gitBash packaging/scripts/release-check.sh
    if ($LASTEXITCODE -ne 0) { Write-Warning "release-check.sh zglosil ostrzezenia - wydanie niespojne (patrz wyzej)" }
} else {
    Write-Warning "Brak $gitBash - uruchom packaging/scripts/release-check.sh recznie"
}

$info = Join-Path $Dist "RELEASE-INFO-$Jupwr.txt"
@(
    "jUPWR $Jupwr"
    "jamovi $Jamovi (--assume-app-version dla .jmo)"
    "galaz  $Branch"
    "commit $Commit"
    "data   $(Get-Date -Format 'yyyy-MM-dd HH:mm')"
    "maszyna Windows native ($env:COMPUTERNAME)"
    "docker $(if ($Docker) { 'zbudowany w tym przebiegu' } else { 'NIE budowany w tym przebiegu' })"
    ""
    "artefakty:"
) + ($artifacts | ForEach-Object { "  $(Split-Path -Leaf $_)" }) | Set-Content $info -Encoding UTF8

$artifacts | ForEach-Object { Get-Item $_ } |
    Select-Object Name, @{n='MB'; e={[math]::Round($_.Length/1MB, 1)}} | Format-Table -AutoSize | Out-String | Write-Host

Write-Host "GOTOWE: $Dist" -ForegroundColor Green
Write-Host "To NIE jest weryfikacja: paczka nie byla uruchamiana, macOS i Linux buduje sie na tamtych systemach." -ForegroundColor DarkGray
