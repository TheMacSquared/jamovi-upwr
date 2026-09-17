# legacy-diag.ps1 - diagnostyka jUPWR na Windows 8.1 x64 (Faza 0 planu Legacy,
# patrz packaging/30-legacy-win81.md). Dziala na PowerShell 4.0 (Windows 8.1),
# bez admina, NICZEGO nie instaluje i nie zmienia w systemie.
#
# Przygotowanie pendrive'a:
#   1. rozpakuj packaging/build/dist/jUPWR-<wersja>-portable-win64.zip -> <pendrive>\jUPWR\
#      (katalog z bin\, Frameworks\, Resources\)
#   2. skopiuj ten skrypt do <pendrive>\legacy-diag.ps1
#   3. (opcjonalnie) plik .omv do testu w przegladarce
# Uruchomienie w sali (ExecutionPolicy na 8.1 to domyslnie Restricted):
#   powershell -ExecutionPolicy Bypass -File D:\legacy-diag.ps1
#   powershell -ExecutionPolicy Bypass -File D:\legacy-diag.ps1 -AppDir C:\jUPWR -NoGui
# Wynik: legacy-diag-<komputer>-<data>.log obok skryptu (albo -LogDir). Przywiez log.
#
# UWAGA: jesli ktorys program pokaze okno "Nie znaleziono punktu wejscia procedury
# ... w KERNEL32.dll", PRZEPISZ jego tresc do notatek i dopiero kliknij OK -
# skrypt czeka na zamkniecie okna. To jest wlasnie podpis niezgodnosci z 8.1.

param(
    [string]$AppDir = (Join-Path (Split-Path -Parent $MyInvocation.MyCommand.Path) "jUPWR"),
    [string]$LogDir = (Split-Path -Parent $MyInvocation.MyCommand.Path),
    [int]$Port = 41337,
    [switch]$NoGui        # pomin kroki wymagajace klikania (8: scenariusz w przegladarce, 9: okno jUPWR.exe)
)

$ErrorActionPreference = 'Continue'
$stamp   = Get-Date -Format 'yyyyMMdd-HHmmss'
$LogFile = Join-Path $LogDir ("legacy-diag-{0}-{1}.log" -f $env:COMPUTERNAME, $stamp)

function Log([string]$m) {
    Write-Host $m
    [System.IO.File]::AppendAllText($LogFile, $m + "`r`n", [System.Text.Encoding]::UTF8)
}
function Section([string]$t) { Log ""; Log ("=" * 72); Log "== $t"; Log ("=" * 72) }

# znane kody wyjscia Windows (NTSTATUS jako int32)
function DecodeExit($code) {
    if ($code -eq $null) { return "nie uruchomiono" }
    switch ([int]$code) {
        0            { return "OK" }
        -1073741511  { return "STATUS_ENTRYPOINT_NOT_FOUND (0xC0000139) = brak funkcji API w systemie -> NIEZGODNOSC Z 8.1" }
        -1073741515  { return "STATUS_DLL_NOT_FOUND (0xC0000135) = brak biblioteki DLL (UCRT? VC++ runtime?)" }
        -1073741701  { return "STATUS_INVALID_IMAGE_FORMAT (0xC000007B) = zla architektura DLL/EXE" }
        -1073741819  { return "STATUS_ACCESS_VIOLATION (0xC0000005) = crash" }
        default      { return "kod $code" }
    }
}

# uruchom program, przechwyc stdout+stderr do logu, zwroc kod wyjscia
function Run([string]$title, [string]$exe, [string[]]$argv) {
    Section $title
    Log ("> " + $exe + " " + ($argv -join " "))
    if (-not (Test-Path $exe)) { Log "BRAK PLIKU: $exe"; return $null }
    $out = ""; $code = $null
    try {
        $out  = (& $exe @argv 2>&1 | Out-String)
        $code = $LASTEXITCODE
    } catch {
        $out  = ($_ | Out-String); $code = -1
    }
    if ($out.Trim() -ne "") { Log $out.TrimEnd() }
    Log ("exit: " + (DecodeExit $code))
    return $code
}

Log "legacy-diag.ps1  $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')"
Log "AppDir: $AppDir"
Log "Log:    $LogFile"

# ---------------------------------------------------------------------------
Section "1. Maszyna"
# ---------------------------------------------------------------------------
$os = Get-WmiObject Win32_OperatingSystem
Log ("OS: {0}  wersja {1}  {2}" -f $os.Caption, $os.Version, $os.OSArchitecture)
Log ("PowerShell: " + $PSVersionTable.PSVersion)
$cs = Get-WmiObject Win32_ComputerSystem
Log ("Komputer: {0} {1}  RAM {2:N1} GB" -f $cs.Manufacturer, $cs.Model, ($cs.TotalPhysicalMemory / 1GB))
Get-WmiObject Win32_Processor       | ForEach-Object { Log ("CPU: " + $_.Name) }
Get-WmiObject Win32_VideoController | ForEach-Object { Log ("GPU: {0}  sterownik {1} ({2})" -f $_.Name, $_.DriverVersion, $_.DriverDate) }

# KB2919355 = Windows 8.1 Update (bez niego Electron/Chromium nie ruszy)
# KB2999226 = Universal CRT (bez niego nie ruszy Python, R ani silnik)
foreach ($kb in 'KB2919355', 'KB2999226') {
    $h = Get-HotFix -Id $kb -ErrorAction SilentlyContinue
    if ($h) { Log ("{0}: JEST (zainstalowano {1})" -f $kb, $h.InstalledOn) } else { Log "${kb}: BRAK wg Get-HotFix" }
}
$sys32 = Join-Path $env:SystemRoot 'System32'
foreach ($d in 'ucrtbase.dll', 'msvcp140.dll', 'vcruntime140.dll', 'vcruntime140_1.dll', 'api-ms-win-crt-runtime-l1-1-0.dll') {
    $p = Join-Path $sys32 $d
    if (Test-Path $p) { Log ("{0}: {1}" -f $d, (Get-Item $p).VersionInfo.FileVersion) } else { Log "${d}: BRAK w System32" }
}
$isAdmin = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
Log "Konto: $env:USERDOMAIN\$env:USERNAME  administrator: $isAdmin"
try {
    Get-WmiObject -Namespace root\SecurityCenter2 -Class AntiVirusProduct -ErrorAction Stop | ForEach-Object { Log ("Antywirus: " + $_.displayName) }
} catch { Log "Antywirus: nie udalo sie odczytac (SecurityCenter2)" }
foreach ($b in 'chrome.exe', 'msedge.exe', 'firefox.exe') {
    $k = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\$b"
    if (Test-Path $k) {
        $bp = (Get-ItemProperty $k).'(default)'
        if ($bp -and (Test-Path $bp)) { Log ("Przegladarka: {0} {1}" -f $b, (Get-Item $bp).VersionInfo.ProductVersion) }
    }
}
Log ("Sciezka aplikacji zawiera spacje: " + ($AppDir -match ' '))
try { Log ("Wolne miejsce na dysku aplikacji: {0:N1} GB" -f ((Get-PSDrive ($AppDir.Substring(0, 1))).Free / 1GB)) } catch { }

# ---------------------------------------------------------------------------
Section "2. Srodowisko z bin\env.conf (jak robi to Electron w main.js)"
# ---------------------------------------------------------------------------
$Bin  = Join-Path $AppDir 'bin'
$conf = Join-Path $Bin 'env.conf'
if (-not (Test-Path $conf)) { Log "BRAK $conf - AppDir nie wskazuje na rozpakowany portable jUPWR. Koniec."; exit 1 }
$origPath = $env:PATH
Get-Content $conf | ForEach-Object {
    $line = $_.Trim()
    if ($line -eq '' -or $line.StartsWith('[') -or $line.StartsWith('#')) { return }
    $i = $line.IndexOf('='); if ($i -lt 1) { return }
    $k = $line.Substring(0, $i).Trim()
    $v = $line.Substring($i + 1).Trim() -replace '\\\\', '\'      # ini: \\ -> \
    if ($k -match '(PATH|HOME|LIBS)$') {
        $v = (($v -split ';') | ForEach-Object {
            if ($_ -match '^\.\.?([\\/]|$)') { [System.IO.Path]::GetFullPath((Join-Path $Bin $_)) } else { $_ }
        }) -join ';'
    }
    if ($k -eq 'PATH') { $v = $v + ';' + $origPath }
    Set-Item -Path "env:$k" -Value $v
    Log "$k=$v"
}

$Py  = Join-Path $AppDir 'Frameworks\python\python.exe'
$Rs  = Join-Path $AppDir 'Frameworks\R\bin\x64\Rscript.exe'
$Eng = Join-Path $Bin 'jamovi-engine.exe'
$Exe = Join-Path $Bin 'jUPWR.exe'
foreach ($f in $Py, $Rs, $Eng, $Exe) { if (Test-Path $f) { Log "jest: $f" } else { Log "BRAK: $f" } }
Set-Location $Bin

# kod do plikow tymczasowych - PowerShell 4 zle cytuje argumenty z cudzyslowami
$tmp = Join-Path $env:TEMP "legacy-diag-$stamp"
New-Item -ItemType Directory -Path $tmp -Force | Out-Null
$pyTest = Join-Path $tmp 'imports.py'
@'
import sys
print(sys.version)
import jamovi.core
print("jamovi.core OK")
import jamovi.server
print("jamovi.server OK")
import nanomsg
print("nanomsg OK")
print("IMPORT OK")
'@ | Set-Content -Path $pyTest -Encoding ASCII
$rInfo = Join-Path $tmp 'info.R'
@'
cat(R.version.string, "\n")
cat("R_HOME:", R.home(), "\n")
cat(".libPaths():", paste(.libPaths(), collapse = "; "), "\n")
print(sessionInfo())
'@ | Set-Content -Path $rInfo -Encoding ASCII
$rPkgs = Join-Path $tmp 'pkgs.R'
@'
pk <- c("jmvcore", "ggplot2", "car", "emmeans", "afex", "lavaan", "BayesFactor",
        "psych", "Hmisc", "lpSolve", "boot", "mvtnorm", "ragg", "systemfonts")
for (p in pk) {
  ok <- tryCatch(suppressWarnings(suppressMessages(require(p, character.only = TRUE, quietly = TRUE))),
                 error = function(e) { cat("  ", conditionMessage(e), "\n"); FALSE })
  cat(sprintf("%-12s %s\n", p, if (isTRUE(ok)) "OK" else "BRAK/BLAD"))
}
'@ | Set-Content -Path $rPkgs -Encoding ASCII

# ---------------------------------------------------------------------------
$c3 = Run "3. Python (Frameworks\python)" $Py @('-VV')
$c4 = Run "4. Import jamovi.core / jamovi.server / nanomsg  (test rozstrzygajacy dla backendu)" $Py @('-X', 'utf8', $pyTest)
$c5 = Run "5. R sessionInfo" $Rs @('--vanilla', $rInfo)
$c6 = Run "6. Pakiety R (rdzen modulow wbudowanych)" $Rs @('--vanilla', $rPkgs)
$c7 = Run "7. Silnik jamovi-engine.exe bez argumentow (liczy sie JAK pada: blad o argumentach = binarka sie ladowala)" $Eng @()

# ---------------------------------------------------------------------------
Section "8. Serwer bez Electrona: python -m jamovi.server $Port --start-wb"
# ---------------------------------------------------------------------------
$srvOut = Join-Path $LogDir "legacy-diag-server-$stamp.out.log"
$srvErr = Join-Path $LogDir "legacy-diag-server-$stamp.err.log"
$srvArgs = @('-u', '-X', 'utf8', '-m', 'jamovi.server', "$Port", '--start-wb')
Log ("> " + $Py + " " + ($srvArgs -join ' '))
$srv = $null; $http = $false; $srvUrl = $null
try {
    $srv = Start-Process -FilePath $Py -ArgumentList $srvArgs -WorkingDirectory $Bin `
        -RedirectStandardOutput $srvOut -RedirectStandardError $srvErr -PassThru -NoNewWindow
    # serwer NIE uzywa podanego portu: losuje trzy wlasne i wypisuje na stdout
    # "jamovi accessible from: 127.0.0.1:<port>/?access_key=<klucz>" - bez klucza HTTP odmawia
    $deadline = (Get-Date).AddSeconds(120)
    while ((Get-Date) -lt $deadline) {
        if ($srv.HasExited) { break }
        if (-not $srvUrl -and (Test-Path $srvOut)) {
            $m = Select-String -Path $srvOut -Pattern 'accessible from:\s*(\S+)' | Select-Object -First 1
            if ($m) { $srvUrl = "http://" + $m.Matches[0].Groups[1].Value; Log "adres serwera: $srvUrl" }
        }
        if ($srvUrl) {
            try {
                $r = Invoke-WebRequest -Uri $srvUrl -UseBasicParsing -TimeoutSec 3 -ErrorAction Stop
                if ($r.StatusCode -eq 200) { $http = $true; break }
            } catch { }
        }
        Start-Sleep -Seconds 2
    }
    if ($srv.HasExited) {
        Log ("serwer zakonczyl sie sam: " + (DecodeExit $srv.ExitCode))
    } elseif ($http) {
        Log "HTTP 200 z $srvUrl - serwer dziala"
        Log ("procesy silnika: " + ((Get-Process jamovi-engine -ErrorAction SilentlyContinue | Measure-Object).Count))
        if (-not $NoGui) {
            Log "W przegladarce (otworzyla sie sama albo wejdz na $srvUrl):"
            Log "  otworz plik .omv (ikona folderu) -> Eksploracja -> Zmienne ilosciowe -> dodaj zmienna"
            Log "  oczekiwane: tabela + wykres (nie wieczny spinner)"
            $ans = Read-Host "Wpisz wynik (np. 'OK tabela i wykres' albo opis bledu) i nacisnij Enter"
            Log "WYNIK KROKU 8 (reka): $ans"
        }
    } else {
        Log "serwer nie odpowiedzial na HTTP w 120 s (proces zyje: $(-not $srv.HasExited); adres z logu: $srvUrl)"
    }
} catch {
    Log ("blad uruchamiania serwera: " + $_)
} finally {
    if ($srv -and -not $srv.HasExited) { Stop-Process -Id $srv.Id -Force -ErrorAction SilentlyContinue }
    Get-Process jamovi-engine -ErrorAction SilentlyContinue | Stop-Process -Force -ErrorAction SilentlyContinue
    Start-Sleep -Seconds 1
    foreach ($f in $srvOut, $srvErr) {
        if (Test-Path $f) {
            Log "--- $(Split-Path -Leaf $f) (ostatnie 40 linii) ---"
            Get-Content $f -Tail 40 | ForEach-Object { Log $_ }
        }
    }
}

# ---------------------------------------------------------------------------
Section "9. jUPWR.exe (Electron) - oczekiwany blad na 8.1; zanotuj tresc okna"
# ---------------------------------------------------------------------------
$t0 = Get-Date
$exeOut = Join-Path $LogDir "legacy-diag-electron-$stamp.out.log"
$exeErr = Join-Path $LogDir "legacy-diag-electron-$stamp.err.log"
if (-not (Test-Path $Exe)) {
    Log "BRAK: $Exe"
} elseif ($NoGui) {
    Log "pominiete (-NoGui)"
} else {
    $app = Start-Process -FilePath $Exe -WorkingDirectory $Bin -RedirectStandardOutput $exeOut -RedirectStandardError $exeErr -PassThru
    Start-Sleep -Seconds 20
    if ($app.HasExited) {
        Log ("jUPWR.exe zakonczyl sie po <20 s: " + (DecodeExit $app.ExitCode))
    } else {
        Log "jUPWR.exe zyje (PID $($app.Id)) po 20 s - albo dziala, albo wisi na oknie bledu"
        $ans = Read-Host "Co widac? (okno jUPWR / okno bledu - przepisz tresc / nic). Enter zamyka proces"
        Log "WYNIK KROKU 9 (reka): $ans"
        Stop-Process -Id $app.Id -Force -ErrorAction SilentlyContinue
        Get-Process jamovi-engine -ErrorAction SilentlyContinue | Stop-Process -Force -ErrorAction SilentlyContinue
    }
    foreach ($f in $exeOut, $exeErr) {
        if (Test-Path $f) { Log "--- $(Split-Path -Leaf $f) ---"; Get-Content $f -Tail 40 | ForEach-Object { Log $_ } }
    }
    Log "--- Dziennik zdarzen Application od startu kroku 9 ---"
    try {
        Get-EventLog -LogName Application -After $t0 -ErrorAction Stop |
            Where-Object { $_.EntryType -eq 'Error' } |
            ForEach-Object { Log ("[{0}] {1}: {2}" -f $_.TimeGenerated, $_.Source, ($_.Message -replace "`r?`n", ' ')) }
    } catch { Log "nie udalo sie odczytac dziennika: $_" }
}

# ---------------------------------------------------------------------------
Section "10. Punkt odniesienia: oryginalne jamovi 2.3.28 (recznie)"
# ---------------------------------------------------------------------------
Log "Zainstaluj/uruchom jamovi 2.3.28 z pendrive'a i zanotuj: instaluje sie? startuje? liczy analize?"
Log "Jesli 2.3.28 TEZ nie dziala, problem jest w systemie (UCRT/KB), nie w Electronie."

# ---------------------------------------------------------------------------
Section "PODSUMOWANIE"
# ---------------------------------------------------------------------------
Log ("3 python -VV:        " + (DecodeExit $c3))
Log ("4 import jamovi:     " + (DecodeExit $c4))
Log ("5 Rscript:           " + (DecodeExit $c5))
Log ("6 pakiety R:         " + (DecodeExit $c6) + "  (szczegoly wyzej)")
Log ("7 jamovi-engine.exe: " + (DecodeExit $c7) + "  (blad o argumentach = OK)")
if ($http) { Log "8 serwer HTTP:       OK" } else { Log "8 serwer HTTP:       NIE" }
Log ""
$utf8bad = 0
foreach ($f in $srvOut, $srvErr, $exeOut, $exeErr) {
    if (Test-Path $f) { $utf8bad += (Select-String -Path $f -Pattern 'invalid UTF-8|bad UTF-8|Restarting engine' | Measure-Object).Count }
}
if ($utf8bad -gt 0) {
    Log "UWAGA: w logach serwera/Electrona sa wpisy 'invalid UTF-8' / 'Restarting engine' ($utf8bad)."
    Log "  To NIE jest limit zasobow ani problem toolchainu: R pracuje w code page systemu (brak locale"
    Log "  .UTF-8), a napisy trafiaja do protobufa bez konwersji. Paczka bez poprawki jmvcore (pbstr)."
}
Log "Brama decyzyjna (packaging/30-legacy-win81.md):"
Log "  9 OK i analiza z tabela liczy sie  -> paczka gotowa do pilotazu"
Log "  9 OK, tabele 'przerwane'            -> patrz UWAGA wyzej (kodowanie), nie toolchain"
Log "  8 OK, 9 pada                        -> sprawdz KB2919355 i GPU; wariant bez Electrona"
Log "  4 pada / 5 pada                     -> paczka r41 (R 4.1.3, bez UCRT); potem IT"
Log "  brak KB2999226/ucrtbase            -> paczka r41 (silnik i R bez UCRT)"
Log ""
Log "Log zapisany: $LogFile"
Remove-Item $tmp -Recurse -Force -ErrorAction SilentlyContinue
