; jUPWR-legacy.nsi - installer NSIS wariantu Legacy (Windows 8.1 x64), per-user, bez admina.
; Kopia jUPWR.nsi z roznicami opisanymi w packaging/30-legacy-win81.md. NIE modyfikowac
; jUPWR.nsi zamiast tego pliku: czyta go release-check.sh i test_release_check.py.
; Budowanie (z katalogu packaging\scripts\windows\, po build.ps1 na galezi legacy/win81):
;   makensis jUPWR-legacy.nsi
;   makensis /DVERSION=1.0.4 jUPWR-legacy.nsi      ; nadpisanie wersji
; Zaklada gotowy payload w PAYLOAD (zmontowany jUPWR\ przez build.ps1 -> dist-legacy).
;
; Roznice wobec jUPWR.nsi:
;   * osobny katalog instalacji, osobny klucz Uninstall i osobne skroty (APPID) -
;     Legacy instaluje sie OBOK zwyklego jUPWR i nie odinstalowuje go w .onInit
;   * WinVer: <8.1 -> Abort; >=10 -> ostrzezenie "zainstaluj zwykla wersje" (MB_YESNO);
;     brak ucrtbase.dll -> ostrzezenie nieblokujace o KB2999226
;   * VERSION = ta sama co w client/common/jupwr.ts (bez sufiksu); "Legacy" jest w nazwie

!include FileFunc.nsh
!include LogicLib.nsh
!include WinVer.nsh
!insertmacro GetParent

!define APPNAME    "jUPWR"                    ; nazwa pliku exe w bin\ (z build.ps1)
!define APPID      "jUPWR-Legacy"             ; katalog instalacji, klucz rejestru, folder menu Start
!define APPDISPLAY "jUPWR Legacy"             ; nazwa widoczna dla studenta
!define COMPANY    "Uniwersytet Przyrodniczy we Wroclawiu"
!ifndef VERSION
  !define VERSION  "1.0.4"                   ; wersja jUPWR (client/common/jupwr.ts) - rownac z jUPWR.nsi
!endif
; wariant 1 (Electron 22, R 4.6): domyslnie; wariant 3 (R 4.1.3):
;   makensis /DDISTDIR=dist-legacy-r41 /DTAG=legacy-r41 jUPWR-legacy.nsi
!ifndef DISTDIR
  !define DISTDIR  "dist-legacy"
!endif
!ifndef TAG
  !define TAG      "legacy"
!endif
!define PAYLOAD    "..\..\build\${DISTDIR}\jUPWR"
!define ICON       "..\..\..\platform\app-icon.ico"

Name "${APPDISPLAY} ${VERSION}"
OutFile "..\..\build\${DISTDIR}\${APPNAME}-${VERSION}-${TAG}-win81-x64-setup.exe"
InstallDir "$LOCALAPPDATA\Programs\${APPID}"
RequestExecutionLevel user            ; instalacja per-user (bez admina, idealne dla pracowni)
SetCompressor /SOLID lzma
Unicode true
Icon "${ICON}"
UninstallIcon "${ICON}"
BrandingText "${APPDISPLAY} ${VERSION} (Windows 8.1) - ${COMPANY}"

Page directory
Page instfiles
UninstPage uninstConfirm
UninstPage instfiles

!define UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPID}"

Var ExistingUninstaller
Var ExistingVersion
Var ExistingUninstallerCopy
Var UninstallResult

Function .onInit
    ; --- wersja systemu (wariant tylko dla Windows 8.1) ---
    ${IfNot} ${AtLeastWin8.1}
        MessageBox MB_ICONSTOP|MB_OK \
            "${APPDISPLAY} wymaga systemu Windows 8.1 (64-bit).$\r$\nTen komputer ma starszy system." \
            /SD IDOK
        Abort
    ${EndIf}
    ${If} ${AtLeastWin10}
        MessageBox MB_ICONEXCLAMATION|MB_YESNO \
            "To jest wersja ${APPDISPLAY} przeznaczona dla Windows 8.1.$\r$\nNa tym komputerze (Windows 10 lub nowszy) zainstaluj zwykla wersje jUPWR.$\r$\n$\r$\nMimo to kontynuowac instalacje ${APPDISPLAY}?" \
            /SD IDNO IDYES +2
        Abort
    ${EndIf}
    ; Universal CRT (KB2999226) - bez niego nie ruszy Python, R ani silnik; nie blokujemy,
    ; bo sprawdzenie pliku nie jest pewne na wszystkich obrazach systemu
    IfFileExists "$SYSDIR\ucrtbase.dll" ucrtOk 0
        MessageBox MB_ICONEXCLAMATION|MB_OK \
            "Nie znaleziono $SYSDIR\ucrtbase.dll (Universal C Runtime, aktualizacja KB2999226).$\r$\nBez niej ${APPDISPLAY} moze sie nie uruchomic. Instalacja bedzie kontynuowana." \
            /SD IDOK
ucrtOk:

    ; --- poprzednia instalacja Legacy (tylko klucz APPID; zwyklego jUPWR nie ruszamy) ---
    ReadRegStr $ExistingUninstaller HKCU "${UNINST_KEY}" "UninstallString"
    StrCmp $ExistingUninstaller "" done
    ; wartosc bywa w cudzyslowach - IfFileExists by ja odrzucilo
    StrCpy $0 $ExistingUninstaller 1
    StrCmp $0 '"' 0 +2
    StrCpy $ExistingUninstaller $ExistingUninstaller -1 1

    ReadRegStr $ExistingVersion HKCU "${UNINST_KEY}" "DisplayVersion"
    StrCmp $ExistingVersion "" 0 haveVersion
    StrCpy $ExistingVersion "nieznana"

haveVersion:
    MessageBox MB_ICONQUESTION|MB_YESNO \
        "Wykryto istniejaca instalacje ${APPDISPLAY} $ExistingVersion.$\r$\n$\r$\nOdinstalowac ja przed instalacja ${APPDISPLAY} ${VERSION}?" \
        /SD IDYES IDYES uninstallExisting
    Abort "Instalacja przerwana. Odinstaluj poprzednia wersje ${APPDISPLAY} albo uruchom instalator ponownie i potwierdz odinstalowanie."

uninstallExisting:
    IfFileExists $ExistingUninstaller 0 missingUninstaller
    StrCpy $ExistingUninstallerCopy "$TEMP\${APPID}-previous-uninstall.exe"
    CopyFiles /SILENT $ExistingUninstaller $ExistingUninstallerCopy
    ; _?= wskazuje katalog programu; bez tego $INSTDIR kopii to %TEMP%,
    ; ktory sekcja deinstalacji skasowalaby przez RMDir /r
    ${GetParent} $ExistingUninstaller $0
    ExecWait '"$ExistingUninstallerCopy" /S _?=$0' $UninstallResult
    Delete $ExistingUninstallerCopy
    IntCmp $UninstallResult 0 done
    Abort "Nie udalo sie odinstalowac poprzedniej wersji ${APPDISPLAY}. Kod bledu: $UninstallResult"

missingUninstaller:
    MessageBox MB_ICONEXCLAMATION|MB_OK \
        "Wykryto wpis poprzedniej instalacji ${APPDISPLAY}, ale nie znaleziono pliku odinstalowania:$\r$\n$ExistingUninstaller$\r$\n$\r$\nInstalator bedzie kontynuowal i nadpisze istniejace pliki." \
        /SD IDOK

done:
FunctionEnd

Section "install"
    SetOutPath "$INSTDIR"
    File /r "${PAYLOAD}\*.*"
    File "/oname=app-icon.ico" "${ICON}"   ; ikona dla skrotow

    ; skroty (ikona UPWr) - osobny folder i osobna nazwa, zeby nie nadpisac skrotow zwyklego jUPWR
    CreateDirectory "$SMPROGRAMS\${APPDISPLAY}"
    CreateShortcut  "$SMPROGRAMS\${APPDISPLAY}\${APPDISPLAY}.lnk" "$INSTDIR\bin\${APPNAME}.exe" "" "$INSTDIR\app-icon.ico"
    CreateShortcut  "$DESKTOP\${APPDISPLAY}.lnk"                  "$INSTDIR\bin\${APPNAME}.exe" "" "$INSTDIR\app-icon.ico"

    ; rejestr - wpis w "Dodaj/usun programy"
    WriteRegStr   HKCU "${UNINST_KEY}" "DisplayName"     "${APPDISPLAY} ${VERSION}"
    WriteRegStr   HKCU "${UNINST_KEY}" "DisplayVersion"  "${VERSION}"
    WriteRegStr   HKCU "${UNINST_KEY}" "Publisher"       "${COMPANY}"
    WriteRegStr   HKCU "${UNINST_KEY}" "DisplayIcon"     "$INSTDIR\app-icon.ico"
    WriteRegStr   HKCU "${UNINST_KEY}" "InstallLocation" "$INSTDIR"
    WriteRegStr   HKCU "${UNINST_KEY}" "UninstallString" "$INSTDIR\uninstall.exe"
    WriteRegDWORD HKCU "${UNINST_KEY}" "NoModify" 1
    WriteRegDWORD HKCU "${UNINST_KEY}" "NoRepair" 1

    WriteUninstaller "$INSTDIR\uninstall.exe"
SectionEnd

Section "uninstall"
    Delete "$SMPROGRAMS\${APPDISPLAY}\${APPDISPLAY}.lnk"
    RMDir  "$SMPROGRAMS\${APPDISPLAY}"
    Delete "$DESKTOP\${APPDISPLAY}.lnk"
    DeleteRegKey HKCU "${UNINST_KEY}"
    RMDir /r "$INSTDIR"
SectionEnd
