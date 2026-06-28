; jUPWR.nsi — installer NSIS (per-user, bez admina) dla natywnego buildu Windows.
; Budowanie (z katalogu packaging\scripts\windows\):
;   makensis jUPWR.nsi
;   makensis /DVERSION=0.5.0 jUPWR.nsi      ; nadpisanie wersji
; Zaklada gotowy payload w PAYLOAD (zmontowany jUPWR\ przez build.ps1).

!define APPNAME    "jUPWR"
!define COMPANY    "Uniwersytet Przyrodniczy we Wroclawiu"
!ifndef VERSION
  !define VERSION  "0.5.0"                 ; wersja jUPWR (client/common/jupwr.ts)
!endif
!define PAYLOAD    "..\..\build\dist\jUPWR"
!define ICON       "..\..\..\platform\app-icon.ico"

Name "${APPNAME} ${VERSION}"
OutFile "..\..\build\dist\${APPNAME}-${VERSION}-x64-setup.exe"
InstallDir "$LOCALAPPDATA\Programs\${APPNAME}"
RequestExecutionLevel user            ; instalacja per-user (bez admina, idealne dla pracowni)
SetCompressor /SOLID lzma
Unicode true
Icon "${ICON}"
UninstallIcon "${ICON}"
BrandingText "${APPNAME} ${VERSION} — ${COMPANY}"

Page directory
Page instfiles
UninstPage uninstConfirm
UninstPage instfiles

!define UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}"

Section "install"
    SetOutPath "$INSTDIR"
    File /r "${PAYLOAD}\*.*"
    File "/oname=app-icon.ico" "${ICON}"   ; ikona dla skrotow

    ; skroty (ikona UPWr)
    CreateDirectory "$SMPROGRAMS\${APPNAME}"
    CreateShortcut  "$SMPROGRAMS\${APPNAME}\${APPNAME}.lnk" "$INSTDIR\bin\${APPNAME}.exe" "" "$INSTDIR\app-icon.ico"
    CreateShortcut  "$DESKTOP\${APPNAME}.lnk"               "$INSTDIR\bin\${APPNAME}.exe" "" "$INSTDIR\app-icon.ico"

    ; rejestr — wpis w "Dodaj/usun programy"
    WriteRegStr   HKCU "${UNINST_KEY}" "DisplayName"     "${APPNAME} ${VERSION}"
    WriteRegStr   HKCU "${UNINST_KEY}" "DisplayVersion"  "${VERSION}"
    WriteRegStr   HKCU "${UNINST_KEY}" "Publisher"       "${COMPANY}"
    WriteRegStr   HKCU "${UNINST_KEY}" "DisplayIcon"     "$INSTDIR\app-icon.ico"
    WriteRegStr   HKCU "${UNINST_KEY}" "InstallLocation" "$INSTDIR"
    WriteRegStr   HKCU "${UNINST_KEY}" "UninstallString" "$INSTDIR\uninstall.exe"
    WriteRegDWORD HKCU "${UNINST_KEY}" "NoModify" 1
    WriteRegDWORD HKCU "${UNINST_KEY}" "NoRepair" 1

    ; skojarzenie .omv (opcjonalne) — odkomentuj, jesli potrzebne
    ; WriteRegStr HKCU "Software\Classes\.omv" "" "jUPWR.Dataset"

    WriteUninstaller "$INSTDIR\uninstall.exe"
SectionEnd

Section "uninstall"
    Delete "$SMPROGRAMS\${APPNAME}\${APPNAME}.lnk"
    RMDir  "$SMPROGRAMS\${APPNAME}"
    Delete "$DESKTOP\${APPNAME}.lnk"
    DeleteRegKey HKCU "${UNINST_KEY}"
    RMDir /r "$INSTDIR"
SectionEnd
