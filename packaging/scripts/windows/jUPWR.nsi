; jUPWR.nsi — SZKIELET installera NSIS (DO PRZETESTOWANIA na Windows).
; Buduj: makensis jUPWR.nsi
; Zakłada gotowy payload w katalogu PAYLOAD (zmontowany jUPWR\ z .exe, env.conf, jamovi\, jamovi-engine.exe).

!define APPNAME    "jUPWR"
!define COMPANY    "Uniwersytet Przyrodniczy we Wroclawiu"
!define VERSION    "2.7.35.0"          ; zsynchronizuj z plikiem version
!define PAYLOAD    "..\..\build\dist\jUPWR"   ; DOSTOSUJ do wyniku montażu

Name "${APPNAME} ${VERSION}"
OutFile "..\..\build\dist\${APPNAME}-${VERSION}-x64-setup.exe"
InstallDir "$LOCALAPPDATA\Programs\${APPNAME}"
RequestExecutionLevel user            ; instalacja per-user (bez admina)
SetCompressor /SOLID lzma
Unicode true

Page directory
Page instfiles
UninstPage uninstConfirm
UninstPage instfiles

Section "install"
    SetOutPath "$INSTDIR"
    File /r "${PAYLOAD}\*.*"

    ; skróty
    CreateDirectory "$SMPROGRAMS\${APPNAME}"
    CreateShortcut  "$SMPROGRAMS\${APPNAME}\${APPNAME}.lnk" "$INSTDIR\${APPNAME}.exe"
    CreateShortcut  "$DESKTOP\${APPNAME}.lnk"               "$INSTDIR\${APPNAME}.exe"

    ; rejestr (deinstalacja + wersja)
    WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" \
        "DisplayName" "${APPNAME}"
    WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" \
        "DisplayVersion" "${VERSION}"
    WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" \
        "Publisher" "${COMPANY}"
    WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" \
        "UninstallString" "$INSTDIR\uninstall.exe"

    ; skojarzenie .omv (opcjonalne) — patrz platform\jamovi-dataset.xml
    ; WriteRegStr HKCU "Software\Classes\.omv" "" "jUPWR.Dataset"

    WriteUninstaller "$INSTDIR\uninstall.exe"
SectionEnd

Section "uninstall"
    Delete "$SMPROGRAMS\${APPNAME}\${APPNAME}.lnk"
    RMDir  "$SMPROGRAMS\${APPNAME}"
    Delete "$DESKTOP\${APPNAME}.lnk"
    DeleteRegKey HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}"
    RMDir /r "$INSTDIR"
SectionEnd
