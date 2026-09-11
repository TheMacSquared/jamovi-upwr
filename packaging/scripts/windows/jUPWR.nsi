; jUPWR.nsi - installer NSIS dla natywnego buildu Windows (tryb per-user LUB dla wszystkich).
; Budowanie (z katalogu packaging\scripts\windows\):
;   makensis jUPWR.nsi
;   makensis /DVERSION=0.7.7 jUPWR.nsi      ; nadpisanie wersji
; Zaklada gotowy payload w PAYLOAD (zmontowany jUPWR\ przez build.ps1).
;
; Tryby instalacji (MultiUser.nsh):
;   - uruchomiony przez zwyklego uzytkownika: instalacja "tylko dla mnie" do
;     %LocalAppData%\Programs\jUPWR, skroty i wpis "Odinstaluj" w profilu (HKCU);
;   - uruchomiony z uprawnieniami administratora: strona wyboru "dla wszystkich
;     uzytkownikow" (domyslnie; C:\Program Files\jUPWR, skroty wspolne, HKLM)
;     albo "tylko dla mnie".
; Przelaczniki wiersza polecen (wdrozenie skryptem / GPO / Intune):
;   /S            instalacja cicha (bez okien; poprzednia wersja odinstalowana automatycznie)
;   /AllUsers     wymusza tryb dla wszystkich (wymaga uprawnien administratora)
;   /CurrentUser  wymusza tryb tylko dla mnie
;   /D=C:\sciezka katalog docelowy (MUSI byc ostatni, bez cudzyslowow)
;   np.  jUPWR-1.0.4-x64-setup.exe /S /AllUsers /D=C:\jUPWR
; Deinstalator przyjmuje te same przelaczniki:  uninstall.exe /S /AllUsers

!ifndef APPNAME
  !define APPNAME  "jUPWR"                   ; /DAPPNAME=... tylko do testow instalatora
!endif
!define COMPANY    "Uniwersytet Przyrodniczy we Wroclawiu"
!ifndef VERSION
  !define VERSION  "1.0.4"                   ; wersja jUPWR (client/common/jupwr.ts)
!endif
!ifndef PAYLOAD
  !define PAYLOAD  "..\..\build\dist\jUPWR"   ; /DPAYLOAD=... do testow instalatora
!endif
!define ICON       "..\..\..\platform\app-icon.ico"

!define UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}"

; --- MultiUser: uprawnienia i tryb instalacji -------------------------------
!define MULTIUSER_EXECUTIONLEVEL Highest         ; UAC tylko na koncie z prawami admina
!define MULTIUSER_INSTALLMODE_COMMANDLINE        ; /AllUsers, /CurrentUser
!define MULTIUSER_USE_PROGRAMFILES64             ; C:\Program Files, nie (x86)
!define MULTIUSER_INSTALLMODE_INSTDIR "${APPNAME}"
; tryb poprzedniej instalacji staje sie domyslnym (i wybiera kontekst deinstalatora)
!define MULTIUSER_INSTALLMODE_DEFAULT_REGISTRY_KEY "${UNINST_KEY}"
!define MULTIUSER_INSTALLMODE_DEFAULT_REGISTRY_VALUENAME "InstallMode"
; polskie teksty strony wyboru trybu (bez pliku jezykowego)
!define MULTIUSER_INSTALLMODEPAGE_TEXT_TOP "Wybierz, dla kogo zainstalowac ${APPNAME} ${VERSION}.$\r$\n$\r$\nW pracowni komputerowej wybierz instalacje dla wszystkich uzytkownikow: program trafi do Program Files, a skroty zobaczy kazde konto."
!define MULTIUSER_INSTALLMODEPAGE_TEXT_ALLUSERS "Dla wszystkich uzytkownikow tego komputera"
!define MULTIUSER_INSTALLMODEPAGE_TEXT_CURRENTUSER "Tylko dla mnie"
!include MultiUser.nsh

Name "${APPNAME} ${VERSION}"
!ifndef OUTFILE
  !define OUTFILE  "..\..\build\dist\${APPNAME}-${VERSION}-x64-setup.exe"
!endif
OutFile "${OUTFILE}"
; bez InstallDir: katalog domyslny ustala tryb (MultiUser), a /D= go nadpisuje
SetCompressor /SOLID lzma
Unicode true
Icon "${ICON}"
UninstallIcon "${ICON}"
BrandingText "${APPNAME} ${VERSION} - ${COMPANY}"

!insertmacro MULTIUSER_PAGE_INSTALLMODE   ; pomijana automatycznie bez uprawnien admina
Page directory
Page instfiles
UninstPage uninstConfirm
UninstPage instfiles

Var CmdLineInstDir
Var ExistingUninstaller
Var ExistingVersion
Var ExistingUninstallerCopy
Var UninstallResult

; Odinstalowanie poprzedniej wersji zarejestrowanej w podanym galezi rejestru
; (HKLM = dla wszystkich, HKCU = tylko dla mnie). W trybie cichym bez pytania.
!macro UninstallPrevious ROOT ID
    ReadRegStr $ExistingUninstaller ${ROOT} "${UNINST_KEY}" "UninstallString"
    StrCmp $ExistingUninstaller "" done_${ID}
    ; nowe wpisy sa w cudzyslowach ("C:\Program Files\jUPWR\uninstall.exe"), stare (<= 1.0.4) bez
    StrCpy $0 $ExistingUninstaller 1
    StrCmp $0 '"' 0 +2
    StrCpy $ExistingUninstaller $ExistingUninstaller -1 1

    ReadRegStr $ExistingVersion ${ROOT} "${UNINST_KEY}" "DisplayVersion"
    StrCmp $ExistingVersion "" 0 haveVersion_${ID}
    StrCpy $ExistingVersion "nieznana"

haveVersion_${ID}:
    MessageBox MB_ICONQUESTION|MB_YESNO \
        "Wykryto istniejaca instalacje ${APPNAME} $ExistingVersion.$\r$\n$\r$\nOdinstalowac ja przed instalacja ${APPNAME} ${VERSION}?" \
        /SD IDYES IDYES uninstallExisting_${ID}
    Abort "Instalacja przerwana. Odinstaluj poprzednia wersje ${APPNAME} albo uruchom instalator ponownie i potwierdz odinstalowanie."

uninstallExisting_${ID}:
    IfFileExists $ExistingUninstaller 0 missingUninstaller_${ID}
    StrCpy $ExistingUninstallerCopy "$TEMP\${APPNAME}-previous-uninstall.exe"
    CopyFiles /SILENT $ExistingUninstaller $ExistingUninstallerCopy
    ; /_?= wskazuje deinstalatorowi katalog, bo pracuje z kopii w %TEMP%;
    ; stara wersja (bez MultiUser) ignoruje /AllUsers i /CurrentUser
    ${GetParent} $ExistingUninstaller $0
    ExecWait '"$ExistingUninstallerCopy" /S $1 _?=$0' $UninstallResult
    Delete $ExistingUninstallerCopy
    IntCmp $UninstallResult 0 done_${ID}
    Abort "Nie udalo sie odinstalowac poprzedniej wersji ${APPNAME}. Kod bledu: $UninstallResult"

missingUninstaller_${ID}:
    MessageBox MB_ICONEXCLAMATION|MB_OK \
        "Wykryto wpis poprzedniej instalacji ${APPNAME}, ale nie znaleziono pliku odinstalowania:$\r$\n$ExistingUninstaller$\r$\n$\r$\nInstalator bedzie kontynuowal i nadpisze istniejace pliki." \
        /SD IDOK

done_${ID}:
!macroend

Function .onInit
    ; instalator NSIS jest 32-bitowy, wiec bez tego HKLM ladowalby w Wow6432Node
    ; i 64-bitowe narzedzia (PowerShell, reguly wykrywania GPO/Intune) nie widzialyby
    ; wpisu "Odinstaluj". MultiUser.nsh sam ustawia tylko $PROGRAMFILES64, nie widok rejestru.
    SetRegView 64
    StrCpy $CmdLineInstDir $INSTDIR          ; niepusty tylko przy /D=
    !insertmacro MULTIUSER_INIT
    StrCmp $CmdLineInstDir "" +2
    StrCpy $INSTDIR $CmdLineInstDir          ; /D= ma pierwszenstwo przed katalogiem trybu

    ; poprzednia instalacja w biezacym trybie (SHCTX = HKLM albo HKCU)
    StrCmp $MultiUser.InstallMode "AllUsers" 0 perUser
    StrCpy $1 "/AllUsers"
    !insertmacro UninstallPrevious HKLM 1
    ; instalujac dla wszystkich, usun tez wlasna instalacje "tylko dla mnie",
    ; zeby nie zostaly dwa jUPWR na tym samym koncie
    StrCpy $1 "/CurrentUser"
    !insertmacro UninstallPrevious HKCU 2
    Goto initDone
perUser:
    StrCpy $1 "/CurrentUser"
    !insertmacro UninstallPrevious HKCU 3
initDone:
FunctionEnd

Function un.onInit
    SetRegView 64                            ; ten sam widok co przy instalacji
    !insertmacro MULTIUSER_UNINIT
FunctionEnd

Section "install"
    SetOutPath "$INSTDIR"
    File /r "${PAYLOAD}\*.*"
    File "/oname=app-icon.ico" "${ICON}"   ; ikona dla skrotow

    ; skroty (ikona UPWr); w trybie dla wszystkich: wspolne menu Start i pulpit publiczny
    CreateDirectory "$SMPROGRAMS\${APPNAME}"
    CreateShortcut  "$SMPROGRAMS\${APPNAME}\${APPNAME}.lnk" "$INSTDIR\bin\${APPNAME}.exe" "" "$INSTDIR\app-icon.ico"
    CreateShortcut  "$DESKTOP\${APPNAME}.lnk"               "$INSTDIR\bin\${APPNAME}.exe" "" "$INSTDIR\app-icon.ico"

    ; rejestr - wpis w "Dodaj/usun programy" (SHCTX = HKLM dla wszystkich, HKCU tylko dla mnie)
    WriteRegStr   SHCTX "${UNINST_KEY}" "DisplayName"     "${APPNAME} ${VERSION}"
    WriteRegStr   SHCTX "${UNINST_KEY}" "DisplayVersion"  "${VERSION}"
    WriteRegStr   SHCTX "${UNINST_KEY}" "Publisher"       "${COMPANY}"
    WriteRegStr   SHCTX "${UNINST_KEY}" "DisplayIcon"     "$INSTDIR\app-icon.ico"
    WriteRegStr   SHCTX "${UNINST_KEY}" "InstallLocation" "$INSTDIR"
    WriteRegStr   SHCTX "${UNINST_KEY}" "InstallMode"     "$MultiUser.InstallMode"
    WriteRegStr   SHCTX "${UNINST_KEY}" "UninstallString" '"$INSTDIR\uninstall.exe"'
    WriteRegStr   SHCTX "${UNINST_KEY}" "QuietUninstallString" '"$INSTDIR\uninstall.exe" /S'
    WriteRegDWORD SHCTX "${UNINST_KEY}" "NoModify" 1
    WriteRegDWORD SHCTX "${UNINST_KEY}" "NoRepair" 1

    ; skojarzenie .omv (opcjonalne) - odkomentuj, jesli potrzebne
    ; WriteRegStr SHCTX "Software\Classes\.omv" "" "jUPWR.Dataset"

    WriteUninstaller "$INSTDIR\uninstall.exe"
SectionEnd

Section "uninstall"
    Delete "$SMPROGRAMS\${APPNAME}\${APPNAME}.lnk"
    RMDir  "$SMPROGRAMS\${APPNAME}"
    Delete "$DESKTOP\${APPNAME}.lnk"
    DeleteRegKey SHCTX "${UNINST_KEY}"
    RMDir /r "$INSTDIR"
SectionEnd
