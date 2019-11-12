  #define MyAppName "DoE"
#define MyAppVersion "1.4"
#define MyAppExeName "DoE.vbs"
#define MyAppPublisher ""
#define MyAppURL ""

[Setup]
AppName = {#MyAppName}
AppId = {{BFKZHHOU-0DJD-HJU6-BOGC-CT3Z7Q46G493}
DefaultDirName = {sd}\{#MyAppName}
DefaultGroupName = {#MyAppName}
OutputDir = SetUp
OutputBaseFilename = setup_{#MyAppName}
SetupIconFile = doe.ico
AppVersion = 1.4
AppPublisher = {#MyAppPublisher}
AppPublisherURL = {#MyAppURL}
AppSupportURL = {#MyAppURL}
AppUpdatesURL = {#MyAppURL}
PrivilegesRequired = none
InfoBeforeFile = infobefore.txt
InfoAfterFile = infoafter.txt
Compression = lzma2/ultra64
SolidCompression = yes
ArchitecturesInstallIn64BitMode = x64
LicenseFile = gpl_3.0.txt

[Languages]
Name: "italian"; MessagesFile: "compiler:Languages\Italian.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\doe.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\doe.ico"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon; IconFilename: "{app}\doe.ico"

[Files]

Source: "C:\Users\Camillo\GitHub\DoE\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

; NOTE: Don't use "Flags: ignoreversion" on any shared system files





