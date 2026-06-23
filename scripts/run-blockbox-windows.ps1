$ErrorActionPreference = "Stop"

$LWJGL_VERSION = "3.4.1"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$SourceFile = Join-Path $ScriptDir "Blockbox_Main_CrossPlatformSandPause_v25_UNIQUE.scala"

if (-not (Test-Path $SourceFile)) {
  $ProjectSource = Join-Path $ScriptDir "src\main\scala\blockbox\Main.scala"
  if (Test-Path $ProjectSource) {
    $SourceFile = $ProjectSource
  } else {
    throw "Could not find Blockbox Scala source next to this script or at src\main\scala\blockbox\Main.scala."
  }
}

function Test-Command($Name) {
  return [bool](Get-Command $Name -ErrorAction SilentlyContinue)
}

function Refresh-Path {
  $machine = [System.Environment]::GetEnvironmentVariable("Path", "Machine")
  $user = [System.Environment]::GetEnvironmentVariable("Path", "User")
  $env:Path = "$machine;$user"
}

function Install-WithWinget($Id, $FriendlyName) {
  if (-not (Test-Command winget)) {
    throw "$FriendlyName is missing and winget is not available. Install $FriendlyName manually, then rerun this script."
  }
  Write-Host "Blockbox: installing $FriendlyName with winget..."
  winget install --id $Id --exact --source winget --accept-package-agreements --accept-source-agreements
  Refresh-Path
}

if (-not (Test-Command java)) {
  Install-WithWinget "EclipseAdoptium.Temurin.21.JDK" "Temurin JDK 21"
}

if (-not (Test-Command scala-cli)) {
  Install-WithWinget "VirtusLab.ScalaCLI" "Scala CLI"
}

if (-not (Test-Command java)) { throw "java is still not on PATH after install. Open a new PowerShell window and rerun this script." }
if (-not (Test-Command scala-cli)) { throw "scala-cli is still not on PATH after install. Open a new PowerShell window and rerun this script." }

$NativeAccessOpt = @()
& java --enable-native-access=ALL-UNNAMED -version *> $null
if ($LASTEXITCODE -eq 0) {
  $NativeAccessOpt = @("--java-opt", "--enable-native-access=ALL-UNNAMED")
}

$Deps = @(
  "org.lwjgl:lwjgl:$LWJGL_VERSION,classifier=natives-windows",
  "org.lwjgl:lwjgl-glfw:$LWJGL_VERSION,classifier=natives-windows",
  "org.lwjgl:lwjgl-opengl:$LWJGL_VERSION,classifier=natives-windows",
  "org.lwjgl:lwjgl-stb:$LWJGL_VERSION,classifier=natives-windows"
)

$ArgsList = @("run", $SourceFile, "--java-opt", "-Xmx4G") + $NativeAccessOpt
foreach ($dep in $Deps) {
  $ArgsList += @("--dependency", $dep)
}

Write-Host "Blockbox: running $SourceFile"
& scala-cli @ArgsList
if ($LASTEXITCODE -ne 0) {
  throw "scala-cli exited with code $LASTEXITCODE"
}
