$ErrorActionPreference = "Stop"

$LWJGL_VERSION = "3.4.1"
$ScriptPath = $MyInvocation.MyCommand.Path
$ScriptDir = Split-Path -Parent $ScriptPath
$CurrentDir = (Get-Location).Path

function Add-UniqueRoot([System.Collections.Generic.List[string]]$List, [string]$PathValue) {
  if ([string]::IsNullOrWhiteSpace($PathValue)) { return }
  $Full = [System.IO.Path]::GetFullPath($PathValue)
  if (-not $List.Contains($Full)) { [void]$List.Add($Full) }
}

function Resolve-BlockboxRootAndSource {
  $roots = New-Object System.Collections.Generic.List[string]
  Add-UniqueRoot $roots $ScriptDir
  Add-UniqueRoot $roots (Split-Path -Parent $ScriptDir)
  Add-UniqueRoot $roots $CurrentDir
  Add-UniqueRoot $roots (Split-Path -Parent $CurrentDir)

  # Normal project layout first. This is the real target: src\main\scala\blockbox\Main.scala
  foreach ($root in $roots) {
    $projectMain = Join-Path $root "src\main\scala\blockbox\Main.scala"
    if (Test-Path $projectMain) {
      return @{ Root = $root; Source = $projectMain }
    }
  }

  # Also support a simple single-file folder.
  foreach ($root in $roots) {
    $rootMain = Join-Path $root "Main.scala"
    if (Test-Path $rootMain) {
      return @{ Root = $root; Source = $rootMain }
    }
  }

  # Last fallback for older packages, but not required.
  foreach ($root in $roots) {
    $anyUnique = Get-ChildItem -Path $root -Filter "Blockbox_Main_*_UNIQUE.scala" -File -ErrorAction SilentlyContinue |
      Sort-Object LastWriteTime -Descending |
      Select-Object -First 1
    if ($null -ne $anyUnique) {
      return @{ Root = $root; Source = $anyUnique.FullName }
    }
  }

  throw @"
Could not find Blockbox Scala source.

Expected one of:
  src\main\scala\blockbox\Main.scala
  Main.scala
  Blockbox_Main_*_UNIQUE.scala

Checked:
$($roots -join "`n")
"@
}

$Resolved = Resolve-BlockboxRootAndSource
$ProjectRoot = $Resolved.Root
$SourceFile = $Resolved.Source
Set-Location $ProjectRoot

# Run from a clean mirror so scala-cli cannot accidentally compile old extra files nearby.
$RunDir = Join-Path $ProjectRoot ".blockbox-run"
if (Test-Path $RunDir) { Remove-Item -Recurse -Force $RunDir }
New-Item -ItemType Directory -Force -Path $RunDir | Out-Null
$RunFile = Join-Path $RunDir "Main.scala"
Copy-Item -Force $SourceFile $RunFile

$LogFile = Join-Path $ProjectRoot "blockbox-last-run.log"

Write-Host "Blockbox: project root = $ProjectRoot"
Write-Host "Blockbox: source       = $SourceFile"
Write-Host "Blockbox: run mirror   = $RunFile"
Write-Host "Blockbox: log          = $LogFile"

function Test-Command($Name) {
  return [bool](Get-Command $Name -ErrorAction SilentlyContinue)
}

function Refresh-Path {
  $machine = [System.Environment]::GetEnvironmentVariable("Path", "Machine")
  $user = [System.Environment]::GetEnvironmentVariable("Path", "User")
  $env:Path = "$machine;$user"
}

function Install-WithWinget($Ids, $FriendlyName) {
  if (-not (Test-Command winget)) {
    throw "$FriendlyName is missing and winget is not available. Install $FriendlyName manually, then rerun this script."
  }

  foreach ($id in $Ids) {
    Write-Host "Blockbox: trying winget install $FriendlyName ($id)..."
    winget install --id $id --exact --source winget --accept-package-agreements --accept-source-agreements
    Refresh-Path
    if ($LASTEXITCODE -eq 0) { return }
  }

  throw "Could not install $FriendlyName with winget. Install it manually, then rerun this script."
}

if (-not (Test-Command java)) {
  Install-WithWinget @("EclipseAdoptium.Temurin.21.JDK") "Temurin JDK 21"
}

if (-not (Test-Command scala-cli)) {
  Install-WithWinget @("VirtusLab.ScalaCLI", "Scala.ScalaCli") "Scala CLI"
}

if (-not (Test-Command java)) { throw "java is still not on PATH after install. Open a new PowerShell window and rerun this script." }
if (-not (Test-Command scala-cli)) { throw "scala-cli is still not on PATH after install. Open a new PowerShell window and rerun this script." }

$Deps = @(
  "org.lwjgl:lwjgl:$LWJGL_VERSION",
  "org.lwjgl:lwjgl-glfw:$LWJGL_VERSION",
  "org.lwjgl:lwjgl-opengl:$LWJGL_VERSION",
  "org.lwjgl:lwjgl-stb:$LWJGL_VERSION",
  "org.lwjgl:lwjgl:$LWJGL_VERSION,classifier=natives-windows",
  "org.lwjgl:lwjgl-glfw:$LWJGL_VERSION,classifier=natives-windows",
  "org.lwjgl:lwjgl-opengl:$LWJGL_VERSION,classifier=natives-windows",
  "org.lwjgl:lwjgl-stb:$LWJGL_VERSION,classifier=natives-windows"
)

$ArgsList = @("run", $RunFile, "--server=false", "--java-opt", "-Xmx4G", "--java-opt", "--enable-native-access=ALL-UNNAMED")
foreach ($dep in $Deps) {
  $ArgsList += @("--dependency", $dep)
}

Write-Host "Blockbox: running..."
"Blockbox command: scala-cli $($ArgsList -join ' ')" | Out-File -FilePath $LogFile -Encoding utf8
& scala-cli @ArgsList 2>&1 | Tee-Object -FilePath $LogFile -Append
$Exit = $LASTEXITCODE

if ($Exit -ne 0) {
  Write-Host ""
  Write-Host "Blockbox failed. Full log saved to:"
  Write-Host "  $LogFile"
  Write-Host ""
  Write-Host "Send blockbox-last-run.log, especially the FIRST [error] lines."
  throw "scala-cli exited with code $Exit"
}
