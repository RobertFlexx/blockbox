$ErrorActionPreference = "Stop"

$LWJGL_VERSION = "3.4.1"
$GROOVY_VERSION = "5.0.6"
$LWJGL_NATIVE_CLASSIFIER = switch ($env:PROCESSOR_ARCHITECTURE) {
  "ARM64" { "natives-windows-arm64"; break }
  "x86" { "natives-windows-x86"; break }
  default { "natives-windows"; break }
}
$LWJGL_MODULES = @("lwjgl", "lwjgl-glfw", "lwjgl-opengl", "lwjgl-stb")
$ScriptPath = $MyInvocation.MyCommand.Path
$ScriptDir = Split-Path -Parent $ScriptPath
$CurrentDir = (Get-Location).Path
$LaunchDir = $CurrentDir

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
$env:BLOCKBOX_PROJECT_ROOT = $ProjectRoot
if ([string]::IsNullOrWhiteSpace($env:BLOCKBOX_INSTANCE)) {
  Set-Location $ProjectRoot
} else {
  Set-Location $LaunchDir
}

# Run from a clean mirror so scala-cli cannot accidentally compile old extra files nearby.
$RunDir = Join-Path $ProjectRoot ".blockbox-run"
if (Test-Path $RunDir) { Remove-Item -Recurse -Force $RunDir }
New-Item -ItemType Directory -Force -Path $RunDir | Out-Null
$ProjectMain = Join-Path $ProjectRoot "src\main\scala\blockbox\Main.scala"
if ([System.IO.Path]::GetFullPath($SourceFile) -eq [System.IO.Path]::GetFullPath($ProjectMain)) {
  Copy-Item -Recurse -Force (Join-Path $ProjectRoot "src") (Join-Path $RunDir "src")
  $RunTarget = Join-Path $RunDir "src\main"
  $RunFile = Join-Path $RunDir "src\main\scala\blockbox\Main.scala"
} else {
  $RunFile = Join-Path $RunDir "Main.scala"
  Copy-Item -Force $SourceFile $RunFile
  $RunTarget = $RunFile
}

$LogFile = Join-Path $ProjectRoot "blockbox-last-run.log"

Write-Host "Blockbox: project root = $ProjectRoot"
Write-Host "Blockbox: source       = $SourceFile"
Write-Host "Blockbox: run mirror   = $RunTarget"
Write-Host "Blockbox: log          = $LogFile"

function Test-Command($Name) {
  return [bool](Get-Command $Name -ErrorAction SilentlyContinue)
}

function Refresh-Path {
  $machine = [System.Environment]::GetEnvironmentVariable("Path", "Machine")
  $user = [System.Environment]::GetEnvironmentVariable("Path", "User")
  $process = [System.Environment]::GetEnvironmentVariable("Path", "Process")
  $env:Path = @($process, $machine, $user) -join ";"
  Remove-EmptyPathSegments "Path"
}

function Remove-EmptyPathSegments([string]$Name) {
  $value = [System.Environment]::GetEnvironmentVariable($Name, "Process")
  if ([string]::IsNullOrEmpty($value)) { return }
  $clean = (($value -split ";") | Where-Object { -not [string]::IsNullOrWhiteSpace($_) }) -join ";"
  [System.Environment]::SetEnvironmentVariable($Name, $clean, "Process")
}

function Quote-CmdArg([string]$Arg) {
  if ($null -eq $Arg) { return '""' }
  if ($Arg -notmatch '[\s"&()<>^|]') { return $Arg }
  return '"' + ($Arg -replace '"', '\"') + '"'
}

function Invoke-NativeLogged([string]$FileName, [string[]]$Arguments, [string]$OutputLog) {
  $oldPreference = $ErrorActionPreference
  try {
    # Windows PowerShell turns native stderr records into NativeCommandError when ErrorActionPreference is Stop.
    $ErrorActionPreference = "Continue"
    & $FileName @Arguments 2>&1 | Tee-Object -FilePath $OutputLog -Append | ForEach-Object { Write-Host $_ }
    $exitCode = $LASTEXITCODE
    return $exitCode
  } finally {
    $ErrorActionPreference = $oldPreference
  }
}

function Get-LwjglNativeJars([string]$NativeDir) {
  New-Item -ItemType Directory -Force -Path $NativeDir | Out-Null
  $jars = New-Object System.Collections.Generic.List[string]

  foreach ($module in $LWJGL_MODULES) {
    $fileName = "$module-$LWJGL_VERSION-$LWJGL_NATIVE_CLASSIFIER.jar"
    $target = Join-Path $NativeDir $fileName
    if (-not (Test-Path $target)) {
      $url = "https://repo1.maven.org/maven2/org/lwjgl/$module/$LWJGL_VERSION/$fileName"
      Write-Host "Blockbox: downloading $fileName..."
      Invoke-WebRequest -UseBasicParsing -Uri $url -OutFile $target
    }
    [void]$jars.Add($target)
  }

  return $jars.ToArray()
}

function Compile-JavaSources([string]$SourceRoot, [string]$ClassesDir) {
  $javaRoot = Join-Path $SourceRoot "main\java"
  if (-not (Test-Path $javaRoot)) { return $null }
  $sources = @(Get-ChildItem -Path $javaRoot -Filter "*.java" -Recurse -File -ErrorAction SilentlyContinue | ForEach-Object { $_.FullName })
  if ($sources.Length -eq 0) { return $null }
  if (-not (Test-Command javac)) { throw "javac is missing. Install JDK 21, not just a JRE, then rerun this script." }
  New-Item -ItemType Directory -Force -Path $ClassesDir | Out-Null
  Write-Host "Blockbox: compiling Java networking helpers..."
  & javac --release 21 -encoding UTF-8 -d $ClassesDir @sources
  if ($LASTEXITCODE -ne 0) { throw "javac failed with code $LASTEXITCODE" }
  return $ClassesDir
}

function Show-ScalaCliStacktraces([string]$Text) {
  $matches = [regex]::Matches($Text, "'([^']*\\.scala-build\\stacktraces\\[^']+\.log)'")
  foreach ($match in $matches) {
    $path = $match.Groups[1].Value
    if (Test-Path $path) {
      Write-Host ""
      Write-Host "Scala CLI stacktrace: $path"
      Write-Host ""
      Get-Content -Path $path -Tail 120 -ErrorAction SilentlyContinue | ForEach-Object { Write-Host $_ }
    }
  }
}

function Get-JavaMajorVersion {
  if (-not (Test-Command java)) { return 0 }
  $output = & cmd.exe /d /c "java -version 2>&1"
  foreach ($line in $output) {
    if ($line -match 'version "([0-9]+)(?:\.([0-9]+))?') {
      $major = [int]$matches[1]
      if ($major -eq 1 -and $matches[2]) { return [int]$matches[2] }
      return $major
    }
  }
  return 0
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

Remove-EmptyPathSegments "Path"
Remove-EmptyPathSegments "CLASSPATH"

if ((Get-JavaMajorVersion) -lt 21) {
  Install-WithWinget @("EclipseAdoptium.Temurin.21.JDK") "Temurin JDK 21"
}

if (-not (Test-Command scala-cli)) {
  Install-WithWinget @("VirtusLab.ScalaCLI", "Scala.ScalaCli") "Scala CLI"
}

if (-not (Test-Command java)) { throw "java is still not on PATH after install. Open a new PowerShell window and rerun this script." }
if ((Get-JavaMajorVersion) -lt 21) { throw "Blockbox needs Java 21 or newer. Install Temurin JDK 21 and make sure it is first on PATH, then rerun this script." }
if (-not (Test-Command scala-cli)) { throw "scala-cli is still not on PATH after install. Open a new PowerShell window and rerun this script." }

$Deps = @(
  "org.lwjgl:lwjgl:$LWJGL_VERSION",
  "org.lwjgl:lwjgl-glfw:$LWJGL_VERSION",
  "org.lwjgl:lwjgl-opengl:$LWJGL_VERSION",
  "org.lwjgl:lwjgl-stb:$LWJGL_VERSION",
  "org.apache.groovy:groovy:$GROOVY_VERSION"
)

$NativeJars = Get-LwjglNativeJars (Join-Path $RunDir "lwjgl-natives")
$JavaClasses = Compile-JavaSources (Join-Path $RunDir "src") (Join-Path $RunDir "java-classes")

$ArgsList = @("run", $RunTarget, "--server=false")
if ((-not [string]::IsNullOrWhiteSpace($env:BLOCKBOX_JVM_ARGS_FILE)) -and (Test-Path $env:BLOCKBOX_JVM_ARGS_FILE)) {
  foreach ($jvmArg in (Get-Content -Path $env:BLOCKBOX_JVM_ARGS_FILE -ErrorAction SilentlyContinue)) {
    if (-not [string]::IsNullOrWhiteSpace($jvmArg)) {
      $ArgsList += @("--java-opt", $jvmArg)
    }
  }
} else {
  $JvmArgText = if ([string]::IsNullOrWhiteSpace($env:BLOCKBOX_JVM_ARGS)) { "-Xmx4G" } else { $env:BLOCKBOX_JVM_ARGS }
  foreach ($jvmArg in ($JvmArgText -split '\s+')) {
    if (-not [string]::IsNullOrWhiteSpace($jvmArg)) {
      $ArgsList += @("--java-opt", $jvmArg)
    }
  }
}
$ArgsList += @("--java-opt", "--enable-native-access=ALL-UNNAMED")
foreach ($dep in $Deps) {
  $ArgsList += "--dependency=$dep"
}
if ($JavaClasses -ne $null) {
  $ArgsList += @("--extra-jar", $JavaClasses)
}
foreach ($jar in $NativeJars) {
  $ArgsList += @("--extra-jar", $jar)
}
if ((-not [string]::IsNullOrWhiteSpace($env:BLOCKBOX_GAME_ARGS_FILE)) -and (Test-Path $env:BLOCKBOX_GAME_ARGS_FILE)) {
  $GameArgs = @(Get-Content -Path $env:BLOCKBOX_GAME_ARGS_FILE -ErrorAction SilentlyContinue | Where-Object { -not [string]::IsNullOrWhiteSpace($_) })
  if ($GameArgs.Length -gt 0) {
    $ArgsList += "--"
    foreach ($gameArg in $GameArgs) {
      $ArgsList += $gameArg
    }
  }
} elseif (-not [string]::IsNullOrWhiteSpace($env:BLOCKBOX_GAME_ARGS)) {
  $ArgsList += "--"
  foreach ($gameArg in ($env:BLOCKBOX_GAME_ARGS -split '\s+')) {
    if (-not [string]::IsNullOrWhiteSpace($gameArg)) {
      $ArgsList += $gameArg
    }
  }
}
Write-Host "Blockbox: running..."
"Blockbox command: scala-cli $($ArgsList -join ' ')" | Out-File -FilePath $LogFile -Encoding utf8
$Exit = Invoke-NativeLogged "scala-cli" $ArgsList $LogFile

if ($Exit -ne 0) {
  Write-Host ""
  Write-Host "Blockbox failed. Full log saved to:"
  Write-Host "  $LogFile"
  Write-Host ""
  Write-Host "Last log lines:"
  Write-Host ""
  $lastLog = @(Get-Content -Path $LogFile -Tail 80 -ErrorAction SilentlyContinue)
  $lastLog | ForEach-Object { Write-Host $_ }
  Show-ScalaCliStacktraces ($lastLog -join "`n")
  Write-Host ""
  Write-Host "Send blockbox-last-run.log, especially the FIRST [error] lines above."
  Write-Host "scala-cli exited with code $Exit"
  exit $Exit
}

exit 0
