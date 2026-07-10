[CmdletBinding()]
param(
    [switch]$NoDefaults,
    [switch]$DaemonOnly,
    [string]$EmacsClientPath,
    [string]$RunEmacsPath,
    [string[]]$Extensions = @(
        '.txt', '.md', '.org', '.el', '.py', '.js', '.ts', '.tsx', '.jsx', '.json', '.yaml', '.yml',
        '.xml', '.html', '.css', '.sh', '.bash', '.zsh', '.ps1', '.bat', '.cmd', '.c', '.h',
        '.cpp', '.hpp', '.rs', '.go', '.lua', '.sql', '.toml', '.ini', '.conf'
    )
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Find-Executable {
    param(
        [Parameter(Mandatory=$true)][string]$Name,
        [string[]]$ExtraCandidates = @()
    )

    if ($ExtraCandidates) {
        foreach ($candidate in $ExtraCandidates) {
            if ($candidate -and (Test-Path -LiteralPath $candidate)) {
                return (Resolve-Path -LiteralPath $candidate).Path
            }
        }
    }

    $cmd = Get-Command $Name -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Source }

    $roots = @(
        $env:ProgramFiles,
        ${env:ProgramFiles(x86)},
        $env:LOCALAPPDATA,
        $env:APPDATA
    ) | Where-Object { $_ }

    foreach ($root in $roots) {
        $matches = Get-ChildItem -LiteralPath $root -Filter $Name -Recurse -File -ErrorAction SilentlyContinue | Select-Object -First 1
        if ($matches) { return $matches.FullName }
    }

    return $null
}

function Quote-CommandPath {
    param([Parameter(Mandatory=$true)][string]$Path)
    '"{0}"' -f ($Path -replace '"', '\"')
}

$emacsClient = if ($EmacsClientPath) { (Resolve-Path -LiteralPath $EmacsClientPath).Path } else { Find-Executable 'emacsclientw.exe' }
$runEmacs = if ($RunEmacsPath) { (Resolve-Path -LiteralPath $RunEmacsPath).Path } else { Find-Executable 'runemacs.exe' }

if (-not $emacsClient) { throw 'Could not find emacsclientw.exe. Pass -EmacsClientPath or add it to PATH.' }
if (-not $runEmacs) { throw 'Could not find runemacs.exe. Pass -RunEmacsPath or add it to PATH.' }

$progId = 'EmacsClientFile'
$appKey = 'HKCU:\Software\Classes\Applications\emacsclientw.exe'
$progIdKey = "HKCU:\Software\Classes\$progId"
$command = ('{0} -c -n -a {1} "%1"' -f (Quote-CommandPath $emacsClient), (Quote-CommandPath $runEmacs))

function Ensure-Key {
    param([Parameter(Mandatory=$true)][string]$Path)
    if (-not (Test-Path -LiteralPath $Path)) { New-Item -Path $Path -Force | Out-Null }
}

function Install-Associations {
    Ensure-Key $appKey
    Ensure-Key "$appKey\shell\open\command"
    New-ItemProperty -LiteralPath $appKey -Name 'FriendlyAppName' -Value 'Emacs Client' -PropertyType String -Force | Out-Null
    Set-Item -LiteralPath "$appKey\shell\open\command" -Value $command

    Ensure-Key $progIdKey
    Set-Item -LiteralPath $progIdKey -Value 'Emacs Client File'
    Ensure-Key "$progIdKey\shell\open\command"
    Set-Item -LiteralPath "$progIdKey\shell\open\command" -Value $command

    foreach ($ext in $Extensions) {
        if (-not $ext.StartsWith('.')) { $ext = ".$ext" }

        Ensure-Key "$appKey\SupportedTypes"
        New-ItemProperty -LiteralPath "$appKey\SupportedTypes" -Name $ext -Value '' -PropertyType String -Force | Out-Null

        $extKey = "HKCU:\Software\Classes\$ext"
        Ensure-Key $extKey
        Ensure-Key "$extKey\OpenWithProgids"
        New-ItemProperty -LiteralPath "$extKey\OpenWithProgids" -Name $progId -Value ([byte[]]@()) -PropertyType Binary -Force | Out-Null

        if (-not $NoDefaults) {
            Set-Item -LiteralPath $extKey -Value $progId
        }
    }

    Write-Host "Registered Emacs Client for $($Extensions.Count) extensions."
    if ($NoDefaults) {
        Write-Host 'Skipped changing per-user default ProgIDs (-NoDefaults).'
    } else {
        Write-Host 'Set per-user default ProgID where Windows allows it. Existing UserChoice defaults may still take precedence.'
    }
}

function Install-DaemonStartup {
    $taskName = 'EmacsDaemon'
    $action = New-ScheduledTaskAction -Execute $runEmacs -Argument '--daemon'
    $trigger = New-ScheduledTaskTrigger -AtLogOn
    $principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Interactive

    try {
        Register-ScheduledTask -TaskName $taskName -Action $action -Trigger $trigger -Principal $principal -Description 'Start Emacs daemon at user logon' -Force | Out-Null
        Write-Host "Registered scheduled task: $taskName"
    } catch {
        $startup = [Environment]::GetFolderPath('Startup')
        $shortcutPath = Join-Path $startup 'Emacs Daemon.lnk'
        $shell = New-Object -ComObject WScript.Shell
        $shortcut = $shell.CreateShortcut($shortcutPath)
        $shortcut.TargetPath = $runEmacs
        $shortcut.Arguments = '--daemon'
        $shortcut.WorkingDirectory = Split-Path -Parent $runEmacs
        $shortcut.Save()
        Write-Host "Scheduled task failed; created Startup shortcut: $shortcutPath"
    }
}

if (-not $DaemonOnly) {
    Install-Associations
}
Install-DaemonStartup

Write-Host ''
Write-Host 'Verification:'
Write-Host ('  {0} -c -n -a {1} README.org' -f (Quote-CommandPath $emacsClient), (Quote-CommandPath $runEmacs))
