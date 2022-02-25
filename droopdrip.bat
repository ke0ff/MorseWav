echo off
if "%1"=="?" goto HELP
rem tone, HZ
set WAV_TONE=450
if not "%1"=="" set WAV_TONE=%1
rem WPM
set WAV_WPM=20
set WAV_RAMP=30
echo on
del cabin_001.wav
ct.pl --p droopdrip.txt --w %WAV_WPM% --d 10 --to %WAV_TONE% --ram %WAV_RAMP% --ca ke0ff --ta 0
echo off
goto END
:HELP
rem ECHO Useage: ringtones <tone,hz>'
rem ECHO 'Processes text files to generate .wav CW/tone output'
:END
rem free up variables
set WAV_TONE=
set WAV_WPM=
set WAV_RAMP=
