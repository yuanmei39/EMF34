@echo off

echo	====================================================
echo	Recursive-Dynamic Model for EMF 34 Trilateral Study
echo	.
echo	Massachusetts Institute of Technology
echo	.
echo	http://globalchange.mit.edu/
echo	====================================================

: Mei Yuan
: September 2017

set workbook=report_EMF34

set lic=gamslice_JP
set curdir=%~dp0
set ds=emf34

set bmkyr=2011
set fstyr=2015
set tint=5

set year=2011
set endyr=2011

if not exist restart\null	mkdir restart\
if not exist results\gdx\	mkdir results\gdx\
if not exist lst\		mkdir lst\

for %%f in (a b c d e f g h i j k l m n o p q r s t u v w x y z) do rd ^
	core\225%%f /s /q 
del log.txt

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:SCENARIOS   
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

call :LOOP bau Ref
::call :LOOP scn gdp_tri Ref
::call :LOOP scn gdp_usa Ref
::call :LOOP scn gdp_can Ref
::call :LOOP scn gdp_mex Ref
::goto merge
goto end


::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:LOOP
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set tag=%1	
set case=%2
set bau=%3

if %year% equ %bmkyr% set loadyr=%bmkyr%&set loadfile=core
if %year% equ %fstyr% set loadyr=%bmkyr%&set loadfile=loop_%case%_%loadyr%
if %year% gtr %fstyr% set /a loadyr=%year%-%tint%&set loadfile=loop_%case%_%loadyr%

TITLE Solving Model: year_%year% 
echo EMF34 year=%year% loadyr=%loadyr% loadfile=%loadfile% >> log.txt

cd core
if %year% gtr %bmkyr% goto loop

gams core --year=%year% --ds=%ds% --tint=%tint% s=..\restart\core ^
	  o=..\lst\core_%year%.lst ps=0

goto endcore

:: Do we need to run loop.gms over bmkyr?
:loop

gams loop --year=%year% --tag=%tag% --case=%case% --bau=%bau% --loadyr=%loadyr% ^
	  r=..\restart\%loadfile% s=..\restart\loop_%case%_%year% ^
	  o=..\lst\loop_%case%_%year%.lst license=%lic%.txt sysout=1 ps=0

:endcore
cd ..
if errorlevel 1 goto errloop 

if %year% equ %bmkyr% (set year=%fstyr%) else (set /a year+=%tint%)
if %year% leq %endyr% (goto LOOP) else (goto end)


::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:merge
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

cd ..\results
gams ..\core\report r=..\restart\core --workbook=%workbook% ps=0 fw=1
goto end


:end

