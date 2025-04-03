@echo off
chcp 65001
:start
echo 1 - next1
echo 2 - next2

set /p var="Выберите действие: ":
if "%var%" == "2" goto next2
if "%var%" == "1" goto next1

:next1
echo 1 - exit
echo 2 - start
set /p var="Выберите действие: ":
if "%var%" == "2" goto start
if "%var%" == "1" goto exit
:exit

:next2
echo 1 - exit
echo 2 - next3
set /p var="Выберите действие: ":
if "%var%" == "2" goto next3
if "%var%" == "1" goto exit
:exit

:next3
echo 1 - exit
echo 2 - start
echo 3 - next7

set /p var="Выберите действие: ":
if "%var%" == "3" goto start 
if "%var%" == "2" goto start
if "%var%" == "1" goto exit
:exit