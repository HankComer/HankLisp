@echo off
rm Main.exe
robocopy . scratch
cd scratch
ghc Main.hs
move Main.exe ..
rm *.*
cd ..
rmdir scratch
@echo on