:: Windows only, but it should be clear what's being done here.
ehc -i../src && ghc --odir=../dist -i../src -o examplesGHC.exe --make examples || GOTO EXIT
examplesGHC.exe > outGHC
examples.exe > outEHC
diff outGHC outEHC -b

:EXIT