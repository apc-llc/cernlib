# Some nmake macros for Win32 apps development

# !include <f77.mak>
!include <ntwin32.mak>

PROJ = hplexam

# application specific C defines

#  Fortran compilers specifics:

.SUFFIXES: .f

#for = f2c

!IF "$(FOR)" != "f77"
for = f2c
FFLAGS = -w -Nn802 -Nx802 -NL802
flib   = f2c.lib
!ELSE
for = $(f77)
FFLAGS = $(fflags) -align:dcommon -align:records -debug:full
flib   = $(f77libs)
!ENDIF

# Rules to constract obj either under f2c or under "native" FORTRAN

.f.obj:
!IF "$(for)" == "f2c"
    $(for)  $(fflags) $*.f
    $(cc)  $(cflags) $(cvarsdll) -O2  $*.c >> $*_c.log
!ELSE
    $(for) $(FFLAGS) $*.f /nolink /object:$@      >>$*_f.log
!ENDIF

.c.obj:
	$(cc) -c -O2 $(cflags) $(cvarsdll)  /G5  $*.c >$*.log



cf = -DNT -DWIN -DNOT_IMPLEMENTED -D_MT -D_PORTABLE_32BIT_CONTEXT

# This line allows NMAKE to work as well

all: $(PROJ).exe

IX_HIGZ.obj: IX_HIGZ.c IX_HIGZ.h

IW_TEXT.obj: IW_TEXT.c IX_HIGZ.h IW_TEXT.H

# Update the executable file if necessary, and if so, add the resource back in.
IX_HIGZ.exp: IX_HIGZ.def IX_HIGZ.obj IW_TEXT.obj
  $(implib) /out:$*.lib $*.obj  IW_TEXT.obj /def:$*.def

IX_HIGZ.dll: IX_HIGZ.obj IW_TEXT.obj IX_HIGZ.exp
        $(link) /out:$*.dll $(dlllflags)  \
                $*.obj IW_TEXT.obj $*.exp \
                $(guilibsdll)

$(PROJ).exe: $(PROJ).obj
        $(link) $(linkdebug) $(conflags) -out:$(PROJ).exe   \
        $(PROJ).obj   \
        ix_higz.lib \
        \misf58\cern\lib\hplot.lib \misf58\cern\lib\higz.lib \
        \misf58\cern\lib\94a\hbook.lib \
        \misf58\cern\lib\zebra.lib  \misf58\cern\lib\mathlib.lib \
        \misf58\cern\lib\kernlib.lib \
        $(flib) $(guilibs)
