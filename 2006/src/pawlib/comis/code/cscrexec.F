*
* $Id: cscrexec.F,v 1.7 2006/09/15 09:35:36 mclareni Exp $
*
* $Log: cscrexec.F,v $
* Revision 1.7  2006/09/15 09:35:36  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.6  2000/09/25 14:16:36  couet
* The SGI systems have a minor inconsistency in that both f77 and cc
* default to -n32 but ld does not. As a result, attempts to build a
* shared library file from a Comis function fail.
* The needed changes are:
*
*                 ld -shared -o ...
* to
*                 ld -n32 -shared -o ...
*
* Revision 1.5  1998/01/12 09:55:41  couet
* - Mods for LINUX (shared libraries)
*
* Revision 1.4  1996/12/09 12:03:14  berejnoi
* For AIX added ld option:
*        -e _nostart  for AIX 3
*        -bnoenrty    for AIX 4
*
* Revision 1.3  1996/12/03 16:36:08  berejnoi
* Mods in csnumb:more then 30 digits in a number
* Mods in cscrexec: for AIX -berok added to solve problem
* with incremental dyn. loading
*
* Revision 1.2  1996/05/13 08:04:58  berejnoi
* Mods for SHL version on IBMRT
*
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_SHL)
*CMZ :  1.19/00 07/04/95  14.29.44  by  Unknown
*-- Author :    Vladimir Berezhnoi   07/01/94
      SUBROUTINE CSCREXEC(NAME,CPID,FEXT,FEXEC,IERR)
      CHARACTER *(*)      NAME,CPID,FEXT,FEXEC
#include "comis/cshlnm.inc"
      CHARACTER *256 CHLINE
#if defined(CERNLIB_SGI)
      CHARACTER *2048 USERLIBS
#endif
#if defined(CERNLIB_IBMRT)
      CHARACTER *256 IMPORT, PROG, PPATH, PNAME, PEXT
      INTEGER LIMPORT, LPROG
      SAVE IMPORT, PROG, LIMPORT, LPROG
      DATA LIMPORT/0/
      IF(LIMPORT.EQ.0)THEN
        CALL GETARG(0,PROG)
        LPROG=LENOCC(PROG)
        CALL CSPARSFN(PROG,PPATH,PNAME,PEXT)
        IMPORT=PNAME(:LENOCC(PNAME)) // '_Import.o'
        LIMPORT=LENOCC(IMPORT)
        IF(LENOCC(PPATH).EQ.0)THEN
          PROG='$olddir/' // PROG(:LPROG)
          LPROG=LENOCC(PROG)
        ENDIF
      ENDIF
#endif
      IERR=0
**      CALL PALUNF(60,3,LUNOUT)
      CALL CSLUNF(LUNOUT)
      IF(LUNOUT.EQ.0)GO TO 99
      LN=LENOCC(NAME)
      LP=LENOCC(CPID)
      LPATH=LENOCC(CHPATH)
      LEXEC=LENOCC(FEXEC)
      OPEN(LUNOUT,FILE=FEXEC(:LEXEC), STATUS='UNKNOWN',ERR=99)
      WRITE(LUNOUT,'(A)')         '#!/bin/sh'
      WRITE(LUNOUT,'(A)')         'olddir=`pwd`'
*      WRITE(LUNOUT,'(A)')         'cd '//CHPATH(:LPATH)
      CHLINE=                     'cd '//CHPATH(:LPATH)
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)') CHLINE(:L)
*      WRITE(LUNOUT,'(A)')         '/bin/rm -f '//NAME(:LN)//'.sl'
      CHLINE=                      '/bin/rm -f '//NAME(:LN)//'.sl'
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
      IF(FEXT.EQ.'.c')THEN
        L=LENOCC(CHCC)
*                                 'cc -c .... name.c'
        CHLINE=CHCC(:L)//' '//NAME(:LN)// '.c'
      ELSE
        L=LENOCC(CHF77)
*                                 'f77 -c .... name.f'
        CHLINE=CHF77(:L)//' '//NAME(:LN)// '.f'
      ENDIF
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
      WRITE(LUNOUT,'(A)')         'errno=$?'
      WRITE(LUNOUT,'(A)')         'if [ $errno != 0 ]'
      WRITE(LUNOUT,'(A)')         'then'
      WRITE(LUNOUT,'(A)')         '   exit $errno'
      WRITE(LUNOUT,'(A)')         'fi'
#if defined(CERNLIB_HPUX)
      CHLINE=                     'ld -b -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
#endif
#if defined(CERNLIB_IBMRT)
      CHLINE=         'if [ ! -r ' // IMPORT(:LIMPORT) // ' ] ; then'
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
      CHLINE=  'echo "#!' // PROG(:LPROG) // '" > '// IMPORT(:LIMPORT)
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
      CHLINE=                'if [ `uname -v` -le "3" ] ; then'
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
      CHLINE=                     '/usr/ucb/nm '// PROG(:LPROG) //
     + ' | egrep '' [BAD] ''| cut -f3 -d'' '' |sed -e ''s/^#/ #/'' '
     + // '| sort | uniq >> '// IMPORT(:LIMPORT)
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
      WRITE(LUNOUT,'(A)')    'else'

      CHLINE=                     'nm '// PROG(:LPROG) //
     + ' | egrep '' [BAD] ''| cut -f1 -d'' '' |sed -e ''s/^#/ #/'' '
     + // '| sort | uniq >> '// IMPORT(:LIMPORT)
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
      WRITE(LUNOUT,'(A)')    'fi'
      WRITE(LUNOUT,'(A)')    'fi'

      WRITE(LUNOUT,'(A)')    'if [ `uname -v` -le "3" ] ; then'
      CHLINE=                     '/usr/ucb/nm '// NAME(:LN) //
     + '.o | egrep '' [BAD] ''| cut -f3 -d'' '' |sed -e ''s/^#/ #/'' '
     + // '| sort | uniq > '// NAME(:LN) //'.exp'
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
      CHLINE=                     'ld -bE:'// NAME(:LN) //'.exp -o '
     + // NAME(:LN) //'.sl '//IMPORT(:LIMPORT+1) // NAME(:LN) //'.o '
     + // '-bh:8 -berok -e _nostart -T512 -H512'
      L=LENOCC(CHLINE)
      CHLINE= CHLINE(:L)// ' -lxlf90 -lxlf -lm -lc'
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)

      WRITE(LUNOUT,'(A)')    'else'
      CHLINE=                     'nm '// NAME(:LN) //
     + '.o | egrep '' [BAD] ''| cut -f1 -d'' '' |sed -e ''s/^#/ #/'' '
     + // '| sort | uniq > '// NAME(:LN) //'.exp'
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
      CHLINE=                     'ld -bE:'// NAME(:LN) //'.exp -o '
     + // NAME(:LN) //'.sl '//IMPORT(:LIMPORT+1) // NAME(:LN) //'.o '
     + // '-bh:8 -berok -bnoentry -T512 -H512'
      L=LENOCC(CHLINE)
      CHLINE= CHLINE(:L)// ' -lxlf90 -lxlf -lm -lc'
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)

      WRITE(LUNOUT,'(A)')    'fi'

      CHLINE=                     '#==========='
#endif
#if (defined(CERNLIB_SUN))&&(defined(CERNLIB_SOLARIS))
      CHLINE=                     '/usr/ccs/bin/ld -G -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
#endif
#if (defined(CERNLIB_SUN))&&(!defined(CERNLIB_SOLARIS))
      CHLINE=                     'ld -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
#endif
#if defined(CERNLIB_SGI)
      CALL GETENV('USERLIB',USERLIBS)
      LUSRLIB=LENOCC(USERLIBS)
      IF(LUSRLIB.GT.0)THEN
        CHLINE=                   'ld -n32 -shared -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o \\'
      ELSE
        CHLINE=                   'ld -n32 -shared -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
      ENDIF
#endif
#if defined(CERNLIB_LINUX) && !defined(CERNLIB_MACOSX)
      CHLINE=                   'ld -shared -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
#endif
#if defined(CERNLIB_MACOSX)
      CHLINE= 'g77 -bundle -bundle_loader `/usr/bin/which pawX11` '
     +                    // ' -bind_at_load -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
#endif
#if defined(CERNLIB_ALPHA_OSF)
        CHLINE=                   'ld -shared -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o \\'
#endif
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
#if defined(CERNLIB_SGI)
      IF(LUSRLIB.GT.0)THEN
        WRITE(LUNOUT,'(A)')USERLIBS(:LUSRLIB)
      ENDIF
#endif
#if defined(CERNLIB_ALPHA_OSF)
      WRITE(LUNOUT,'(A)')       ' -lUfor -lfor -lFutil -lm -lots -lc'
#endif
      WRITE(LUNOUT,'(A)')         'errno=$?'
      WRITE(LUNOUT,'(A)')         'if [ $errno != 0 ]'
      WRITE(LUNOUT,'(A)')         'then'
      WRITE(LUNOUT,'(A)')         '   exit $errno'
      WRITE(LUNOUT,'(A)')         'fi'
      CHLINE=                     '/bin/chmod 555 '//NAME(:LN)//'.sl'
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
*?      CHLINE='/bin/rm -f *_'//CPID(:LP)//'.o *_'//CPID(:LP)//'.f'
*      CHLINE=                     '/bin/rm -f *_'//CPID(:LP)//'.o'
      CHLINE=                     '/bin/rm -f '//NAME(:LN)//'.o'
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
      WRITE(LUNOUT,'(A)')         'cd $olddir'
      WRITE(LUNOUT,'(A)')         'exit 0'
      CALL CSCLOS(LUNOUT)
      CLOSE(LUNOUT)
      RETURN
 99   PRINT *,' CS: could not open file: ',FEXEC(:LENOCC(FEXEC))
      IERR=1
      IF(LUNOUT.GT.0)CALL CSCLOS(LUNOUT)
      END
#endif
