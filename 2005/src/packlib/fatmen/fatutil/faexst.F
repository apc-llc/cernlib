*
* $Id: faexst.F,v 1.1.1.1 1996/03/07 15:18:06 mclareni Exp $
*
* $Log: faexst.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:06  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAEXST(IRC)
*
*     Check if FATMEN catalogue exists
*
      CHARACTER*255 CHFILE
#include "fatmen/fatbug.inc"
#include "fatmen/fatsys.inc"
#if defined(CERNLIB_IBMVM)
      CHARACTER*20 FILE
#endif
      LOGICAL EXIST
 
      IRC  = 0
 
      LDEF = LENOCC(DEFAULT)
      LTOP = LENOCC(TOPDIR)
#if defined(CERNLIB_VAXVMS)
*
*     File CHFILE is in default directory of server
*
      IF(LDEF.GT.0) THEN
         CHFILE = DEFAULT(1:LDEF)
     +         // TOPDIR(3:LTOP) // '.FATRZ'
      ELSE
         CHFILE = TOPDIR(3:LTOP) // '.FATRZ'
      ENDIF
#endif
#if defined(CERNLIB_UNIX)
*
*     File CHFILE is in default directory of server
*
      IF(LDEF.GT.0) THEN
         CHFILE = DEFAULT(1:LDEF) // '/'
     +           // TOPDIR(3:LTOP) // '.FATRZ'
      ELSE
         CHFILE = TOPDIR(3:LTOP) // '.FATRZ'
      ENDIF
*
* *** Check if the file exists
*
#endif
#if defined(CERNLIB_IBMVM)
      CHFILE = '/' // TOPDIR(3:LTOP) // ' FATRZ '//SERMOD
#endif
#if defined(CERNLIB_IBMMVS)
      CHFILE = '/'//DEFAULT(1:LDEF)//'.'
     +           // TOPDIR(3:LTOP) // '.FATRZ'
#endif
      LCHF = LENOCC(CHFILE)
#if defined(CERNLIB_UNIX)
      CALL CUTOL(CHFILE)
#endif
      IF(IDEBFA.GE.1) PRINT *,'FAEXST. looking for FATMEN catalogue ',
     +   'file = ',CHFILE(1:LCHF)
      INQUIRE(FILE=CHFILE(1:LCHF),EXIST=EXIST,IOSTAT=ISTAT)
      IF(.NOT.EXIST) THEN
         IRC = 28
         IF(IDEBFA.GE.-3) PRINT *,'FAEXST. FORTRAN INQUIRE ',
     +      'for FATMEN catalogue failed - ',
     +      'file = ',CHFILE(1:LCHF)
      ENDIF
      END
