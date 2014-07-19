*
* $Id: fmthrg.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmthrg.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMTHRG
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatron.inc"
      CHARACTER*255 PATH
 
      CALL KUGETC(PATH,LP)
      IF(PATH(1:2).NE.'//') THEN
         THRONG = PATH
         LTHR   = LP
 
#if defined(CERNLIB_DESY)
         TOPDIR = '//DESY'
#endif
#if defined(CERNLIB_FNAL)
         TOPDIR = '//FNAL'
#endif
#if (!defined(CERNLIB_FNAL))&&(!defined(CERNLIB_DESY))
         TOPDIR = '//CERN'
#endif
 
      ELSE
         ISTART = 3
         IEND   = INDEX(PATH(ISTART:LP),'/') - 1
         TOPDIR = PATH(1:IEND+2)
         THRONG = PATH(INDEXB(PATH(1:LP),'/')+1:LP)
         LTHR   = LENOCC(THRONG)
      ENDIF
      IF(IDEBFA.GE.3) PRINT *,'FMTHRG. TOPDIR = ',TOPDIR,
     +   ' GROUP = ',THRONG
      IF (THRONG(1:4) .NE. 'NONE') CALL FMINIC
      END
