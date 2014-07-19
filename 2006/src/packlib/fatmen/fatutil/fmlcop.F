*
* $Id: fmlcop.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fmlcop.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLCOP(DSN1,DSN2,CHOPT,IRC)
 
      CHARACTER*(*) DSN1
      CHARACTER*(*) DSN2
      CHARACTER*255 CHDSN1,CHDSN2
#include "fatmen/fatbug.inc"
      INTEGER       SYSTEMF
#if defined(CERNLIB_UNIX)
      CHARACTER*3   CHCOPY
#endif
#if defined(CERNLIB_VAXVMS)
#include "fatmen/slate.inc"
      CHARACTER*255 CHFILE
      CHARACTER*18  CHCOPY
#endif
 
#include "fatmen/fatopts.inc"
#if defined(CERNLIB_UNIX)
      CHCOPY = 'cp '
#endif
#if defined(CERNLIB_VAXVMS)
      CHCOPY = 'COPY/LOG/NOCONFIRM'
#endif
 
      IRC = 0
 
      LDSN1  = LENOCC(DSN1)
      LDSN2  = LENOCC(DSN2)
      CHDSN1 = DSN1(1:LDSN1)
      CHDSN2 = DSN2(1:LDSN2)
 
#if defined(CERNLIB_UNIX)
      IF(IOPTC.EQ.0) THEN
         CALL CUTOL(CHDSN1(1:LDSN1))
         CALL CUTOL(CHDSN2(1:LDSN2))
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Search list handling...
*
      CHFILE = CHDSN2(1:LDSN2)
      CALL FMXDSK(CHFILE(1:LDSN2),CHDSN2,IRC)
      LDSN2 = IS(1)
#endif
#if !defined(CERNLIB_IBM)
      IF(IDEBFA.GE.0) PRINT *,'FMLCOP. executing ',
     +   CHCOPY//' '//CHDSN1(1:LDSN1)//' '//CHDSN2(1:LDSN2)
      IRC = SYSTEMF(CHCOPY//' '//CHDSN1(1:LDSN1)//' '//CHDSN2(1:LDSN2))
#endif
 
      END
