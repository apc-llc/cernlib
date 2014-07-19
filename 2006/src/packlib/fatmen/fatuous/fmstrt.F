*
* $Id: fmstrt.F,v 1.1.1.1 1996/03/07 15:18:04 mclareni Exp $
*
* $Log: fmstrt.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:04  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSTRT(LUNRZ,LUNFZ,CHFAT,IRC)
*CMZ :          11/12/90  15.35.08  by  Jamie Shiers
*-- Author :
 
      CHARACTER*(*) CHFAT
 
#include "fatmen/fat.inc"
#include "fatmen/fatusr.inc"
#include "fatmen/fatbug.inc"
#include "zebra/mzca.inc"
*
      COMMON /QUEST/IQUEST(100)
*
#include "fatmen/fmnkeys.inc"
      DIMENSION KEY(LKEYFA)
      DIMENSION IZKEYS(3)
      SAVE NENTRY
#if defined(CERNLIB_F77)
      DATA IZKEYS(1)/4HEBRA/,IZKEYS(2)/4HINIT/,IZKEYS(3)/4HDONE/
#endif
      DATA NENTRY/0/
*
*     Fix for F90 compiler. Zebra will have to be changed anyway...
*
      CALL UCTOH('EBRA',IZKEYS(1),4,4)
      CALL UCTOH('INIT',IZKEYS(2),4,4)
      CALL UCTOH('DONE',IZKEYS(3),4,4)
*
*     Check on input parameters
*
      IF(LUNRZ.LE.0) THEN
         PRINT *,'FMSTRT. invalid logical unit specified for ',
     +           'reading FATMEN RZ file - ',LUNRZ
         CALL FMSTOP
      ENDIF
 
      IF(LUNFZ.LT.0) THEN
         PRINT *,'FMSTRT. invalid logical unit specified for ',
     +           'writing FATMEN updates - ',LUNFZ
         CALL FMSTOP
      ENDIF
 
      LCHFAT = LENOCC(CHFAT)
      IF(LCHFAT.EQ.0) THEN
         PRINT *,'FMSTRT. invalid FATMEN database specified'
         CALL FMSTOP
      ENDIF
*
*     Has ZEBRA already been initialised?
*
      DO 10  J=1,3
      IF (MQKEYS(J).NE.IZKEYS(J))  GO TO 20
   10 CONTINUE
      IF(IDEBFA.GE.-3) PRINT *,'FMSTRT. info - ZEBRA has already ',
     +   'been initialised'
      GOTO 30
   20 CONTINUE
*
*     Initialise ZEBRA
*
      CALL MZEBRA(-3)
   30 CONTINUE
*
*     Initialise store and link areas
*
      IF(NENTRY.NE.0) GOTO 40
      NENTRY = 1
      CALL MZSTOR(IXSTOR,'/FAT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(LURCOR))
      CALL MZLOGL(IXSTOR,-3)
*
* *** Define user division and link area like:
*
      CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')
      CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)
 
40    CONTINUE
*         read mode
      IRC = 0
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,CHFAT(1:LCHFAT),IRC)
      END
