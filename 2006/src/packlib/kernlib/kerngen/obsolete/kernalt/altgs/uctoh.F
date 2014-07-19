*
* $Id: uctoh.F,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: uctoh.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE UCTOH (MS,MT,NPW,NCHP)
C
C CERN PROGLIB# M409    UCTOH           .VERSION KERNALT  1.00  880212
C ORIG. 10/02/88  JZ
C
      DIMENSION    MS(99), MT(99), NCHP(9)
 
      DIMENSION    MWDV(3)
      CHARACTER    CHWD*12
      EQUIVALENCE (CHWD,MWDV)
 
      PARAMETER    (IBLAN1 = '00202020'X)
      PARAMETER    (IBLAN2 = '00002020'X)
      PARAMETER    (IBLAN3 = '00000020'X)
      PARAMETER    (MASK1  = 'FF000000'X)
      PARAMETER    (MASK2  = 'FFFF0000'X)
      PARAMETER    (MASK3  = 'FFFFFF00'X)
 
      DIMENSION    IBLANK(3), MASK(3)
      DATA  IBLANK / '00202020'X, '00002020'X, '00000020'X /
      DATA  MASK   / 'FF000000'X, 'FFFF0000'X, 'FFFFFF00'X /
 
 
      NCH = NCHP(1)
      IF   (NCH)             91, 29, 11
   11 GO TO (21, 31, 41), NPW
 
C--------          NPW = 4
 
      NWS    = ishft (NCH,-2)
      NTRAIL = IAND (NCH,3)
      IF (NWS.EQ.0)          GO TO 15
 
      DO 14  J=1,NWS
   14 MT(J) = MS(J)
      IF (NTRAIL.EQ.0)       RETURN
 
   15 MT(NWS+1) = IOR (IBLANK(NTRAIL), IAND(MS(NWS+1),MASK(NTRAIL)))
      RETURN
 
C--------          NPW = 1
C--                equivalent to 'CALL UCTOH1(MS,MT,NCH)'
 
   21 NWS    = ishft (NCH,-2)
      NTRAIL = IAND (NCH,3)
      JT     = 0
      IF (NWS.EQ.0)          GO TO 26
 
C--                Unpack the initial complete words
 
      DO 24 JS=1,NWS
      MWD      = MS(JS)
      MT(JT+1) = IOR (IBLAN1,IAND(MASK1,MWD))
      MT(JT+2) = IOR (IBLAN1,IAND(MASK1,ISHFT(MWD, 8)))
      MT(JT+3) = IOR (IBLAN1,IAND(MASK1,ISHFT(MWD,16)))
      MT(JT+4) = IOR (IBLAN1,           ISHFT(MWD,24) )
   24 JT = JT + 4
 
      IF (NTRAIL.EQ.0)       RETURN
 
C--                Unpack the trailing word
 
   26 MWD = MS(NWS+1)
 
      DO 28 JS=1,NTRAIL
      MT(JT+1) = IOR (IBLAN1,IAND(MASK1,MWD))
      MWD = ISHFT (MWD,8)
   28 JT = JT + 1
   29 RETURN
 
C--------          NPW = 2
 
   31 NWS    = ishft (NCH,-2)
      NTRAIL = IAND (NCH,3)
      JT     = 0
      IF (NWS.EQ.0)          GO TO 36
 
C--                Unpack the initial complete words
 
      DO 34 JS=1,NWS
      MWD      = MS(JS)
      MT(JT+1) = IOR (IBLAN2,IAND(MASK2,MWD))
      MT(JT+2) = IOR (IBLAN2,ISHFT(MWD,16))
   34 JT = JT + 2
 
      IF (NTRAIL.EQ.0)       RETURN
 
C--                Unpack the trailing word
 
   36 MWD = MS(NWS+1)
 
      IF (NTRAIL.EQ.1) THEN
          MT(JT+1) = IOR (IBLAN1,IAND(MASK1,MWD))
          RETURN
 
        ELSEIF (NTRAIL.EQ.2)  THEN
          MT(JT+1) = IOR (IBLAN2,IAND(MASK2,MWD))
          RETURN
 
        ELSE
          MT(JT+1) = IOR (IBLAN2,IAND(MASK2,MWD))
          MT(JT+2) = IOR (IBLAN1,IAND(MASK1,ISHFT(MWD,16)))
        ENDIF
      RETURN
 
C--------          NPW = 3
 
   41 NWS    = NCH/12
      NTRAIL = NCH - 12*NWS
      JS     = 0
      JT     = 0
      IF (NWS.EQ.0)          GO TO 46
 
C--                Unpack the initial complete words
 
      DO 44  JL=1,NWS
      MWDV(1)  = MS(JS+1)
      MWDV(2)  = MS(JS+2)
      MWDV(3)  = MS(JS+3)
      MT(JT+1) =      IOR (IBLAN3,IAND(MASK3,MWDV(1)))
      MT(JT+2) = IOR (IOR (IBLAN3,ISHFT(MWDV(1),24))
     +,                        ishft(IAND(MASK2,MWDV(2)),-8))
      MT(JT+3) = IOR (IOR (IBLAN3,ISHFT(MWDV(2),16))
     +,                        ishft(IAND(MASK1,MWDV(3)),-16))
      MT(JT+4) =      IOR (IBLAN3,ISHFT(MWDV(3),8))
      JS = JS + 3
   44 JT = JT + 4
 
      IF (NTRAIL.EQ.0)       RETURN
 
C--                Unpack the trailing words
 
   46 MWDV(1) = MS(JS+1)
      MWDV(2) = MS(JS+2)
      MWDV(3) = MS(JS+3)
 
      CHWD(NTRAIL+1:12) = ' '
 
      MT(JT+1) =     IOR (IBLAN3,IAND(MASK3,MWDV(1)))
      IF (NTRAIL.LE.3)       RETURN
 
      MT(JT+2) = IOR (IOR (IBLAN3,ISHFT(MWDV(1),24))
     +,                        ishft(IAND(MASK2,MWDV(2)),-8))
      IF (NTRAIL.LE.6)       RETURN
 
      MT(JT+3) = IOR (IOR (IBLAN3,ISHFT(MWDV(2),16))
     +,                        ishft(IAND(MASK1,MWDV(3)),-16))
      IF (NTRAIL.LE.9)       RETURN
 
      MT(JT+4) =     IOR (IBLAN3,ISHFT(MWDV(3),8))
      RETURN
 
   91 CALL ABEND
      END
#ifdef CERNLIB_TCGEN_UCTOH
#undef CERNLIB_TCGEN_UCTOH
#endif
