*
* $Id: uctoh.F,v 1.1.1.1 1996/02/15 17:51:06 mclareni Exp $
*
* $Log: uctoh.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:06  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
#if !defined(CERNLIB_QMAPO10)
      SUBROUTINE UCTOH (MS,MT,NPW,NCHP)
C
C CERN PROGLIB# M409    UCTOH           .VERSION KERNAPO  1.11  880222
C ORIG. 10/02/88  JZ
C
      DIMENSION    MS(99), MT(99), NCHP(9)

      DIMENSION    MWDV(3)
      CHARACTER    CHWD*12
      EQUIVALENCE (CHWD,MWDV)

      PARAMETER    (IBLAN1 = 16#00202020)
      PARAMETER    (IBLAN2 = 16#00002020)
      PARAMETER    (IBLAN3 = 16#00000020)
      PARAMETER    (MASK1  = 16#FF000000)
      PARAMETER    (MASK2  = 16#FFFF0000)
      PARAMETER    (MASK3  = 16#FFFFFF00)

      DIMENSION    IBLANK(3), MASK(3)
      DATA  IBLANK / 16#00202020, 16#00002020, 16#00000020 /
      DATA  MASK   / 16#FF000000, 16#FFFF0000, 16#FFFFFF00 /


      NCH = NCHP(1)
      IF   (NCH)             91, 29, 11
   11 GO TO (21, 31, 41), NPW

C--------          NPW = 4

      NWS    = RSHFT (NCH,2)
      NTRAIL = AND (NCH,3)
      IF (NWS.EQ.0)          GO TO 15

      DO 14  J=1,NWS
   14 MT(J) = MS(J)
      IF (NTRAIL.EQ.0)       RETURN

   15 MT(NWS+1) = OR (IBLANK(NTRAIL), AND(MS(NWS+1),MASK(NTRAIL)))
      RETURN

C--------          NPW = 1
C--                equivalent to 'CALL UCTOH1(MS,MT,NCH)'

   21 NWS    = RSHFT (NCH,2)
      NTRAIL = AND (NCH,3)
      JT     = 0
      IF (NWS.EQ.0)          GO TO 26

C--                Unpack the initial complete words

      DO 24 JS=1,NWS
      MWD      = MS(JS)
      MT(JT+1) = OR (IBLAN1,AND(MASK1,MWD))
      MT(JT+2) = OR (IBLAN1,AND(MASK1,LSHFT(MWD, 8)))
      MT(JT+3) = OR (IBLAN1,AND(MASK1,LSHFT(MWD,16)))
      MT(JT+4) = OR (IBLAN1,          LSHFT(MWD,24) )
   24 JT = JT + 4

      IF (NTRAIL.EQ.0)       RETURN

C--                Unpack the trailing word

   26 MWD = MS(NWS+1)

      DO 28 JS=1,NTRAIL
      MT(JT+1) = OR (IBLAN1,AND(MASK1,MWD))
      MWD = LSHFT (MWD,8)
   28 JT = JT + 1
   29 RETURN

C--------          NPW = 2

   31 NWS    = RSHFT (NCH,2)
      NTRAIL = AND (NCH,3)
      JT     = 0
      IF (NWS.EQ.0)          GO TO 36

C--                Unpack the initial complete words

      DO 34 JS=1,NWS
      MWD      = MS(JS)
      MT(JT+1) = OR (IBLAN2,AND(MASK2,MWD))
      MT(JT+2) = OR (IBLAN2,LSHFT(MWD,16))
   34 JT = JT + 2

      IF (NTRAIL.EQ.0)       RETURN

C--                Unpack the trailing word

   36 MWD = MS(NWS+1)

      IF (NTRAIL.EQ.1) THEN
          MT(JT+1) = OR (IBLAN1,AND(MASK1,MWD))
          RETURN

        ELSEIF (NTRAIL.EQ.2)  THEN
          MT(JT+1) = OR (IBLAN2,AND(MASK2,MWD))
          RETURN

        ELSE
          MT(JT+1) = OR (IBLAN2,AND(MASK2,MWD))
          MT(JT+2) = OR (IBLAN1,AND(MASK1,LSHFT(MWD,16)))
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
      MT(JT+1) =      OR (IBLAN3,AND(MASK3,MWDV(1)))
      MT(JT+2) = OR (OR (IBLAN3,LSHFT(MWDV(1),24))
     +,                        RSHFT(AND(MASK2,MWDV(2)),8))
      MT(JT+3) = OR (OR (IBLAN3,LSHFT(MWDV(2),16))
     +,                        RSHFT(AND(MASK1,MWDV(3)),16))
      MT(JT+4) =      OR (IBLAN3,LSHFT(MWDV(3),8))
      JS = JS + 3
   44 JT = JT + 4

      IF (NTRAIL.EQ.0)       RETURN

C--                Unpack the trailing words

   46 MWDV(1) = MS(JS+1)
      MWDV(2) = MS(JS+2)
      MWDV(3) = MS(JS+3)

      CHWD(NTRAIL+1:12) = ' '

      MT(JT+1) =     OR (IBLAN3,AND(MASK3,MWDV(1)))
      IF (NTRAIL.LE.3)       RETURN

      MT(JT+2) = OR (OR (IBLAN3,LSHFT(MWDV(1),24))
     +,                        RSHFT(AND(MASK2,MWDV(2)),8))
      IF (NTRAIL.LE.6)       RETURN

      MT(JT+3) = OR (OR (IBLAN3,LSHFT(MWDV(2),16))
     +,                        RSHFT(AND(MASK1,MWDV(3)),16))
      IF (NTRAIL.LE.9)       RETURN

      MT(JT+4) =     OR (IBLAN3,LSHFT(MWDV(3),8))
      RETURN

   91 CALL ABEND
      END
#ifdef CERNLIB_TCGEN_UCTOH
#undef CERNLIB_TCGEN_UCTOH
#endif
#endif
