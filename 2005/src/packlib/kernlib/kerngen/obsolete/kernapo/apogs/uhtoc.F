*
* $Id: uhtoc.F,v 1.1.1.1 1996/02/15 17:51:06 mclareni Exp $
*
* $Log: uhtoc.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:06  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
#if !defined(CERNLIB_QMAPO10)
      SUBROUTINE UHTOC (MS,NPW,MT,NCHP)
C
C CERN PROGLIB# M409    UHTOC           .VERSION KERNAPO  1.11  880222
C ORIG. 10/02/88  JZ
C
      DIMENSION    MS(99), MT(99), NCHP(9)

      PARAMETER    (MASK1  = 16#FF000000)
      PARAMETER    (MASK2  = 16#FFFF0000)
      PARAMETER    (MASK3  = 16#FFFFFF00)

      DIMENSION    MASK(3)
      DATA  MASK   / 16#FF000000, 16#FFFF0000, 16#FFFFFF00 /


      NCH = NCHP(1)
      IF   (NCH)             91, 29, 11
   11 GO TO (21, 31, 41), NPW

C--------          NPW = 4

      NWT    = RSHFT (NCH,2)
      NTRAIL = AND (NCH,3)
      IF (NWT.EQ.0)          GO TO 15

      DO 14  J=1,NWT
   14 MT(J) = MS(J)
      IF (NTRAIL.EQ.0)       RETURN

   15 MT(NWT+1) = OR (AND (MS(NWT+1),    MASK(NTRAIL))
     +,               AND (MT(NWT+1),NOT(MASK(NTRAIL))))
      RETURN

C--------          NPW = 1
C--                equivalent to 'CALL UH1TOC(MS,MT,NCH)'

   21 NWT    = RSHFT (NCH,2)
      NTRAIL = AND (NCH,3)
      JS     = 0
      IF (NWT.EQ.0)          GO TO 26

C--                Pack the initial complete words

      DO 24 JT=1,NWT
      MT(JT) = OR (OR (OR (
     +                   AND(MASK1,MS(JS+1))
     +,           RSHFT (AND(MASK1,MS(JS+2)), 8))
     +,           RSHFT (AND(MASK1,MS(JS+3)),16))
     +,           RSHFT           (MS(JS+4), 24))
   24 JS = JS + 4

      IF (NTRAIL.EQ.0)       RETURN

C--                Pack the trailing word

   26 GO TO ( 28, 27), NTRAIL

      MT(NWT+1) = OR (OR (OR (
     +                   AND(MASK1,MS(JS+1))
     +,           RSHFT (AND(MASK1,MS(JS+2)), 8))
     +,           RSHFT (AND(MASK1,MS(JS+3)),16))
     +,           AND  (NOT(MASK3),MT(NWT+1)))
      RETURN

   27 MT(NWT+1) = OR (OR (
     +                   AND(MASK1,MS(JS+1))
     +,           RSHFT (AND(MASK1,MS(JS+2)), 8))
     +,           AND  (NOT(MASK2),MT(NWT+1)))
      RETURN

   28 MT(NWT+1) = OR (
     +                   AND(MASK1,MS(JS+1))
     +,           AND  (NOT(MASK1),MT(NWT+1)))
   29 RETURN

C--------          NPW = 2

   31 NWT    = RSHFT (NCH,2)
      NTRAIL = AND (NCH,3)
      JS     = 0
      IF (NWT.EQ.0)          GO TO 36

C--                Pack the initial complete words

      DO 34 JT=1,NWT
      MT(JT) = OR (AND(MASK2,MS(JS+1))
     +,                 RSHFT (MS(JS+2),16))
   34 JS = JS + 2

      IF (NTRAIL.EQ.0)       RETURN

C--                Pack the trailing word

   36 GO TO ( 38, 37), NTRAIL

      MT(NWT+1) = OR (OR (
     +                   AND(MASK2,MS(JS+1))
     +,           RSHFT (AND(MASK1,MS(JS+2)),16))
     +,           AND  (NOT(MASK3),MT(NWT+1)))
      RETURN

   37 MT(NWT+1) = OR (
     +                   AND(MASK2,MS(JS+1))
     +,           AND  (NOT(MASK2),MT(NWT+1)))
      RETURN

   38 MT(NWT+1) = OR (
     +                   AND(MASK1,MS(JS+1))
     +,           AND  (NOT(MASK1),MT(NWT+1)))
      RETURN

C--------          NPW = 3

   41 NWT    = NCH/12
      NTRAIL = NCH - 12*NWT
      JS     = 0
      JT     = 0
      IF (NWT.EQ.0)          GO TO 46

C--                Pack the initial complete words

      DO 44  JL=1,NWT
      MT(JT+1) =   OR ( AND (      MS(JS+1),     MASK3)
     +,                 RSHFT     (MS(JS+2),24))

      MT(JT+2) =   OR ( AND (LSHFT(MS(JS+2), 8), MASK2)
     +,                 RSHFT     (MS(JS+3),16))

      MT(JT+3) =   OR ( AND (LSHFT(MS(JS+3),16), MASK1)
     +,                 RSHFT     (MS(JS+4), 8))
      JS = JS + 4
   44 JT = JT + 3

      IF (NTRAIL.EQ.0)       RETURN

C--                Pack the trailing word

   46 IF (NTRAIL.LE.3)  THEN
          MT(JT+1) =   OR ( AND (MS(JS+1),    MASK(NTRAIL))
     +,                     AND (MT(JT+1),NOT(MASK(NTRAIL))))
          RETURN

        ELSEIF (NTRAIL.LE.6)  THEN
          MT(JT+1) =   OR ( AND   (MS(JS+1),    MASK3)
     +,                     RSHFT (MS(JS+2), 24))

          N  =  NTRAIL - 4
          IF (N.EQ.0)        RETURN

          MT(JT+2) =   OR ( AND (LSHFT(MS(JS+2), 8), MASK(N))
     +,                     AND       (MT(JT+2), NOT(MASK(N))))
          RETURN

        ELSEIF (NTRAIL.LT.9)  THEN
          MT(JT+1) =   OR ( AND   (MS(JS+1),      MASK3)
     +,                     RSHFT (MS(JS+2), 24))

          MWD      =   OR ( AND (LSHFT(MS(JS+2), 8), MASK2)
     +,                     RSHFT     (MS(JS+3),16))
          N  =  NTRAIL - 4
          IF (N.EQ.3)  THEN
              MWD = OR ( AND(MWD,         MASK(N))
     +,                  AND(MT(JT+2),NOT(MASK(N))))
            ENDIF
          MT(JT+2) = MWD
          RETURN

        ELSE
          MT(JT+1) =   OR ( AND (      MS(JS+1),      MASK3)
     +,                     RSHFT     (MS(JS+2), 24))

          MT(JT+2) =   OR ( AND (LSHFT(MS(JS+2),  8), MASK2)
     +,                     RSHFT     (MS(JS+3), 16))

          MWD      =   OR ( AND (LSHFT(MS(JS+3), 16), MASK1)
     +,                     RSHFT     (MS(JS+4),  8))
          N = NTRAIL - 8
          MWD = OR ( AND(MWD,         MASK(N))
     +,              AND(MT(JT+3),NOT(MASK(N))))
          MT(JT+3) = MWD
        ENDIF
      RETURN

   91 CALL ABEND
      END
#ifdef CERNLIB_TCGEN_UHTOC
#undef CERNLIB_TCGEN_UHTOC
#endif
#endif
