*
* $Id: uhtoc.F,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: uhtoc.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE UHTOC (MS,NPW,MT,NCHP)
C
C CERN PROGLIB# M409    UHTOC           .VERSION KERNALT  1.01  880222
C ORIG. 22/02/88  JZ
C
      DIMENSION    MS(99), MT(99), NCHP(9)
 
      PARAMETER    (MASK1  = 'FF000000'X)
      PARAMETER    (MASK2  = 'FFFF0000'X)
      PARAMETER    (MASK3  = 'FFFFFF00'X)
 
      DIMENSION    MASK(3)
      DATA  MASK   / 'FF000000'X, 'FFFF0000'X, 'FFFFFF00'X /
 
 
      NCH = NCHP(1)
      IF   (NCH)             91, 29, 11
   11 GO TO (21, 31, 41), NPW
 
C--------          NPW = 4
 
      NWT    = ishft (NCH,-2)
      NTRAIL = IAND (NCH,3)
      IF (NWT.EQ.0)          GO TO 15
 
      DO 14  J=1,NWT
   14 MT(J) = MS(J)
      IF (NTRAIL.EQ.0)       RETURN
 
   15 MT(NWT+1) = IOR (IAND (MS(NWT+1),    MASK(NTRAIL))
     +,                IAND (MT(NWT+1),NOT(MASK(NTRAIL))))
      RETURN
 
C--------          NPW = 1
C--                equivalent to 'CALL UH1TOC(MS,MT,NCH)'
 
   21 NWT    = ishft (NCH,-2)
      NTRAIL = IAND (NCH,3)
      JS     = 0
      IF (NWT.EQ.0)          GO TO 26
 
C--                Pack the initial complete words
 
      DO 24 JT=1,NWT
      MT(JT) = IOR (IOR (IOR (
     +                   IAND(MASK1,MS(JS+1))
     +,           ishft (IAND(MASK1,MS(JS+2)), -8))
     +,           ishft (IAND(MASK1,MS(JS+3)),-16))
     +,           ishft            (MS(JS+4), -24))
   24 JS = JS + 4
 
      IF (NTRAIL.EQ.0)       RETURN
 
C--                Pack the trailing word
 
   26 GO TO ( 28, 27), NTRAIL
 
      MT(NWT+1) = IOR (IOR (IOR (
     +                   IAND(MASK1,MS(JS+1))
     +,           ishft (IAND(MASK1,MS(JS+2)), -8))
     +,           ishft (IAND(MASK1,MS(JS+3)),-16))
     +,           IAND  (NOT(MASK3),MT(NWT+1)))
      RETURN
 
   27 MT(NWT+1) = IOR (IOR (
     +                   IAND(MASK1,MS(JS+1))
     +,           ishft (IAND(MASK1,MS(JS+2)), -8))
     +,           IAND  (NOT(MASK2),MT(NWT+1)))
      RETURN
 
   28 MT(NWT+1) = IOR (
     +                   IAND(MASK1,MS(JS+1))
     +,           IAND  (NOT(MASK1),MT(NWT+1)))
   29 RETURN
 
C--------          NPW = 2
 
   31 NWT    = ishft (NCH,-2)
      NTRAIL = IAND (NCH,3)
      JS     = 0
      IF (NWT.EQ.0)          GO TO 36
 
C--                Pack the initial complete words
 
      DO 34 JT=1,NWT
      MT(JT) = IOR (IAND(MASK2,MS(JS+1))
     +,                 ishft (MS(JS+2),-16))
   34 JS = JS + 2
 
      IF (NTRAIL.EQ.0)       RETURN
 
C--                Pack the trailing word
 
   36 GO TO ( 38, 37), NTRAIL
 
      MT(NWT+1) = IOR (IOR (
     +                   IAND(MASK2,MS(JS+1))
     +,           ishft (IAND(MASK1,MS(JS+2)),-16))
     +,           IAND  (NOT(MASK3),MT(NWT+1)))
      RETURN
 
   37 MT(NWT+1) = IOR (
     +                   IAND(MASK2,MS(JS+1))
     +,           IAND  (NOT(MASK2),MT(NWT+1)))
      RETURN
 
   38 MT(NWT+1) = IOR (
     +                   IAND(MASK1,MS(JS+1))
     +,           IAND  (NOT(MASK1),MT(NWT+1)))
      RETURN
 
C--------          NPW = 3
 
   41 NWT    = NCH/12
      NTRAIL = NCH - 12*NWT
      JS     = 0
      JT     = 0
      IF (NWT.EQ.0)          GO TO 46
 
C--                Pack the initial complete words
 
      DO 44  JL=1,NWT
      MT(JT+1) =   IOR ( IAND (      MS(JS+1),      MASK3)
     +,                 ishft       (MS(JS+2),-24))
 
      MT(JT+2) =   IOR ( IAND (ISHFT(MS(JS+2),  8), MASK2)
     +,                 ishft       (MS(JS+3),-16))
 
      MT(JT+3) =   IOR ( IAND (ISHFT(MS(JS+3), 16), MASK1)
     +,                 ishft       (MS(JS+4), -8))
      JS = JS + 4
   44 JT = JT + 3
 
      IF (NTRAIL.EQ.0)       RETURN
 
C--                Pack the trailing word
 
   46 IF (NTRAIL.LE.3)  THEN
          MT(JT+1) =   IOR ( IAND (      MS(JS+1),      MASK(NTRAIL))
     +,                     IAND (MT(JT+1),NOT(MASK(NTRAIL))))
          RETURN
 
        ELSEIF (NTRAIL.LE.6)  THEN
          MT(JT+1) =   IOR ( IAND (      MS(JS+1),      MASK3)
     +,                     ishft     (MS(JS+2), -24))
 
          N  =  NTRAIL - 4
          IF (N.EQ.0)        RETURN
 
          MT(JT+2) =   IOR ( IAND (ISHFT(MS(JS+2),  8), MASK(N))
     +,                     IAND (MT(JT+2),NOT(MASK(N))))
          RETURN
 
        ELSEIF (NTRAIL.LT.9)  THEN
          MT(JT+1) =   IOR ( IAND (      MS(JS+1),      MASK3)
     +,                     ishft     (MS(JS+2), -24))
 
          MWD      =   IOR ( IAND (ISHFT(MS(JS+2),  8), MASK2)
     +,                     ishft     (MS(JS+3), -16))
          N  =  NTRAIL - 4
          IF (N.EQ.3)  THEN
              MWD = IOR ( IAND(MWD,         MASK(N))
     +,                  IAND(MT(JT+2),NOT(MASK(N))))
            ENDIF
          MT(JT+2) = MWD
          RETURN
 
        ELSE
          MT(JT+1) =   IOR ( IAND (      MS(JS+1),      MASK3)
     +,                     ishft     (MS(JS+2), -24))
 
          MT(JT+2) =   IOR ( IAND (ISHFT(MS(JS+2),  8), MASK2)
     +,                     ishft     (MS(JS+3), -16))
 
          MWD      =   IOR ( IAND (ISHFT(MS(JS+3), 16), MASK1)
     +,                     ishft     (MS(JS+4),  -8))
          N = NTRAIL - 8
          MWD = IOR ( IAND(MWD,         MASK(N))
     +,              IAND(MT(JT+3),NOT(MASK(N))))
          MT(JT+3) = MWD
        ENDIF
      RETURN
 
   91 CALL ABEND
      END
#ifdef CERNLIB_TCGEN_UHTOC
#undef CERNLIB_TCGEN_UHTOC
#endif
