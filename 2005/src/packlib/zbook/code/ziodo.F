*
* $Id: ziodo.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: ziodo.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZIODO(LUN,NREC,LREC,IBUF,N,IRW)
C
C     ******************************************************************
C     *                                                                *
C     *       I/O ROUTINES FOR DISK/LCM                                *
C     *                                                                *
C     *       NREC  FIRST RECORD NUMBER                                *
C     *       LREC  LENGTH OF RECORD                                   *
C     *       IBUF  ADDRESS OF BUFFER TO BE READ/WRITTEN ON NREC       *
C     *       N     LENGTH OF IBUF                                     *
C     *       IRW   =1 TO READ                                         *
C     *       IRW   =2 TO WRITE                                        *
C     *       LUN   >0  DISK ACCESS  I/O                               *
C     *       LUN   =0  LCM USER (CDC)  /  FASTIO  (UNIVAC)            *
C     *       LUN   <0  LCM FOR HBOOK                                  *
C     *                                                                *
C     ******************************************************************
C
#if defined(CERNLIB_LCM)
      LEVEL 3,/ZLCM/
      COMMON/ZLCM/ZL(1)
      COMMON/HBLCM1/LX1,LX2,LX3
#endif
      DIMENSION IBUF(1)
#if defined(CERNLIB_UNIVAC)
      DATA FILEZB/'Z$B 'F/
#endif
C
C     ------------------------------------------------------------------
C
      IF (N.LE.0)                                GO TO 99
#if defined(CERNLIB_LCM)
      IF (LUN.GT.0)                              GO TO 20
      JFIRST = NREC * LREC + 1
      IF (LUN.LT.0)JFIRST = JFIRST + LX1
      IF (IRW.NE.1)                              GO TO 10
C
C             READ FROM LCM
C
      CALL MOVLEV(ZL(JFIRST),IBUF,N)
      GO TO 99
C
C             WRITE ON LCM
C
  10  CALL MOVLEV(IBUF,ZL(JFIRST),N)
      GO TO 99
  20  CONTINUE
#endif
#if defined(CERNLIB_UNIVAC)
      IF (LUN.GT.0)                              GO TO 20
C
C     USE WORD-ADDRESSABLE FASTIO WHEN LUN=0
C
      IADD   = NREC * LREC
      IF(IRW.EQ.1) CALL FIOR(FILEZB,IBUF,N,IADD,ISTAT)
      IF(IRW.NE.1) CALL FIOW(FILEZB,IBUF,N,IADD,ISTAT)
      IF (ISTAT.LT.0) THEN
      WRITE(6,10) IRW,NREC,LREC,IADD,ISTAT
      IF (IRW.EQ.1) STOP 'ZIODO'
      ENDIF
      GO TO 99
   10 FORMAT('  I/O ERROR IN ZIODO FOR LUN=0 (FILE Z$B).',/,
     + '  IRW=',I2,'   NREC=',I4,'   LREC=',I5,'   IADD=',I8,
     + '   ISTAT=',O14)
   20 CONTINUE
#endif
      NR     = (N + LREC - 1) / LREC
C
C             READ FROM LOGICAL UNIT LUN
C
      IF (IRW.NE.1)                              GO TO 40
      DO 30 I = 1,NR
      IREC   = NREC + I
      JFIRST = (I - 1) * LREC + 1
      JLAST  = JFIRST + LREC - 1
      IF (JLAST.GT.N)JLAST = N
#if (defined(CERNLIB_SINGLE)||defined(CERNLIB_DOUBLE))&&(!defined(CERNLIB_CDC))&&(!defined(CERNLIB_VAX))
      READ(UNIT=LUN,REC=IREC)(IBUF(J),J = JFIRST,JLAST)
#endif
#if defined(CERNLIB_CDC)
      CALL READMS(LUN,IBUF(JFIRST),JLAST-JFIRST+1,IREC)
#endif
#if defined(CERNLIB_NORD)
C      CALL RFILE(LUN,0,IBUF(JFIRST),IREC,JLAST-JFIRST+1)
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_PR1ME)||defined(CERNLIB_UNIVAC)||defined(CERNLIB_PDP10)
      READ(LUN'IREC)(IBUF(J),J = JFIRST,JLAST)
#endif
#if defined(CERNLIB_BESM6)
      IWAD=(IREC-1)*NREC+1
      CALL RREAD(LUN,IBUF(JFIRST),IBUF(JLAST),IWAD)
#endif
  30  CONTINUE
      GO TO 99
C
C             WRITE ONTO LOGICAL UNIT LUN
C
  40  CONTINUE
      DO 50 I = 1,NR
      IREC   = NREC + I
      JFIRST = (I - 1) * LREC + 1
      JLAST  = JFIRST + LREC - 1
      IF (JLAST.GT.N)JLAST = N
#if (defined(CERNLIB_SINGLE)||defined(CERNLIB_DOUBLE))&&(!defined(CERNLIB_CDC))&&(!defined(CERNLIB_VAX))
      WRITE(UNIT=LUN,REC=IREC)(IBUF(J),J = JFIRST,JLAST)
#endif
#if defined(CERNLIB_CDC)
      CALL WRITMS(LUN,IBUF(JFIRST),JLAST-JFIRST+1,IREC)
#endif
#if defined(CERNLIB_NORD)
C      CALL WFILE(LUN,0,IBUF(JFIRST),IREC,JLAST-JFIRST+1)
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_PR1ME)||defined(CERNLIB_UNIVAC)||defined(CERNLIB_PDP10)
      WRITE(LUN'IREC)(IBUF(J),J = JFIRST,JLAST)
#endif
#if defined(CERNLIB_BESM6)
      IWAD=(IREC-1)*LREC+1
      CALL RWRITE(LUN,IBUF(JFIRST),IBUF(JLAST),IWAD)
#endif
  50  CONTINUE
C
  99  RETURN
      END
