*
* $Id: clrstk.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: clrstk.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE CLRSTK(I1,I2)
C.----------------------------------------------------------------------
C.
C.    THIS SUBROUTINE CLEARS THE STACK OF SOFT PARTONS.
C.    LAST UPDATE: 10/04/89
C.
C.----------------------------------------------------------------------
#include "eurodec/eudopt.inc"
#include "eurodec/ptable.inc"
#include "eurodec/convrt.inc"
#include "eurodec/hadgen.inc"
#include "eurodec/result.inc"
      PARAMETER (IDMAX=(NHMAX*(NHMAX+1))/2)
      DIMENSION DELR(IDMAX),IPCOMB(IDMAX),IREMOV(NHMAX)
C--
C--   DETERMINE DELTA R FOR ALLOWED PARTON COMBINATIONS
      NCOMB=0
      DO 10 I=I1,I2-1
        DO 10 J=I+1,I2
C--
C--   REJECT COMBINATIONS WHERE PARTONS ORIGINATE FROM SAME GLUON
          IF ((IPP(I).GT.0).AND.(IPP(J).GT.0)) THEN
            IOR1=IORIGT(IPP(I)/10000)
            IOR2=IORIGT(IPP(J)/10000)
            IF (IOR1.EQ.IOR2) THEN
              IF (IOR1.GT.0) THEN
C--
C--   PARENT GLUON IN /RESULT/ ?
                IF (INDEXT(IOR1-10000*(IOR1/10000)).EQ.9) GOTO 10
              ELSE
C--
C--   PARENT GLUON IN /MOMGEN/ !
                GOTO 10
              ENDIF
            ENDIF
          ENDIF
          IQ=IH(I)
          JQ=IH(J)
C--
C--   ONLY ALLOW FOR THE COMBINATIONS:
C--   QQBAR', QBARQ', QQ'Q'', QBARQBAR'QBAR'', QQ'QBAR''QBAR'''.
          IF ((IQ.GT.0).AND.(IQ.LT.9)) THEN
            IF ((JQ.GT.0).AND.(JQ.LT.9)) GOTO 10
            IF ((JQ.LT.-10).AND.(JQ.GT.-90)) GOTO 10
          ELSEIF ((IQ.GT.-9).AND.(IQ.LT.0)) THEN
            IF ((JQ.GT.-9).AND.(JQ.LT.0)) GOTO 10
            IF ((JQ.GT.10).AND.(JQ.LT.90)) GOTO 10
          ELSEIF ((IQ.GT.10).AND.(IQ.LT.90)) THEN
            IF ((JQ.GT.-9).AND.(JQ.LT.0)) GOTO 10
            IF ((JQ.GT.10).AND.(JQ.LT.90)) GOTO 10
          ELSEIF ((IQ.LT.-10).AND.(IQ.GT.-90)) THEN
            IF ((JQ.GT.0).AND.(JQ.LT.9)) GOTO 10
            IF ((JQ.LT.-10).AND.(JQ.GT.-90)) GOTO 10
          ENDIF
          NCOMB=NCOMB+1
          DELR(NCOMB)=RLEGO(PHA(1,J),PHA(1,I))
          IPCOMB(NCOMB)=100*I+J
   10 CONTINUE
C--
C--   ORDER ACCORDING TO SMALLEST LEGO-PLOT DISTANCE...
      CALL ORDSTK(DELR,IPCOMB,NCOMB)
      ICOMB=0
      ILEFT=I2-I1+1
C--
C--   CHECK WHETHER LAST TWO PARTONS CAN COMBINE!
      DO 30 N=1,NCOMB
        I=IPCOMB(N)/100
        J=IPCOMB(N)-I*100
C--
C--   SEE WHETHER PARTON(S) ARE ALREADY IN PREVIOUS COMBINATION
        IF (ICOMB.GT.0) THEN
          KMAX=2*ICOMB
          DO 20 K=1,KMAX
   20     IF ((I.EQ.IREMOV(K)).OR.(J.EQ.IREMOV(K))) GOTO 30
        ENDIF
        ILEFT=ILEFT-2
        ICOMB=ICOMB+1
        NLAST=N
        IREMOV(2*ICOMB-1)=I
        IREMOV(2*ICOMB)=J
   30 CONTINUE
C--
C--   BREAK LAST COMBINATION IN CASE PARTONS ARE LEFT OVER...
      IF (ILEFT.EQ.0) NLAST=0
      ICOMB=0
      ILEFT=I2-I1+1
      DO 90 N=1,NCOMB
        IF (N.EQ.NLAST) GOTO 90
        I=IPCOMB(N)/100
        J=IPCOMB(N)-I*100
C--
C--   SEE WHETHER PARTON(S) ARE ALREADY IN PREVIOUS COMBINATION
        IF (ICOMB.GT.0) THEN
          KMAX=2*ICOMB
          DO 40 K=1,KMAX
   40     IF ((I.EQ.IREMOV(K)).OR.(J.EQ.IREMOV(K))) GOTO 90
        ENDIF
        IQ=IH(I)
        JQ=IH(J)
C--
C--     GET POINTER TO /RESULT/ AND SELECT BARYONS AND MESONS
        JQ1=JQ/10
        JQ2=JQ-JQ1*10
        NTPOIN=ABS(IPP(J))/10000
        NTPOIN=ABS(IPP(J))-NTPOIN*10000
   50   IF (JQ1.NE.0) THEN
          INDEXT(NTPOIN)=IDIQRK(JQ1,JQ2,JQ3,1)
          ISPIN=MOD(IABS(INDEXT(NTPOIN)),10)
          IF ((IABS(JQ1).EQ.IABS(JQ2)).AND.(IABS(JQ1).EQ.IABS(JQ3)))
     &    ISPIN=0
        ELSE
          INDEXT(NTPOIN)=IQQBAR(JQ2,JQ3)
          ISPIN=MOD(IABS(INDEXT(NTPOIN)),10)
        ENDIF
        IPPOIN=ICONV(IABS(INDEXT(NTPOIN)))
C--
C--   MASS SMEARING (OPTONAL)
        IF (MSMEAR.EQ.1) THEN
          PTEIL(5,NTPOIN)=BWMASS(IPPOIN)
        ELSE
          PTEIL(5,NTPOIN)=PM(IPPOIN)
        ENDIF
        IF ((PTEIL(5,NTPOIN).GT.PHA(4,J)).AND.(ISPIN.EQ.1)) GOTO 50
        DO 60 K=1,3
   60   PTEIL(K,NTPOIN)=PHA(K,J)
        PTEIL(4,NTPOIN)=SQRT(PTEIL(1,NTPOIN)**2+PTEIL(2,NTPOIN)**2+PTEIL
     &  (3,NTPOIN)**2+PTEIL(5,NTPOIN)**2)
        ITHEL(NTPOIN)=0
        NTPOIN=ABS(IPP(I))/10000
        NTPOIN=ABS(IPP(I))-NTPOIN*10000
        IQ1=IQ/10
        IQ2=IQ-IQ1*10
   70   IF (IQ1.NE.0) THEN
          INDEXT(NTPOIN)=IBARYN(IABS(IQ1),IABS(IQ2),IABS(JQ3))
          IF (JQ3.GT.0) INDEXT(NTPOIN)=-INDEXT(NTPOIN)
          ISPIN=MOD(IABS(INDEXT(NTPOIN)),10)
          IF ((IABS(IQ1).EQ.IABS(IQ2)).AND.(IABS(IQ1).EQ.IABS(JQ3)))
     &    ISPIN=0
        ELSE
          INDEXT(NTPOIN)=MESON(IABS(JQ3),IABS(IQ2))
          IF ((JQ3.GT.0).AND.(IABS(JQ3).NE.IABS(IQ2))) INDEXT(NTPOIN)=
     &    -INDEXT(NTPOIN)
          ISPIN=MOD(IABS(INDEXT(NTPOIN)),10)
        ENDIF
        IPPOIN=ICONV(IABS(INDEXT(NTPOIN)))
C--
C--   MASS SMEARING (OPTONAL)
        IF (MSMEAR.EQ.1) THEN
          PTEIL(5,NTPOIN)=BWMASS(IPPOIN)
        ELSE
          PTEIL(5,NTPOIN)=PM(IPPOIN)
        ENDIF
        IF ((PTEIL(5,NTPOIN).GT.PHA(4,I)).AND.(ISPIN.EQ.1)) GOTO 70
        DO 80 K=1,3
   80   PTEIL(K,NTPOIN)=PHA(K,I)
        PTEIL(4,NTPOIN)=SQRT(PTEIL(1,NTPOIN)**2+PTEIL(2,NTPOIN)**2+PTEIL
     &  (3,NTPOIN)**2+PTEIL(5,NTPOIN)**2)
        ITHEL(NTPOIN)=0
C--
C--   REMOVE THIS COMBINATION !
        ILEFT=ILEFT-2
        IF (ILEFT.EQ.0) RETURN
        ICOMB=ICOMB+1
        IREMOV(2*ICOMB-1)=I
        IREMOV(2*ICOMB)=J
   90 CONTINUE
      IF (ILEFT.NE.0) CALL ERRORD(41,' ',FLOAT(ILEFT))
      RETURN
      END
