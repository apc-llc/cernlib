*
* $Id: sumang.F,v 1.2 1996/02/22 13:32:42 ravndal Exp $
*
* $Log: sumang.F,v $
* Revision 1.2  1996/02/22 13:32:42  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:25  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE SUMANG
*****************************************************************************
*
*    Summary the informations of energy deposits in  10 angle bins
*
*                             called by GUKINE
*****************************************************************************
#include "maphc.inc"
#include "tstcom.inc"
      PARAMETER (MAXL=8)
      DIMENSION COMPF(30,10),COMPZ(30,10),FF(58),FZ(45)
      DIMENSION RP(MAXL,10),RZ(MAXL,10),RBINF(MAXL,10),RBINZ(MAXL,10)
*
      CALL VZERO(COMPF,300)
      CALL VZERO(COMPZ,300)
*
      DO 10 N=1, 10
   10 RNON(N)=RNON(N)/FLOAT(NSHOW(IENERG))
      WRITE(6,10000) RNON
10000 format (' rnon',10f10.5)
*
      DO 100  IIII=1,10
*
*             total energy deposit per layer
         DO 40   JJ = 1 , 30
            DO 20   II = 1 , 58
               COMPF(JJ,IIII)=COMPF(JJ,IIII)+FMAP(II,JJ,IIII)
   20       CONTINUE
            DO 30   IJ = 1 , 45
               COMPZ(JJ,IIII)=COMPZ(JJ,IIII)+ZMAP(IJ,JJ,IIII)
   30       CONTINUE
   40    CONTINUE
*
*  calculate the average width for each layer: contain 95% of the energy in laye
*
         DO 90  L=7,6+MAXL
            MBIN=0
            IF(COMPF(L,IIII).LE.0.) GOTO 60
            CALL UCOPY(FMAP(1,L,IIII),FF(1),58)
            CALL FLPSOR(FF,58)
            CUT = COMPF(L,IIII)*0.95
            SUM = 0.0
            DO 50  KL=1,58
               MBIN=MBIN+1
               SUM=SUM+FF(59-KL)
               IF(SUM.GT.CUT) GOTO 60
   50       CONTINUE
   60       RBINF(L-6,IIII)=MBIN
*
            MBIN=0
            IF(COMPZ(L,IIII).LE.0) GOTO 80
            CALL UCOPY(ZMAP(1,L,IIII),FZ(1),45)
            CALL FLPSOR(FZ,45)
            CUT = COMPZ(L,IIII)*0.95
            SUM=0.0
            DO 70  KL=1,45
               MBIN=MBIN+1
               SUM=SUM+FZ(46-KL)
               IF(SUM.GT.CUT) GOTO 80
   70       CONTINUE
   80       RBINZ(L-6,IIII)=MBIN
   90    CONTINUE
*
  100 CONTINUE
*
      DO 110  L=1,MAXL
         RRP=AMAX1(COMPF(L+6,1),.00000001)/(1.-RNON(1))
         RRZ=AMAX1(COMPZ(L+6,1),.00000001)/(1.-RNON(1))
         RBF=AMAX1(1.0,RBINF(L,1))
         RBZ=AMAX1(1.0,RBINZ(L,1))
         DO 110  J= 1, 10
            RBINF(L,J)=RBINF(L,J)/RBF
            RBINZ(L,J)=RBINZ(L,J)/RBZ
            IF(RNON(J).LT.1.) THEN
               RP(L,J)=COMPF(L+6,J)/RRP/(1.-RNON(J))
               RZ(L,J)=COMPZ(L+6,J)/RRZ/(1.-RNON(J))
            ELSE
               RP(L,J)=1.
               RZ(L,J)=1.
            ENDIF
            IF(RP(L,J).GT.3.0) THEN
               RP(L,J)=3.0
            ENDIF
            IF(RZ(L,J).GT.3.0) THEN
               RZ(L,J)=3.0
            ENDIF
            IF(RBINF(L,J).LE.0.0) THEN
               RBINF(L,J)=1.0
            ELSEIF(RBINF(L,J).GT.3.0) THEN
               RBINF(L,J)=3.0
            ENDIF
            IF(RBINZ(L,J).LE.0.0) THEN
               RBINZ(L,J)=1.0
            ELSEIF(RBINZ(L,J).GT.3.0) THEN
               RBINZ(L,J)=3.0
            ENDIF
  110 CONTINUE
*
*    write the angle corelation and the zero degree file
*
      WRITE(21) KEVN,RNON,NPZ,RP,RZ,RBINF,RBINZ
      WRITE(21) (KP(I),I=1,KEVN)
      WRITE(21) (IBUF(I),I=1,NP1)
*
      PRINT *,'summary for one energy'
      WRITE(6,10100) ((COMPF(L+6,J),J=1,10),L=1,MAXL)
      WRITE(6,10100) ((COMPZ(L+6,J),J=1,10),L=1,MAXL)
10100 FORMAT('         SUM PER LAYER',10(/,10F10.7))
      WRITE(6,10200) ((RP(L,J),J=1,10),L=1,MAXL)
      WRITE(6,10200) ((RZ(L,J),J=1,10),L=1,MAXL)
      WRITE(6,10200) ((RBINF(L,J),J=1,10),L=1,MAXL)
      WRITE(6,10200) ((RBINZ(L,J),J=1,10),L=1,MAXL)
10200 FORMAT('         RATIO',10(/,10F10.7))
*
      CALL VZERO(FMAP,17400)
      CALL VZERO(ZMAP,13500)
      CALL VZERO(RNON,10)
      KEVN = 0
*                                  end of SUMANG
      END
