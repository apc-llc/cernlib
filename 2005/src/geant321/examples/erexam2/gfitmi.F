*
* $Id: gfitmi.F,v 1.1.1.1 1996/03/06 15:36:26 mclareni Exp $
*
* $Log: gfitmi.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:26  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE GFITMI(IVER,IFAFT)
*
*    trak fitting using GEANE and matrix inversion
*
*    IVER = 0  forward  tracking
*           1  backward tracking
*
*    IFAFT = 0  Fit successful
*    IFAFT = 1  Error matrix singular
*    IFAFT = 9  Fit not successful
*
*    Ask V.Innocente for documentation
*
#include "geant321/gcflag.inc"
#include "geant321/ertrio.inc"
*
*
      PARAMETER (NPLAN = 8, NSTOP = NPLAN-1, NSTOP5 = 5*NPLAN )
      COMMON /GCFIT/
     +          CSIX(6,0:NSTOP),CSIY(6,0:NSTOP),CSIZ(6,0:NSTOP)
     +         ,WW(5,5,0:NSTOP),DDT(5,5,0:NSTOP)
     +         ,VV(5,5,0:NSTOP),SS(5,5,0:NSTOP)
     +         ,CHI2,CHI2N,CHI2T,CHI2M
     +         ,PLANI(3,4,0:NSTOP)
      DOUBLE PRECISION
     +          CSIX           ,CSIY           ,CSIZ
     +         ,WW             ,DDT
     +         ,VV             ,SS
     +         ,CHI2,CHI2N,CHI2T,CHI2M
*
*     CSIZ            measured values
*     CSIY            predicted values
*     CSIX            fitted  values
*
*     VV              Weight on CSIZ
*     WW              tracking Weight
*     SS              fit covariance
*
*     DDT             trasport matrix
*
*     CHI2            first chi2
*     CHI2N           estimated new chi2
*     CHI2T           estimated new chi2 due to tracking
*     CHI2M           estimated new chi2 due to measurements
*
*
      REAL PD(3),RD(15),PC(3),RC(15),H(3),XXI(3),XXO(3),PII(3),POO(3)
      REAL DIF(3)
      INTEGER LSTER(2,150)
      DOUBLE PRECISION AMA(NSTOP5,NSTOP5),SOL(NSTOP5)
      DOUBLE PRECISION XDU55(5,5),DTR(5,5),WDT(5,5)
     +                ,VDUM(5),VDUM0(5),YDUM(5)
     +                ,ERRA(150),FF,DDUM(5)
      DOUBLE PRECISION DZERO, DSMALL, DBIG
*
      CHARACTER * 4 CHOPT
*
      PARAMETER (NITER = 2)
*
      DATA H/0.,0.,20./
*
      DATA DZERO,DSMALL,DBIG/0.D0,.1D-8,1.D8/
*_____________________________________________________________________
*
      IFAFT = 0
*
*         Define coordinate for which we want the errors
        K = 1
        DO 1 I=1,5
        DO 1 J=I,5
          LSTER(1,K) = I
          LSTER(2,K) = J
          K = K+1
1       CONTINUE
        DO 2 I=6,NPLAN*5
        DO 2 J=I,5*((I-1)/5)+5
          LSTER(1,K) = I
          LSTER(2,K) = J
          K = K+1
2       CONTINUE
        NERRA = K-1
*
        IF (IDEBUG.GE.1) THEN
          PRINT *,' GFITMI, NPLAN, NERRA, NITER ',NPLAN,NERRA,NITER
        ENDIF
*
      CALL VZERO(RD,15)
*
      CHOPT = 'EP'
      IF (IVER.NE.0) CHOPT = 'BEP'
*
**     NITER  iterations
*
      DO 101 ITER=1,NITER
*
        CALL UCOPY(CSIX(1,0),CSIY(1,0),12)
*            loop over inner planes
        DO JSTOP = 1,NSTOP
*            load starting point
           PD(1) = ABS(CSIX(1,JSTOP-1))
           PD(2) = CSIX(2,JSTOP-1)
           PD(3) = CSIX(3,JSTOP-1)
           XXI(1) = PLANI(1,1,JSTOP-1)*CSIX(4,JSTOP-1)
     +            + PLANI(1,2,JSTOP-1)*CSIX(5,JSTOP-1)
     +            + PLANI(1,4,JSTOP-1)*CSIX(6,JSTOP-1)
           XXI(2) = PLANI(2,1,JSTOP-1)*CSIX(4,JSTOP-1)
     +            + PLANI(2,2,JSTOP-1)*CSIX(5,JSTOP-1)
     +            + PLANI(2,4,JSTOP-1)*CSIX(6,JSTOP-1)
           XXI(3) = PLANI(3,1,JSTOP-1)*CSIX(4,JSTOP-1)
     +            + PLANI(3,2,JSTOP-1)*CSIX(5,JSTOP-1)
     +            + PLANI(3,4,JSTOP-1)*CSIX(6,JSTOP-1)
*
           CH = SIGN(1.D0,CSIX(1,JSTOP-1))
           ITYP = 5
           IF ( CH.LT.0)  ITYP = 6
           SPU = 1.
           CALL TRSDSC(PD,RD,PC,RC,H,CH,IERR,SPU,
     +                 PLANI(1,1,JSTOP-1),PLANI(1,2,JSTOP-1))
*
           CALL DIRCOS(PC(2),DIF)
           P0 = 1.D0/PD(1)
           PII(1) = P0*DIF(1)
           PII(2) = P0*DIF(2)
           PII(3) = P0*DIF(3)
           CALL EUFILP(1,RD,PLANI(1,1,JSTOP-1),PLANI(1,1,JSTOP))
*                   track
           CALL ERTRAK(XXI,PII,XXO,POO,ITYP,CHOPT)
           IF (IEPRED(1).NE.1) THEN
             print *, 'plane ',JSTOP,' not reached '
             IFAFT = 9
             GO TO 999
           ENDIF
*
*
**            load arriving point
*
           CSIY(1,JSTOP) = CH*ERPOUT(1,1)
           CSIY(2,JSTOP) = ERPOUT(2,1)
           CSIY(3,JSTOP) = ERPOUT(3,1)
           CSIY(4,JSTOP) =
     +         PLANI(1,1,JSTOP)*XXO(1)+PLANI(2,1,JSTOP)*XXO(2)+
     +         PLANI(3,1,JSTOP)*XXO(3)
           CSIY(5,JSTOP) =
     +         PLANI(1,2,JSTOP)*XXO(1)+PLANI(2,2,JSTOP)*XXO(2)+
     +         PLANI(3,2,JSTOP)*XXO(3)
           CSIY(6,JSTOP) =
     +         PLANI(1,4,JSTOP)*XXO(1)+PLANI(2,4,JSTOP)*XXO(2)+
     +         PLANI(3,4,JSTOP)*XXO(3)
           IF (ITER.EQ.1) CALL UCOPY(CSIY(1,JSTOP),CSIX(1,JSTOP),12)
           K=0
           DO I=1,5
           DO J=I,5
             K=K+1
             FF = 1.D0
             IF (K.NE.1.AND.K.LE.5) FF = CH
             WW(I,J,JSTOP) = FF*ERROUT(K,1)
             WW(J,I,JSTOP) = WW(I,J,JSTOP)
           ENDDO
           ENDDO
C            DO I=1,5
C              IF( WW(I,I,JSTOP).LE.DZERO )  THEN
C                 WW(I,I,JSTOP) = SMALL
C              ENDIF
C            ENDDO
            CALL DSINV(5,WW(1,1,JSTOP),5,IFAIL)
            IF ( IFAIL.NE.0 )  THEN
              IF ( IDEBUG.GE.1 )
     1           PRINT *,'DSINV IFAIL',IFAIL,' AT PLANE ',JSTOP
                CALL VZERO(WW(1,1,JSTOP),50)
            ENDIF
*
           DO I=1,5
           DO J=1,5
             FF = 1.D0
             IF (I.EQ.1 .AND. J.NE.1 )  FF = CH
             IF (J.EQ.1 .AND. I.NE.1 )  FF = CH
             DDT(I,J,JSTOP) = FF*ERDTRP(J,I,1)
           ENDDO
           ENDDO
        ENDDO               !  End plane loop
        IF ( IDEBUG.GE.1 )  THEN
          WRITE(6,'(I6,''     CSIZ '',/(3X,6E15.5))')ITER,CSIZ
          WRITE(6,'(I6,''     CSIX '',/(3X,6E15.5))')ITER,CSIX
          WRITE(6,'(I6,''     CSIY '',/(3X,6E15.5))')ITER,CSIY
        ENDIF
*
**     Solve now
*
        ISOL = 0
150     CONTINUE
        ISOL = ISOL + 1
*
        CALL VZERO(AMA,2*NSTOP5*NSTOP5)
        CALL VZERO(SOL,2*NSTOP5)
        CHI2 = 0.D0
        DO 151 J=1,5
151      VDUM(J) = CSIY(J,0) - CSIX(J,0)
        DO 120 I=0,NSTOP
          CALL UCOPY(VDUM,VDUM0,10)
          IF ( I.NE.NSTOP ) THEN
            CALL DMMLT(5,5,5,WW(1,1,I+1),WW(1,2,I+1),WW(2,1,I+1),
     1                   DDT(1,1,I+1),DDT(2,1,I+1),DDT(1,2,I+1),
     2                   WDT(1,1),WDT(1,2),WDT(2,1), DDUM )
            CALL DMMLT(5,5,5,DDT(1,1,I+1),DDT(1,2,I+1),DDT(2,1,I+1),
     1                   WDT(1,1),WDT(1,2),WDT(2,1),
     2                   XDU55(1,1),XDU55(1,2),XDU55(2,1), DDUM )
*
            DO 152 J=1,5
              VDUM(J) = CSIY(J,I+1) - CSIX(J,I+1)
152         CONTINUE
          ELSE
            CALL VZERO(XDU55,50)
            CALL VZERO(VDUM,10)
          ENDIF
          DO 153 J=1,5
            YDUM(J) = CSIZ(J,I) - CSIX(J,I)
153       CONTINUE
          II = 5*I
          DO 120 J=1,5
            IJ = II+J
            DO 121 K=1,5
              IK = II+K
              AMA(IJ,IK) = WW(K,J,I) + VV(K,J,I)  + XDU55(K,J)
              IF ( I.NE.NSTOP) THEN
                AMA(IJ,IK+5) = -WW(K,1,I+1)*DDT(J,1,I+1)
     2                         -WW(K,2,I+1)*DDT(J,2,I+1)
     3                         -WW(K,3,I+1)*DDT(J,3,I+1)
     4                         -WW(K,4,I+1)*DDT(J,4,I+1)
     5                         -WW(K,5,I+1)*DDT(J,5,I+1)
              ENDIF
              IF ( I.NE.0 ) THEN
                AMA(IJ,IK-5) = -WW(J,1,I)*DDT(K,1,I)
     2                         -WW(J,2,I)*DDT(K,2,I)
     3                         -WW(J,3,I)*DDT(K,3,I)
     4                         -WW(J,4,I)*DDT(K,4,I)
     5                         -WW(J,5,I)*DDT(K,5,I)
              ENDIF
              CHI2 = CHI2
     1         + VDUM0(J)*WW(J,K,I)*VDUM0(K)
     2         +  YDUM(J)*VV(J,K,I)*YDUM(K)
121         CONTINUE
*
            SOL(IJ)=
     1          VDUM0(1)*WW(1,J,I)+VDUM0(2)*WW(2,J,I)
     2         +VDUM0(3)*WW(3,J,I)+VDUM0(4)*WW(4,J,I)+VDUM0(5)*WW(5,J,I)
     3         +YDUM(1)*VV(1,J,I)+YDUM(2)*VV(2,J,I)
     4         +YDUM(3)*VV(3,J,I)+YDUM(4)*VV(4,J,I)+YDUM(5)*VV(5,J,I)
             IF (I.NE.NSTOP) SOL(IJ) = SOL(IJ)
     5         - VDUM(1)*WDT(1,J)-VDUM(2)*WDT(2,J)
     6         - VDUM(3)*WDT(3,J)-VDUM(4)*WDT(4,J)-VDUM(5)*WDT(5,J)
*
120     CONTINUE
*            "home made" band code
*           ( can be optimized )
        MBAN = 9
        CALL DSBEQN(NSTOP5,MBAN,AMA,NSTOP5,IFAIL,1,SOL)
        IF ( IFAIL.NE.0 ) THEN
*
**      inversion failed!!
*
          IF ( IDEBUG.GE.1 )
     1       PRINT *,' DSBEQN IFAIL ',IFAIL
CC          DO I=1,NSTOP5
CC            IF ( AMA(I,I).LE.DZERO ) THEN
CC              IF ( IDEBUG.GE.1 )   THEN
CC                PRINT *,' AMA matrix singular!!'
CC                PRINT *,' element ',I,' = ',AMA(I,I)
CC              ENDIF
CC            ENDIF
CC          ENDDO
          IFAFT = 9
          GO TO 999
        ENDIF
*
        CALL DSBFINV(NSTOP5,MBAN,AMA,NSTOP5,NERRA,ERRA,LSTER)
*
*      compute new fit values  and chi2s
        CHI2T = 0.D0
        CHI2M = 0.D0
        DO I = 0,NSTOP
          II = 5*I
          DO J=1,5
            IJ= II+J
            CSIX(J,I) = CSIX(J,I) + SOL(IJ)
          ENDDO
          IF (I.NE.0) THEN
             CALL UCOPY(CSIY(1,I),VDUM,10)
             DO J=1,5
               IJ= II+J -5
               DO K=1,5
                 VDUM(K) = VDUM(K) + DDT(J,K,I)*SOL(IJ)
               ENDDO
             ENDDO
          ENDIF
          DO J = 1,5
            DO K=1,5
              CHI2M = CHI2M +
     +         (CSIX(K,I)-CSIZ(K,I))*VV(K,J,I)*(CSIX(J,I)-CSIZ(J,I))
            ENDDO
            IF (I.NE.0) THEN
              DO K=1,5
                CHI2T = CHI2T +
     +           (CSIX(K,I)-VDUM(K))*WW(K,J,I)*(CSIX(J,I)-VDUM(J))
              ENDDO
            ENDIF
          ENDDO
        ENDDO
        CHI2N = CHI2T + CHI2M
          print *,' chi2s ',iter,chi2,chi2n,chi2t,chi2m
*
          NDOF = 2*nplan-5
          call hfill(1000*iter+1,prob(sngl(chi2),ndof),0.,1.)
          call hfill(1000*iter+2,prob(sngl(chi2n),ndof),0.,1.)
          call hfill(1000*iter+3,prob(sngl(chi2t),ndof),0.,1.)
          call hfill(1000*iter+4,prob(sngl(chi2m),ndof),0.,1.)
        IF ( IDEBUG.GE.1 )  THEN
          PRINT *, ' ERRA '
          WRITE(6,'(5E15.5)') (ERRA(IE),IE=1,NERRA)
        ENDIF
*
*
101   CONTINUE
*
**      done
*
*       load final coovariance
*
      call vzero(ss,50*nplan)
      do ie=1,nerra
       iplan = (lster(1,ie)-1)/5
       jplan = (lster(2,ie)-1)/5
       if (iplan.eq.jplan) then
         i = lster(1,ie) - 5*iplan
         j = lster(2,ie) - 5*jplan
         ss(i,j,iplan) = erra(ie)
         ss(j,i,iplan) = erra(ie)
         if (i.eq.j.and.erra(ie).le.DZERO) then
           ss(i,j,iplan) = DBIG
           ss(j,i,iplan) = DBIG
           IFAFT = 1
         endif
       endif
      enddo
*
999   CONTINUE
*
      END
