*
* $Id: gfitkf.F,v 1.1.1.1 1996/03/06 15:36:27 mclareni Exp $
*
* $Log: gfitkf.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:27  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE GFITKF(IVER,IFAFT)
*
*    trak fitting using GEANE and KALMAN FILTER
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
*
      DOUBLE PRECISION VDUM(5),YDUM(5),AA(5,5),XDU55(5,5)
      DOUBLE PRECISION WI(5,5,0:NSTOP)
      DOUBLE PRECISION CC(5,5,0:NSTOP),DDUM(5)
      DOUBLE PRECISION DZERO, DSMALL, DBIG
*
      CHARACTER * 4 CHOPT
*
      PARAMETER (NITER = 2)
*
      DATA H/0.,0.,20./   ! magnetc field (not really needed)
*
      DATA E1S/1./    ! % error in 1/p
      DATA E2S/.3/    ! error in py/px
      DATA E3S/.3/    ! error in pz/px
*
      DATA DZERO,DSMALL,DBIG/0.D0,.1D-8,1.D8/
*_____________________________________________________________________
*
      IFAFT = 0
*
        IF (IDEBUG.GE.1) THEN
          PRINT *,' GFITKL, NPLAN, NITER ',NPLAN,NITER
        ENDIF
*
      CALL VZERO(RD,15)
*
      CHOPT = 'EP'
      IF (IVER.NE.0) CHOPT = 'BEP'
*
**     compute initial coovariance matrix (to be moved in GEAFIT?)
*
      CALL VZERO(CC,50)
      CALL VZERO(WI,50)
      CC(1,1,0) = (E1S*CSIX(1,0))**2
      CC(2,2,0) = E2S**2
      CC(3,3,0) = E3S**2
      CC(4,4,0) = 1.D0/VV(4,4,0)
      CC(5,5,0) = 1.D0/VV(5,5,0)
*
**     NITER  iterations
*
      DO 101 ITER=1,NITER
*
ccc        IF (ITER.NE.1) CALL UCOPY(SS(1,1,0),CC(1,1,0),50)
        CALL UCOPY(CSIX(1,0),CSIY(1,0),12)
*                    chi2
        DO J = 1,5
          DO K=1,5
            CHI2 =
     +       (CSIX(K,0)-CSIZ(K,0))*VV(K,J,0)*
     +       (CSIX(J,0)-CSIZ(J,0))
          ENDDO
        ENDDO
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
**         Load coov matrix at plane jstop-1
*
           k=0
           do i=1,5
           do j=i,5
             k = k+1
             FF = 1.D0
             IF (K.NE.1.AND.K.LE.5) FF = CH
             RC(K) = FF*CC(I,J,JSTOP-1)
           enddo
           enddo
*
           CALL DIRCOS(PC(2),DIF)
           P0 = 1.D0/PD(1)
           PII(1) = P0*DIF(1)
           PII(2) = P0*DIF(2)
           PII(3) = P0*DIF(3)
           CALL EUFILP(1,RC,PLANI(1,1,JSTOP-1),PLANI(1,1,JSTOP))
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
C                 WW(I,I,JSTOP) = DSMALL
C              ENDIF
C            ENDDO
            CALL UCOPY(WW(1,1,JSTOP),WI(1,1,JSTOP),50)
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
*
**        filter
*
          CALL DVADD(25,WW(1,1,JSTOP),WW(2,1,JSTOP),
     +                  VV(1,1,JSTOP),VV(2,1,JSTOP),
     +                  CC(1,1,JSTOP),CC(2,1,JSTOP))
          CALL DSINV(5,CC(1,1,JSTOP),5,IFAIL)
          IF ( IFAIL.NE.0 )  THEN
            IF ( IDEBUG.GE.1 )
     1         PRINT *,'DSINV IFAIL',IFAIL,' AT PLANE ',JSTOP
              CALL VZERO(CC(1,1,JSTOP),50)
          ENDIF
          CALL DMMPY(5,5,VV(1,1,JSTOP),VV(1,2,JSTOP),VV(2,1,JSTOP),
     +                   CSIZ(1,JSTOP),CSIZ(2,JSTOP),
     +                   YDUM(1),      YDUM(2))
          CALL DMMPA(5,5,WW(1,1,JSTOP),WW(1,2,JSTOP),WW(2,1,JSTOP),
     +                   CSIY(1,JSTOP),CSIY(2,JSTOP),
     +                   YDUM(1),      YDUM(2))
          CALL DMMPY(5,5,CC(1,1,JSTOP),CC(1,2,JSTOP),CC(2,1,JSTOP),
     +                   YDUM(1),      YDUM(2),
     +                   CSIX(1,JSTOP),CSIX(2,JSTOP))
          CSIX(6,JSTOP) = CSIY(6,JSTOP)
*                CHI2
          DO J = 1,5
            DO K=1,5
              CHI2 = CHI2 +
     +         (CSIX(K,JSTOP)-CSIZ(K,JSTOP))*VV(K,J,JSTOP)*
     +         (CSIX(J,JSTOP)-CSIZ(J,JSTOP))                 +
     +         (CSIX(K,JSTOP)-CSIY(K,JSTOP))*WW(K,J,JSTOP)*
     +         (CSIX(J,JSTOP)-CSIY(J,JSTOP))
            ENDDO
          ENDDO
*
        ENDDO               !  End plane loop
*
**               smoothing
*
        CALL UCOPY(CC(1,1,NSTOP),SS(1,1,NSTOP),50)
*                chi2
        DO J = 1,5
          DO K=1,5
            CHI2M =
     +         (CSIX(K,NSTOP)-CSIZ(K,NSTOP))*VV(K,J,NSTOP)*
     +         (CSIX(J,NSTOP)-CSIZ(J,NSTOP))
            CHI2T =
     +         (CSIX(K,NSTOP)-CSIY(K,NSTOP))*WW(K,J,NSTOP)*
     +         (CSIX(J,NSTOP)-CSIY(J,NSTOP))
          ENDDO
        ENDDO
*
        DO JS=NSTOP-1,0,-1
          CALL DMMLT(5,5,5,DDT(1,1,JS+1),DDT(1,2,JS+1),DDT(2,1,JS+1),
     1                     WW(1,1,JS+1),WW(1,2,JS+1),WW(2,1,JS+1),
     2                     XDU55(1,1),XDU55(1,2),XDU55(2,1), DDUM )
          CALL DMMLT(5,5,5,CC(1,1,JS),CC(1,2,JS),CC(2,1,JS),
     1                     XDU55(1,1),XDU55(1,2),XDU55(2,1),
     2                     AA(1,1),AA(1,2),AA(2,1), DDUM  )
          CALL DVSUB(25,SS(1,1,JS+1),SS(2,1,JS+1),
     1                  WI(1,1,JS+1),WI(2,1,JS+1),
     2                  XDU55(1,1),XDU55(2,1) )
          CALL DMMLT(5,5,5,XDU55(1,1),XDU55(1,2),XDU55(2,1),
     1                     AA(1,1),AA(2,1),AA(1,2),
     2                     XDU55(1,1),XDU55(1,2),XDU55(2,1), DDUM  )
          CALL UCOPY(CC(1,1,JS),SS(1,1,JS),50)
          CALL DMMLA(5,5,5, AA(1,1),AA(1,2),AA(2,1),
     1                     XDU55(1,1),XDU55(1,2),XDU55(2,1),
     2                     SS(1,1,JS),SS(1,2,JS),SS(2,1,JS) )
          CALL DVSUB(5,CSIX(1,JS+1),CSIX(2,JS+1),
     1                 CSIY(1,JS+1),CSIY(2,JS+1),
     2                 YDUM(1),YDUM(2))
          CALL DMMPA(5,5,AA(1,1),AA(1,2),AA(2,1),
     1                  YDUM(1),YDUM(2),
     2                  CSIX(1,JS),CSIX(2,JS) )
*
          DO I=1,5
            IF (SS(I,I,JS).LE.DZERO) THEN
              IFAFT = 1
              SS(I,I,JS) = DBIG
            ENDIF
          ENDDO
*                    chi2
          DO J = 1,5
            DO K=1,5
              CHI2M = CHI2M +
     +           (CSIX(K,JS)-CSIZ(K,JS))*VV(K,J,JS)*
     +           (CSIX(J,JS)-CSIZ(J,JS))
              CHI2T = CHI2T +
     +           (CSIX(K,JS)-CSIY(K,JS))*WW(K,J,JS)*
     +           (CSIX(J,JS)-CSIY(J,JS))
            ENDDO
          ENDDO
*
        ENDDO
        CHI2N = CHI2T + CHI2M
        print *,' chi2s ',iter,chi2,chi2n,chi2t,chi2m
        IF ( IDEBUG.GE.1 )  THEN
          WRITE(6,'(I6,''     CSIZ '',/(3X,6E15.5))')ITER,CSIZ
          WRITE(6,'(I6,''     CSIX '',/(3X,6E15.5))')ITER,CSIX
          WRITE(6,'(I6,''     CSIY '',/(3X,6E15.5))')ITER,CSIY
        ENDIF
*
          NDOF = 2*nplan-5
          call hfill(1000*iter+1,prob(sngl(chi2),ndof),0.,1.)
          call hfill(1000*iter+2,prob(sngl(chi2n),ndof),0.,1.)
          call hfill(1000*iter+3,prob(sngl(chi2t),ndof),0.,1.)
          call hfill(1000*iter+4,prob(sngl(chi2m),ndof),0.,1.)
        IF ( IDEBUG.GE.1 )  THEN
          PRINT *, ' SS '
          WRITE(6,'(5E15.5)') ((SS(i,i,ip),i=1,5),ip=0,nstop)
        ENDIF
*
*
101   CONTINUE
*
**      done
*
999   CONTINUE
*
      END
