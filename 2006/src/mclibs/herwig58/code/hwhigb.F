*
* $Id: hwhigb.F,v 1.1.1.1 1996/03/08 17:02:15 mclareni Exp $
*
* $Log: hwhigb.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:15  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHIGB.
*CMZ :-        -23/08/94  13.22.29  by  Mike Seymour
*-- Author :    Ulrich Baur & Nigel Glover, adapted by Ian Knowles
C------------------------------------------------------------------------
      FUNCTION HWHIGB(NOMASS,S,T,EH2,EQ2)
C     One loop scalar integrals, used in HWHIGJ.
C     If NOMASS=.TRUE. use a small mass approx. for particle in loop.
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      LOGICAL NOMASS
      DOUBLE PRECISION S,T,EQ2,EH2,RAT,COSH,DLS,DLT,DLM,RZ12,DL1,DL2,
     & ST,ROOT,XP,XM
#if !defined(CERNLIB_IBM)
      DOUBLE COMPLEX HWHIGB,HWHIGC,HWHIGD,HWUCI2,HWULI2,EPSI,PII,Z1,Z2
#endif
#if defined(CERNLIB_IBM)
      COMPLEX*16 HWHIGB,HWHIGC,HWHIGD,HWUCI2,HWULI2,EPSI,PII,Z1,Z2
#endif
      EXTERNAL HWULI2,HWUCI2
      COMMON/SMALL/EPSI
C------------------------------------------------------------------------
C     B_0(2p1.p2=S;mq,mq)
C------------------------------------------------------------------------
      PII=CMPLX(ZERO,PIFAC)
      IF (NOMASS) THEN
         RAT=DABS(S/EQ2)
         HWHIGB=-DLOG(RAT)+TWO
         IF (S.GT.ZERO) HWHIGB=HWHIGB+PII
      ELSE
         RAT=S/(FOUR*EQ2)
         IF (S.LT.ZERO) THEN
            HWHIGB=TWO-TWO*DSQRT(ONE-ONE/RAT)
     &                    *DLOG(DSQRT(-RAT)+DSQRT(ONE-RAT))
         ELSEIF (S.GT.ZERO.AND.RAT.LT.ONE) THEN
            HWHIGB=TWO-TWO*DSQRT(ONE/RAT-ONE)*DASIN(DSQRT(RAT))
         ELSEIF (RAT.GT.ONE) THEN
            HWHIGB=TWO-DSQRT(ONE-ONE/RAT)
     &                *(TWO*DLOG(DSQRT(RAT)+DSQRT(RAT-ONE))-PII)
         ENDIF
      ENDIF
      RETURN
C------------------------------------------------------------------------
      ENTRY HWHIGC(NOMASS,S,T,EH2,EQ2)
C     C_0(p{1,2}^2=0,2p1.p2=S;mq,mq,mq)
C------------------------------------------------------------------------
      PII=CMPLX(ZERO,PIFAC)
      IF (NOMASS) THEN
         RAT=DABS(S/EQ2)
         HWHIGC=HALF*DLOG(RAT)**2
         IF (S.GT.ZERO) HWHIGC=HWHIGC-HALF*PIFAC**2-PII*DLOG(RAT)
         HWHIGC=HWHIGC/S
      ELSE
         RAT=S/(FOUR*EQ2)
         IF (S.LT.ZERO) THEN
            HWHIGC=TWO*DLOG(DSQRT(-RAT)+DSQRT(ONE-RAT))**2/S
         ELSEIF (S.GT.ZERO.AND.RAT.LT.ONE) THEN
            HWHIGC=-TWO*(DASIN(DSQRT(RAT)))**2/S
         ELSEIF (RAT.GT.ONE) THEN
            COSH=DLOG(DSQRT(RAT)+DSQRT(RAT-ONE))
            HWHIGC=TWO*(COSH**2-PIFAC**2/FOUR-PII*COSH)/S
         ENDIF
      ENDIF
      RETURN
C------------------------------------------------------------------------
      ENTRY HWHIGD(NOMASS,S,T,EH2,EQ2)
C     D_0(p{1,2,3}^2=0,p4^2=EH2,2p1.p2=S,2p2.p3=T;mq,mq,mq,mq)
C------------------------------------------------------------------------
      PII=CMPLX(ZERO,PIFAC)
      IF (NOMASS) THEN
         DLS=DLOG(DABS(S/EQ2))
         DLT=DLOG(DABS(T/EQ2))
         DLM=DLOG(DABS(EH2/EQ2))
         IF (S.GE.ZERO.AND.T.LE.ZERO) THEN
            DL1=DLOG((EH2-T)/S)
            Z1=T/(T-EH2)
            Z2=(S-EH2)/S
            HWHIGD=DLS**2+DLT**2-DLM**2+DL1**2
     &            +TWO*(DLOG(S/(EH2-T))*DLOG(-T/S)+HWULI2(Z1)-HWULI2(Z2)
     &                 +PII*DLOG(EH2/(EH2-T)))
         ELSEIF (S.LT.ZERO.AND.T.LT.ZERO) THEN
            Z1=(S-EH2)/S
            Z2=(T-EH2)/T
            RZ12=ONE/(Z1*Z2)
            DL1=DLOG((T-EH2)/(S-EH2))
            DL2=DLOG(RZ12)
            HWHIGD=DLS**2+DLT**2-DLM**2+TWO*PIFAC**2/THREE
     &            +TWO*DLOG(S/(T-EH2))*DLOG(ONE/DBLE(REAL(Z2)))
     &            +TWO*DLOG(T/(S-EH2))*DLOG(ONE/DBLE(REAL(Z1)))
     &            -DL1**2-DL2**2-TWO*(HWULI2(Z1)+HWULI2(Z2))
     &            +TWO*PII*DLOG(RZ12**2*EH2/EQ2)
         ENDIF
         HWHIGD=HWHIGD/(S*T)
      ELSE
         ST=S*T
         ROOT=DSQRT(ST**2-FOUR*ST*EQ2*(S+T-EH2))
         XP=HALF*(ST+ROOT)/ST
         XM=1-XP
         HWHIGD=TWO/ROOT*(-HWUCI2(EQ2,S,XP)-HWUCI2(EQ2,T,XP)
     &         +HWUCI2(EQ2,EH2,XP)+DLOG(-XM/XP)
     &         *(LOG(EQ2+EPSI)-LOG(EQ2+EPSI-S*XP*XM)
     &          +LOG(EQ2+EPSI-EH2*XP*XM)-LOG(EQ2+EPSI-T*XP*XM)))
      ENDIF
      RETURN
      END
