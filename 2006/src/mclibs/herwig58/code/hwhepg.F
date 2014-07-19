*
* $Id: hwhepg.F,v 1.1.1.1 1996/03/08 17:02:14 mclareni Exp $
*
* $Log: hwhepg.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:14  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHEPG.
*CMZ :-        -02/05/91  10.57.27  by  Federico Carminati
*-- Author :    Bryan Webber and Ian Knowles
C------------------------------------------------------------------------
      SUBROUTINE HWHEPG
C------------------------------------------------------------------------
C     (Initially polarised) e+e- --> qqbar g, with parton thrust < THMAX,
C     equivalent to: maximum parton energy < THMAX*EMSCA/2; or a JADE, E0
c     scheme, y_cut=1.-THMAX.
C     If flavour specified mass effects fully included.
C     EVWGT=sig(e^+e^- --> qqbar g) in nb
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER ID1,IQ,I,IQ1
      LOGICAL MASS
      DOUBLE PRECISION HWRGEN,HWUALF,HWUAEM,HWULDO,HWDPWT,Q2NOW,Q2LST,
     & PHASP,QGMAX,QGMIN,FACTR,QM2,CLF(7),ORDER,PRAN,PQWT,QQG,QBG,SUM,
     & RUT,QQLM,QQLP,QBLM,QBLP,DYN1,DYN2,DYN3,DYN4,DYN5,DYN6
      EXTERNAL HWDPWT
      SAVE Q2NOW,Q2LST,QGMAX,QGMIN,FACTR,ORDER,ID1,MASS,QM2,CLF,IQ1,
     & QQG,QBG,SUM
      DATA Q2LST/0./
      IF (GENEV) THEN
C Label produced partons and calculate gluon spin
        IDHW(4)=200
        IDHW(5)=IQ1
        IDHW(6)=13
        IDHW(7)=IQ1+6
        IDHEP(4)=23
        IDHEP(5)=IQ1
        IDHEP(6)=21
        IDHEP(7)=-IQ1
        ISTHEP(4)=110
        ISTHEP(5)=113
        ISTHEP(6)=114
        ISTHEP(7)=114
        JMOHEP(1,4)=1
        JMOHEP(2,4)=2
        JMOHEP(1,5)=4
        JMOHEP(2,5)=6
        JMOHEP(1,6)=4
        JMOHEP(2,6)=7
        JMOHEP(1,7)=4
        JMOHEP(2,7)=5
        JDAHEP(1,4)=5
        JDAHEP(2,4)=7
        JDAHEP(1,5)=0
        JDAHEP(2,5)=7
        JDAHEP(1,6)=0
        JDAHEP(2,6)=5
        JDAHEP(1,7)=0
        JDAHEP(2,7)=6
        NHEP=7
        IF (AZSPIN) THEN
C  Calculate the transverse polarisation of the gluon
C  Correlation with leptons presently neglected
           GPOLN=(QQG**2+QBG**2)/((Q2NOW-2.*SUM)*Q2NOW)
           GPOLN=2./(2.+GPOLN)
        ENDIF
      ELSE
        Q2NOW=PHEP(5,3)**2
        IF (Q2NOW.NE.Q2LST) THEN
          EMSCA=PHEP(5,3)
          Q2LST=Q2NOW
          PHASP=3.*THMAX-2.
          IF (PHASP.LE.0.) CALL HWWARN('HWHEPG',400,*999)
          QGMAX=.5*Q2NOW*THMAX
          QGMIN=.5*Q2NOW*(1.-THMAX)
          FACTR=GEV2NB*FLOAT(NCOLO)*CFFAC*HWUALF(1,EMSCA)
     &         *.5*(HWUAEM(Q2NOW)*PHASP)**2/Q2NOW
          CALL HWVEQU(5,PHEP(1,3),PHEP(1,4))
          ORDER=1.
          IF (IDHW(1).GT.IDHW(2)) ORDER=-ORDER
          ID1=MOD(IPROC,10)
          IF (ID1.NE.0) THEN
             MASS=.TRUE.
             QM2=RMASS(ID1)**2
             CALL HWUCFF(11,ID1,Q2NOW,CLF(1))
             FACTR=FACTR*CLF(1)
          ELSE
             MASS=.FALSE.
             CALL HWUEEC(1)
             FACTR=FACTR*TQWT
          ENDIF
        ENDIF
        IF (ID1.EQ.0) THEN
C Select quark flavour
          PRAN=TQWT*HWRGEN(0)
          PQWT=0.
          DO 10 IQ=1,MAXFL
          PQWT=PQWT+CLQ(1,IQ)
          IF (PQWT.GT.PRAN) GOTO 11
   10     CONTINUE
          IQ=MAXFL
   11     IQ1=MAPQ(IQ)
          DO 20 I=1,7
   20     CLF(I)=CLQ(I,IQ)
        ELSE
          IQ1=ID1
        ENDIF
C Select final state momentum configuration
        PHEP(5,5)=RMASS(IQ1)
        PHEP(5,6)=RMASS(13)
        PHEP(5,7)=RMASS(IQ1)
   30   CALL HWDTHR(PHEP(1,4),PHEP(1,5),PHEP(1,6),PHEP(1,7),HWDPWT)
        QQG=HWULDO(PHEP(1,5),PHEP(1,6))
        IF (QQG.LT.QGMIN) GOTO 30
        QBG=HWULDO(PHEP(1,7),PHEP(1,6))
        SUM=QQG+QBG
        IF (QBG.LT.QGMIN.OR.SUM.GT.QGMAX) GOTO 30
        QQLM=HWULDO(PHEP(1,5),PHEP(1,1))
        QQLP=HWULDO(PHEP(1,5),PHEP(1,2))
        QBLM=HWULDO(PHEP(1,7),PHEP(1,1))
        QBLP=HWULDO(PHEP(1,7),PHEP(1,2))
        DYN1=QQLM**2+QQLP**2+QBLM**2+QBLP**2
        DYN2=0.
        DYN3=DYN1-2.*(QQLM**2+QBLP**2)
        IF (MASS) THEN
           RUT=1./QQG+1./QBG
           DYN1=DYN1+8.*QM2*(1.-.25*Q2NOW*RUT
     &         +QQLM*QQLP/(Q2NOW*QBG)+QBLM*QBLP/(Q2NOW*QQG))
           DYN2=QM2*(Q2NOW-SUM*(2.+QM2*RUT)
     &         -4.*HWULDO(PHEP(1,6),PHEP(1,1))
     &            *HWULDO(PHEP(1,6),PHEP(1,2))/Q2NOW)
           DYN3=DYN3+QM2*2.*RUT*(QBG*(QBLP-QBLM)-QQG*(QQLP-QQLM))
        ENDIF
        EVWGT=CLF(1)*DYN1+CLF(2)*DYN2+ORDER*CLF(3)*DYN3
        IF (TPOL) THEN
C Include event plane azimuthal angle
           DYN4=.5*Q2NOW
           DYN5=DYN4
           DYN6=0.
           IF (MASS) THEN
              DYN4=DYN4-QM2*SUM/QBG
              DYN5=DYN5-QM2*SUM/QQG
              DYN6=QM2
           ENDIF
           EVWGT=EVWGT
     &     +(CLF(4)*COSS-CLF(6)*SINS)*(DYN4*(PHEP(1,5)**2-PHEP(2,5)**2)
     &                                +DYN5*(PHEP(1,7)**2-PHEP(2,7)**2))
     &     +(CLF(4)*SINS+CLF(6)*COSS)*2.*(DYN4*PHEP(1,5)*PHEP(2,5)
     &                                   +DYN5*PHEP(1,7)*PHEP(2,7))
     &     +(CLF(5)*COSS-CLF(7)*SINS)*DYN6*(PHEP(1,6)**2-PHEP(2,6)**2)
     &     +(CLF(5)*SINS+CLF(7)*COSS)*DYN6*2.*PHEP(1,6)*PHEP(2,6)
        ENDIF
C Assign event weight
        EVWGT=EVWGT*FACTR/(QQG*QBG*CLF(1))
      ENDIF
  999 END
