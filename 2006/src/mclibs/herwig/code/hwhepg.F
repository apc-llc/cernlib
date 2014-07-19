CDECK  ID>, HWHEPG.
*CMZ :-        -02/05/91  10.57.27  by  Federico Carminati
*-- Author :    Bryan Webber and Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWHEPG
C-----------------------------------------------------------------------
C     (Initially polarised) e-e+ --> qqbar g with parton thrust < THMAX,
C     equivalent to: maximum parton energy < THMAX*EMSCA/2; or a JADE E0
c     scheme, y_cut=1.-THMAX.
C     If flavour specified mass effects fully included.
C     EVWGT=sig(e^-e^+ --> qqbar g) in nb
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWUALF,HWUAEM,HWULDO,HWDPWT,Q2NOW,Q2LST,
     & PHASP,QGMAX,QGMIN,FACTR,QM2,CLF(7),ORDER,PRAN,PQWT,QQG,QBG,SUM,
     & RUT,QQLM,QQLP,QBLM,QBLP,DYN1,DYN2,DYN3,DYN4,DYN5,DYN6,XQ2,X2SUM,
     & PVRT(4)
      INTEGER ID1,IQ,I,LM,LP,IQ1
      LOGICAL MASS
      EXTERNAL HWRGEN,HWUALF,HWUAEM,HWULDO,HWDPWT
      SAVE Q2NOW,Q2LST,QGMAX,QGMIN,FACTR,ORDER,ID1,MASS,QM2,CLF,LM,LP,
     & IQ1,QQG,QBG,SUM
      DATA Q2LST/0./
      IF (GENEV) THEN
C Label produced partons and calculate gluon spin
        IDHW(NHEP+1)=200
        IDHW(NHEP+2)=IQ1
        IDHW(NHEP+3)=13
        IDHW(NHEP+4)=IQ1+6
        IDHEP(NHEP+1)=23
        IDHEP(NHEP+2)=IQ1
        IDHEP(NHEP+3)=21
        IDHEP(NHEP+4)=-IQ1
        ISTHEP(NHEP+1)=110
        ISTHEP(NHEP+2)=113
        ISTHEP(NHEP+3)=114
        ISTHEP(NHEP+4)=114
        JMOHEP(1,NHEP+1)=LM
        JMOHEP(2,NHEP+1)=LP
        JMOHEP(1,NHEP+2)=NHEP+1
        JMOHEP(2,NHEP+2)=NHEP+3
        JMOHEP(1,NHEP+3)=NHEP+1
        JMOHEP(2,NHEP+3)=NHEP+4
        JMOHEP(1,NHEP+4)=NHEP+1
        JMOHEP(2,NHEP+4)=NHEP+2
        JDAHEP(1,NHEP+1)=NHEP+2
        JDAHEP(2,NHEP+1)=NHEP+4
        JDAHEP(1,NHEP+2)=0
        JDAHEP(2,NHEP+2)=NHEP+4
        JDAHEP(1,NHEP+3)=0
        JDAHEP(2,NHEP+3)=NHEP+2
        JDAHEP(1,NHEP+4)=0
        JDAHEP(2,NHEP+4)=NHEP+3
C Decide which quark radiated and assign production vertices
        XQ2=(Q2NOW-2.*QBG)**2
        X2SUM=XQ2+(Q2NOW-2.*QQG)**2
        IF (XQ2.LT.HWRGEN(0)*X2SUM) THEN
C Quark radiated the gluon
          CALL HWVZRO(4,VHEP(1,NHEP+4))
          CALL HWVSUM(4,PHEP(1,NHEP+2),PHEP(1,NHEP+3),PVRT)
          CALL HWUDKL(IQ1,PVRT,VHEP(1,NHEP+3))
          CALL HWVEQU(4,VHEP(1,NHEP+3),VHEP(1,NHEP+2))
        ELSE
C Anti-quark radiated the gluon
          CALL HWVZRO(4,VHEP(1,NHEP+2))
          CALL HWVSUM(4,PHEP(1,NHEP+4),PHEP(1,NHEP+3),PVRT)
          CALL HWUDKL(IQ1,PVRT,VHEP(1,NHEP+3))
          CALL HWVEQU(4,VHEP(1,NHEP+3),VHEP(1,NHEP+4))
        ENDIF
        IF (AZSPIN) THEN
C  Calculate the transverse polarisation of the gluon
C  Correlation with leptons presently neglected
           GPOLN=(QQG**2+QBG**2)/((Q2NOW-2.*SUM)*Q2NOW)
           GPOLN=2./(2.+GPOLN)
        ENDIF
        NHEP=NHEP+4
      ELSE
        EMSCA=PHEP(5,3)
        Q2NOW=EMSCA**2
        IF (Q2NOW.NE.Q2LST) THEN
          Q2LST=Q2NOW
          PHASP=3.*THMAX-2.
          IF (PHASP.LE.0.) CALL HWWARN('HWHEPG',400,*999)
          QGMAX=.5*Q2NOW*THMAX
          QGMIN=.5*Q2NOW*(1.-THMAX)
          FACTR=GEV2NB*FLOAT(NCOLO)*CFFAC*HWUALF(1,EMSCA)
     &         *.5*(HWUAEM(Q2NOW)*PHASP)**2/Q2NOW
          LM=1
          IF (JDAHEP(1,LM).NE.0) LM=JDAHEP(1,LM)
          LP=2
          IF (JDAHEP(1,LP).NE.0) LP=JDAHEP(1,LP)
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
          PRAN=TQWT*HWRGEN(1)
          PQWT=0.
          DO 10 IQ=1,MAXFL
          PQWT=PQWT+CLQ(1,IQ)
          IF (PQWT.GT.PRAN) GOTO 11
   10     CONTINUE
          IQ=MAXFL
   11     IQ1=MAPQ(IQ)
          DO 20 I=1,7
   20     CLF(I)=CLQ(I,IQ)
        ELSEIF (Q2NOW.GT.4*QM2/(2*THMAX-1)) THEN
          IQ1=ID1
        ELSE
          EVWGT=0.
          RETURN
        ENDIF
C Select final state momentum configuration
        CALL HWVEQU(5,PHEP(1,3),PHEP(1,NHEP+1))
        PHEP(5,NHEP+2)=RMASS(IQ1)
        PHEP(5,NHEP+3)=RMASS(13)
        PHEP(5,NHEP+4)=RMASS(IQ1)
   30   CALL HWDTHR(PHEP(1,NHEP+1),PHEP(1,NHEP+2),
     &              PHEP(1,NHEP+3),PHEP(1,NHEP+4),HWDPWT)
        QQG=HWULDO(PHEP(1,NHEP+2),PHEP(1,NHEP+3))
        IF (QQG.LT.QGMIN) GOTO 30
        QBG=HWULDO(PHEP(1,NHEP+4),PHEP(1,NHEP+3))
        SUM=QQG+QBG
        IF (QBG.LT.QGMIN.OR.SUM.GT.QGMAX) GOTO 30
        QQLM=HWULDO(PHEP(1,NHEP+2),PHEP(1,LM))
        QQLP=HWULDO(PHEP(1,NHEP+2),PHEP(1,LP))
        QBLM=HWULDO(PHEP(1,NHEP+4),PHEP(1,LM))
        QBLP=HWULDO(PHEP(1,NHEP+4),PHEP(1,LP))
        DYN1=QQLM**2+QQLP**2+QBLM**2+QBLP**2
        DYN2=0.
        DYN3=DYN1-2.*(QQLM**2+QBLP**2)
        IF (MASS) THEN
           RUT=1./QQG+1./QBG
           DYN1=DYN1+8.*QM2*(1.-.25*Q2NOW*RUT
     &         +QQLM*QQLP/(Q2NOW*QBG)+QBLM*QBLP/(Q2NOW*QQG))
           DYN2=QM2*(Q2NOW-SUM*(2.+QM2*RUT)
     &         -4.*HWULDO(PHEP(1,NHEP+3),PHEP(1,LM))
     &            *HWULDO(PHEP(1,NHEP+3),PHEP(1,LP))/Q2NOW)
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
     &     +(CLF(4)*COSS-CLF(6)*SINS)
     &      *(DYN4*(PHEP(1,NHEP+2)**2-PHEP(2,NHEP+2)**2)
     &       +DYN5*(PHEP(1,NHEP+4)**2-PHEP(2,NHEP+4)**2))
     &     +(CLF(4)*SINS+CLF(6)*COSS)*2.
     &      *(DYN4*PHEP(1,NHEP+2)*PHEP(2,NHEP+2)
     &       +DYN5*PHEP(1,NHEP+4)*PHEP(2,NHEP+4))
     &     +(CLF(5)*COSS-CLF(7)*SINS)*DYN6
     &      *(PHEP(1,NHEP+3)**2-PHEP(2,NHEP+3)**2)
     &     +(CLF(5)*SINS+CLF(7)*COSS)*DYN6*2.
     &      *PHEP(1,NHEP+3)*PHEP(2,NHEP+3)
        ENDIF
C Assign event weight
        EVWGT=EVWGT*FACTR/(QQG*QBG*CLF(1))
      ENDIF
  999 END
