CDECK  ID>, HWDHVY.
*CMZ :-        -26/04/91  12.19.24  by  Federico Carminati
*-- Author :    Ian Knowles & Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWDHVY
C-----------------------------------------------------------------------
C     Performs partonic decays of hadrons containing heavy quark(s):
C     either, meson/baryon spectator model weak decays;
C     or, quarkonia -> 2-gluons, q-qbar, 3-gluons, or 2-gluons + photon.
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWULDO,HWRGEN,XS,XB,EMWSQ,GMWSQ,EMLIM,PW(4),
     & EMTST,X1,X2,X3,TEST
      INTEGER IST(3),I,IHEP,IM,ID,IDQ,IQ,IS,J
      EXTERNAL HWRGEN,HWDWWT,HWDPWT,HWULDO
      DATA IST/113,114,114/
      IF (IERROR.NE.0) RETURN
      DO 100 I=1,NMXQDK
      IF (I.GT.NQDK) RETURN
      IHEP=LOCQ(I)
      IF (ISTHEP(IHEP).EQ.199) GOTO 100
      IM=IMQDK(I)
      IF (NHEP+NPRODS(IM).GT.NMXHEP) CALL HWWARN('HWDHVY',100,*999)
      IF (IDKPRD(4,IM).NE.0) THEN
C Weak decay of meson or baryon
C Idenitify decaying heavy quark and spectator
        ID=IDHW(IHEP)
        IF (ID.EQ.136.OR.ID.EQ.140.OR.ID.EQ.144.OR.
     &      ID.EQ.150.OR.ID.EQ.155.OR.ID.EQ.158.OR.ID.EQ.161.OR.
     &     (ID.EQ.254.AND.IDKPRD(4,IM).EQ.11)) THEN
C c hadron or c decay of B_c+
          IDQ=4
          IQ=NHEP+1
          IS=NHEP+2
        ELSEIF (ID.EQ.171.OR.ID.EQ.175.OR.ID.EQ.179.OR.
     &          ID.EQ.185.OR.ID.EQ.190.OR.ID.EQ.194.OR.ID.EQ.196.OR.
     &         (ID.EQ.230.AND.IDKPRD(4,IM).EQ.5)) THEN
C cbar hadron or cbar decay of B_c-
          IDQ=10
          IS=NHEP+1
          IQ=NHEP+2
        ELSEIF ((ID.GE.221.AND.ID.LE.229).OR.
     &          (ID.EQ.230.AND.IDKPRD(4,IM).EQ.10)) THEN
C b hadron or b decay of B_c-
          IDQ=5
          IQ=NHEP+1
          IS=NHEP+2
        ELSEIF ((ID.GE.245.AND.ID.LE.253).OR.
     &          (ID.EQ.254.AND.IDKPRD(4,IM).EQ.4)) THEN
C bbar hadron or bbar decay of B_c+
          IDQ=11
          IS=NHEP+1
          IQ=NHEP+2
        ELSE
C Decay not recognized
          CALL HWWARN('HWDHVY',100,*999)
        ENDIF
C Label constituents
        IF (NHEP+5.GT.NMXHEP) CALL HWWARN('HWDHVY',101,*999)
        ISTHEP(IHEP)=199
        JDAHEP(1,IHEP)=NHEP+1
        JDAHEP(2,IHEP)=NHEP+2
        IDHW(IQ)=IDQ
        IDHW(IS)=IDKPRD(4,IM)
        IDHEP(IQ)=IDPDG(IDQ)
        IDHEP(IS)=IDPDG(IDKPRD(4,IM))
        ISTHEP(IQ)=155
        ISTHEP(IS)=115
        JMOHEP(1,IQ)=IHEP
        JMOHEP(2,IQ)=IS
        JDAHEP(1,IQ)=NHEP+3
        JDAHEP(2,IQ)=NHEP+5
        JMOHEP(1,IS)=IHEP
        JMOHEP(2,IS)=NHEP+5
        JDAHEP(1,IS)=0
        JDAHEP(2,IS)=NHEP+5
        NHEP=NHEP+2
C and weak decay product jets
        DO 10 J=1,3
        NHEP=NHEP+1
        IDHW(NHEP)=IDKPRD(J,IM)
        IDHEP(NHEP)=IDPDG(IDKPRD(J,IM))
        ISTHEP(NHEP)=IST(J)
        JMOHEP(1,NHEP)=IQ
        JDAHEP(1,NHEP)=0
  10    PHEP(5,NHEP)=RMASS(IDKPRD(J,IM))
        JMOHEP(2,NHEP-2)=NHEP-1
        JDAHEP(2,NHEP-2)=NHEP-1
        JMOHEP(2,NHEP-1)=NHEP-2
        JDAHEP(2,NHEP-1)=NHEP-2
        JMOHEP(2,NHEP  )=IQ
        JDAHEP(2,NHEP  )=IQ
C Share momenta in ratio of masses, preserving specator mass
        XS=RMASS(IDHW(IS))/PHEP(5,IHEP)
        XB=ONE-XS
        CALL HWVSCA(5,XB,PHEP(1,IHEP),PHEP(1,IQ))
        CALL HWVSCA(5,XS,PHEP(1,IHEP),PHEP(1,IS))
        IF (NME(IM).EQ.130) THEN
C Generate decay momenta using full (V-A)*(V-A) matrix element
          EMWSQ=RMASS(198)**2
          GMWSQ=(RMASS(198)*GAMW)**2
          EMLIM=GMWSQ+(EMWSQ-(PHEP(5,IQ)-PHEP(5,NHEP))**2)**2
  20      CALL HWDTHR(PHEP(1,IQ  ),PHEP(1,NHEP-1),
     &                PHEP(1,NHEP-2),PHEP(1,NHEP),HWDWWT)
          CALL HWVSUM(4,PHEP(1,NHEP-2),PHEP(1,NHEP-1),PW)
          EMTST=(HWULDO(PW,PW)-EMWSQ)**2
          IF ((EMTST+GMWSQ)*HWRGEN(0).GT.EMLIM) GOTO 20
        ELSE
C Use phase space
          CALL HWDTHR(PHEP(1,IQ  ),PHEP(1,NHEP-2),
     &                PHEP(1,NHEP-1),PHEP(1,NHEP),HWDPWT)
          CALL HWVSUM(4,PHEP(1,NHEP-2),PHEP(1,NHEP-1),PW)
        ENDIF
C Set up production vertices
        CALL HWVZRO(4,VHEP(1,IQ))
        CALL HWVEQU(4,VHEP(1,IQ),VHEP(1,IS))
        CALL HWVEQU(4,VHEP(1,IQ),VHEP(1,NHEP))
        CALL HWUDKL(198,PW,VHEP(1,NHEP-2))
        CALL HWVSUM(4,VHEP(1,IQ),VHEP(1,NHEP-2),VHEP(1,NHEP-2))
        CALL HWVEQU(4,VHEP(1,NHEP-2),VHEP(1,NHEP-1))
        EMSCA=PHEP(5,IQ)
      ELSE
C Quarkonium decay
C Label products
        ISTHEP(IHEP)=199
        JDAHEP(1,IHEP)=NHEP+1
        DO 30 J=1,NPRODS(IM)
        NHEP=NHEP+1
        IDHW(NHEP)=IDKPRD(J,IM)
        IDHEP(NHEP)=IDPDG(IDKPRD(J,IM))
        ISTHEP(NHEP)=IST(J)
        JMOHEP(1,NHEP)=IHEP
        JDAHEP(1,NHEP)=0
        PHEP(5,NHEP)=RMASS(IDKPRD(J,IM))
  30    CALL HWVZRO(4,VHEP(1,NHEP))
        JDAHEP(2,IHEP)=NHEP
C Establish colour connections and select momentum configuration
        IF (NPRODS(IM).EQ.3) THEN
          IF (IDKPRD(3,IM).EQ.13) THEN
C 3-gluon decay
            JMOHEP(2,NHEP-2)=NHEP
            JMOHEP(2,NHEP-1)=NHEP-2
            JMOHEP(2,NHEP  )=NHEP-1
            JDAHEP(2,NHEP-2)=NHEP-1
            JDAHEP(2,NHEP-1)=NHEP
            JDAHEP(2,NHEP  )=NHEP-2
          ELSE
C or 2-gluon + photon decay
            JMOHEP(2,NHEP-2)=NHEP-1
            JMOHEP(2,NHEP-1)=NHEP-2
            JMOHEP(2,NHEP  )=NHEP
            JDAHEP(2,NHEP-2)=NHEP-1
            JDAHEP(2,NHEP-1)=NHEP-2
            JDAHEP(2,NHEP  )=NHEP
          ENDIF
          IF (NME(IM).EQ.130) THEN
C Use Ore & Powell orthopositronium matrix element
  40        CALL HWDTHR(PHEP(1,IHEP),PHEP(1,NHEP-2),
     &                               PHEP(1,NHEP-1),PHEP(1,NHEP),HWDPWT)
            X1=TWO*HWULDO(PHEP(1,IHEP),PHEP(2,NHEP-2))/PHEP(5,IHEP)**2
            X2=TWO*HWULDO(PHEP(1,IHEP),PHEP(2,NHEP-1))/PHEP(5,IHEP)**2
            X3=TWO-X1-X2
            TEST=((X1*(ONE-X1))**2+(X2*(ONE-X2))**2+(X3*(ONE-X3))**2)
     &          /(X1*X2*X3)**2
            IF (TEST.LT.TWO*HWRGEN(0)) GOTO 40
          ELSE
C Use phase space
            CALL HWDTHR(PHEP(1,IHEP),PHEP(1,NHEP-2),
     &                               PHEP(1,NHEP-1),PHEP(1,NHEP),HWDPWT)
          ENDIF
        ELSE
C Parapositronium 2-gluon or q-qbar decay
          JMOHEP(2,NHEP-1)=NHEP
          JMOHEP(2,NHEP  )=NHEP-1
          JDAHEP(2,NHEP-1)=NHEP
          JDAHEP(2,NHEP  )=NHEP-1
          CALL HWDTWO(PHEP(1,IHEP),PHEP(1,NHEP-1),
     &                             PHEP(1,NHEP),CMMOM(IM),TWO,.FALSE.)
        ENDIF
        EMSCA=PHEP(5,IHEP)
      ENDIF
C Process this new hard scatter
      CALL HWVSUM(4,VHEP(1,3),VTXQDK(1,I),VTXPIP)
      CALL HWBGEN
      CALL HWCFOR
      CALL HWCDEC
      CALL HWDHAD
  100 CONTINUE
  999 END
