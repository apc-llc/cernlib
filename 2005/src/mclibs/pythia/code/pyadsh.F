 
C*********************************************************************
 
C...PYADSH
C...Administers the generation of successive final-state showers
C...in external processes.
 
      SUBROUTINE PYADSH(NFIN)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      INTEGER PYK,PYCHGE,PYCOMP
C...Commonblocks.
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      SAVE /PYJETS/,/PYDAT1/,/PYPARS/,/PYINT1/
C...Local array.
      DIMENSION IBEG(100),KSAV(10,5),IORD(10),PSUM(4),BETA(3)
 
C...Set primary vertex.
      DO 100 J=1,5
        V(MINT(83)+5,J)=0D0
        V(MINT(83)+6,J)=0D0
        V(MINT(84)+1,J)=0D0
        V(MINT(84)+2,J)=0D0
  100 CONTINUE
 
C...Isolate systems of particles with the same mother.
      NSYS=0
      IMS=-1
      DO 140 I=MINT(84)+3,NFIN
        IM=K(I,3)
        IF(IM.GT.0.AND.IM.LE.MINT(84)) IM=K(IM,3)
        IF(IM.NE.IMS) THEN
          NSYS=NSYS+1
          IBEG(NSYS)=I
          IMS=IM
        ENDIF
 
C...Set production vertices.
        IF(IM.LE.MINT(83)+6.OR.(IM.GT.MINT(84).AND.IM.LE.MINT(84)+2))
     &  THEN
          DO 110 J=1,4
            V(I,J)=0D0
  110     CONTINUE
        ELSE
          DO 120 J=1,4
            V(I,J)=V(IM,J)+V(IM,5)*P(IM,J)/P(IM,5)
  120     CONTINUE
        ENDIF
        IF(MSTP(125).GE.1) THEN
          IDOC=I-MSTP(126)+4
          DO 130 J=1,5
            V(IDOC,J)=V(I,J)
  130     CONTINUE
        ENDIF
  140 CONTINUE
 
C...End loop over systems. Return if no showers to be performed.
      IBEG(NSYS+1)=NFIN+1
      IF(MSTP(71).LE.0) RETURN
 
C...Loop through systems of particles; check that sensible size.
      DO 260 ISYS=1,NSYS
        NSIZ=IBEG(ISYS+1)-IBEG(ISYS)
        IF(NSIZ.EQ.1.AND.ISYS.EQ.1) THEN
        ELSEIF(NSIZ.LE.1) THEN
          CALL PYERRM(2,'(PYADSH:) only one particle in system')
        ELSEIF(NSIZ.GT.7) THEN
          CALL PYERRM(2,'(PYADSH:) more than seven particles in system')
        ELSE
 
C...Save status codes and daughters of showering pair; reset them.
          DO 150 J=1,4
            PSUM(J)=0D0
  150     CONTINUE
          DO 170 II=1,NSIZ
            I=IBEG(ISYS)-1+II
            KSAV(II,1)=K(I,1)
            IF(K(I,1).GT.10) THEN
              K(I,1)=1
              IF(KSAV(II,1).EQ.14) K(I,1)=3
            ENDIF
            IF(KSAV(II,1).LE.10) THEN
            ELSEIF(K(I,1).EQ.1) THEN
              KSAV(II,4)=K(I,4)
              KSAV(II,5)=K(I,5)
              K(I,4)=0
              K(I,5)=0
            ELSE
              KSAV(II,4)=MOD(K(I,4),MSTU(5))
              KSAV(II,5)=MOD(K(I,5),MSTU(5))
              K(I,4)=K(I,4)-KSAV(II,4)
              K(I,5)=K(I,5)-KSAV(II,5)
            ENDIF
            DO 160 J=1,4
              PSUM(J)=PSUM(J)+P(I,J)
  160       CONTINUE
  170     CONTINUE
 
C...Perform shower.
          QMAX=SQRT(MAX(0D0,PSUM(4)**2-PSUM(1)**2-PSUM(2)**2-
     &    PSUM(3)**2))
          IF(ISYS.EQ.1) QMAX=VINT(55)
          NSAV=N
          IF(NSIZ.EQ.2) THEN
            CALL PYSHOW(IBEG(ISYS),IBEG(ISYS)+1,QMAX)
          ELSE
            CALL PYSHOW(IBEG(ISYS),-NSIZ,QMAX)
          ENDIF
 
C...Look up showered copies of original showering particles.
          DO 250 II=1,NSIZ
            I=IBEG(ISYS)-1+II
            IMV=I
            IF(N.EQ.NSAV.OR.K(I,1).LE.10) THEN
            ELSEIF(K(I,1).EQ.11) THEN
  180         IMV=MOD(K(IMV,4),MSTU(5))
              IF(K(IMV,1).EQ.11) GOTO 180
            ELSE
              KDA1=MOD(K(I,4),MSTU(5))
              KDA2=MOD(K(I,5),MSTU(5))
              DO 190 I3=I+1,N
                IF(K(I3,2).EQ.K(I,2).AND.(I3.EQ.KDA1.OR.I3.EQ.KDA2))
     &          THEN
                  IMV=I3
                  KDA1=MOD(K(I3,4),MSTU(5))
                  KDA2=MOD(K(I3,5),MSTU(5))
                ENDIF
  190         CONTINUE
            ENDIF
 
C...Restore daughter info of original partons to showered copies.
            IF(KSAV(II,1).GT.10) K(IMV,1)=KSAV(II,1)
            IF(KSAV(II,1).LE.10) THEN
            ELSEIF(K(I,1).EQ.1) THEN
              K(IMV,4)=KSAV(II,4)
              K(IMV,5)=KSAV(II,5)
            ELSE
              K(IMV,4)=K(IMV,4)+KSAV(II,4)
              K(IMV,5)=K(IMV,5)+KSAV(II,5)
            ENDIF
 
C...Reset mother info of existing daughters to showered copies.
            DO 200 I3=IBEG(ISYS+1),NFIN
              IF(K(I3,3).EQ.I) K(I3,3)=IMV
              IF(K(I3,1).EQ.3.OR.K(I3,1).EQ.14) THEN
                IF(K(I3,4)/MSTU(5).EQ.I) K(I3,4)=K(I3,4)+MSTU(5)*(IMV-I)
                IF(K(I3,5)/MSTU(5).EQ.I) K(I3,5)=K(I3,5)+MSTU(5)*(IMV-I)
              ENDIF
  200       CONTINUE
 
C...Boost all original daughters to new frame of showered copy.
            IF(IMV.NE.I) THEN
              DO 210 J=1,3
                BETA(J)=(P(IMV,J)-P(I,J))/(P(IMV,4)+P(I,4))
  210         CONTINUE
              FAC=2D0/(1D0+BETA(1)**2+BETA(2)**2+BETA(3)**2)
              DO 220 J=1,3
                BETA(J)=FAC*BETA(J)
  220         CONTINUE
              DO 240 I3=IBEG(ISYS+1),NFIN
                IMO=I3
  230           IMO=K(IMO,3)
                IF(IMO.GT.0.AND.IMO.NE.I.AND.IMO.NE.K(I,3)) GOTO 230
                IF(IMO.EQ.I.OR.(K(I,3).LE.MINT(84).AND.IMO.EQ.K(I,3)))
     &          CALL PYROBO(I3,I3,0D0,0D0,BETA(1),BETA(2),BETA(3))
  240         CONTINUE
            ENDIF
  250     CONTINUE
 
C...End of loop over showering systems
        ENDIF
  260 CONTINUE
 
      RETURN
      END
