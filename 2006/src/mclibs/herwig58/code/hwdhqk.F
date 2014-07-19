*
* $Id: hwdhqk.F,v 1.2 1996/04/11 07:50:50 jamie Exp $
*
* $Log: hwdhqk.F,v $
* Revision 1.2  1996/04/11 07:50:50  jamie
* Fix from Bryan Webber
*
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWDHQK.
*CMZ :-        -09/12/92  10.51.11  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWDHQK
C     PERFORMS WEAK DECAYS OF HEAVY QUARKS
C       0 -> W + 3,  W -> 1 + 2
C------------------------------------------------------------------------
C     ID=209,210 ARE B',T' WITH DECAYS T'->B'->C
C     ID=211,212 ARE B',T' WITH DECAYS T'->B'->T
C     ID=215-218 ARE THEIR ANTIQUARKS
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION HWRGEN,HWULDO,EMLIM,EMTST,EMMX,
     & EMWSQ,EMW,DMW,GMWSQ,PW(4),HWDWWT
      INTEGER HWDIDP,IFIRST,IHEP,JHEP,KHEP,LHEP,MHEP,IST,ID,ID1,ID2,ID3
      LOGICAL HEAVY,FOUND
      EXTERNAL HWDWWT,HWRGEN
      EQUIVALENCE (EMW,RMASS(198))
      EQUIVALENCE (DMW,GAMW)
      IF (IERROR.NE.0) RETURN
      IFIRST=1
   10 FOUND=.FALSE.
      DO 50 IHEP=IFIRST,NMXHEP
      IST=ISTHEP(IHEP)
      ID=IDHW(IHEP)
      IF (IST.GT.146.AND.IST.LE.151) THEN
        HEAVY=.FALSE.
        IF (.NOT.(ID.LT.5.OR.(ID.GT.6.AND.ID.LT.11))) THEN
C---TEST IDENTITY OF PARTON
          IF (ID.EQ.5) THEN
            HEAVY=IST.EQ.151
            ID1=HWDIDP(8,FBTM)
            ID2=ID1-7
            ID3=4
          ELSE IF (ID.EQ.6) THEN
            HEAVY=IST.EQ.151
            IF (.NOT.HEAVY) CALL HWDTOP(HEAVY)
            ID1=HWDIDP(7,FTOP)
            ID2=ID1-5
            ID3=5
          ELSEIF (ID.EQ.209.OR.ID.EQ.211) THEN
            HEAVY=.TRUE.
            ID3=4
            IF (ID.EQ.211) ID3=6
            IF (PHEP(5,IHEP).GT.RMASS(ID3)+RMASS(5)+RMASS(6)) THEN
              ID1=HWDIDP(8,FHVY)
            ELSE
              ID1=HWDIDP(8,FTOP)
            ENDIF
            ID2=ID1-7
          ELSEIF (ID.EQ.210.OR.ID.EQ.212) THEN
            HEAVY=.TRUE.
            ID3=ID-1
            IF (PHEP(5,IHEP).GT.RMASS(ID3)+RMASS(5)+RMASS(6)) THEN
              ID1=HWDIDP(7,FHVY)
            ELSE
              ID1=HWDIDP(7,FTOP)
            ENDIF
            ID2=ID1-5
          ELSE IF (ID.EQ.11) THEN
            HEAVY=IST.EQ.151
            ID1=HWDIDP(2,FBTM(1,2))
            ID2=ID1+5
            ID3=10
          ELSE IF (ID.EQ.12) THEN
            HEAVY=IST.EQ.151
            IF (.NOT.HEAVY) CALL HWDTOP(HEAVY)
            ID1=HWDIDP(1,FTOP(1,2))
            ID2=ID1+7
            ID3=11
          ELSEIF (ID.EQ.215.OR.ID.EQ.217) THEN
            HEAVY=.TRUE.
            ID3=10
            IF (ID.EQ.217) ID3=12
            IF (PHEP(5,IHEP).GT.RMASS(ID3)+RMASS(5)+RMASS(6)) THEN
              ID1=HWDIDP(2,FHVY(1,2))
            ELSE
              ID1=HWDIDP(2,FTOP(1,2))
            ENDIF
            ID2=ID1+5
          ELSEIF (ID.EQ.216.OR.ID.EQ.218) THEN
            HEAVY=.TRUE.
            ID3=ID-1
            IF (PHEP(5,IHEP).GT.RMASS(ID3)+RMASS(5)+RMASS(6)) THEN
              ID1=HWDIDP(1,FHVY(1,2))
            ELSE
              ID1=HWDIDP(1,FTOP(1,2))
            ENDIF
            ID2=ID1+7
          ENDIF
        ENDIF
        IF (HEAVY) THEN
C---THIS PARTON IS HEAVY
          FOUND=.TRUE.
          EMSCA=PHEP(5,IHEP)
          IF (NHEP+3.GT.NMXHEP) CALL HWWARN('HWDHQK',100,*999)
          KHEP=NHEP+1
          LHEP=NHEP+2
          MHEP=NHEP+3
C---SET UP WEAK DECAY
          IDHW(KHEP)=ID1
          IDHW(LHEP)=ID2
          IDHW(MHEP)=ID3
          IDHEP(KHEP)=IDPDG(ID1)
          IDHEP(LHEP)=IDPDG(ID2)
          IDHEP(MHEP)=IDPDG(ID3)
          PHEP(5,KHEP)=RMASS(ID1)
          PHEP(5,LHEP)=RMASS(ID2)
          PHEP(5,MHEP)=RMASS(ID3)
C---INCLUDE W PROPAGATOR
          EMMX=PHEP(5,IHEP)-PHEP(5,MHEP)
          EMWSQ=EMW**2
          GMWSQ=(EMW*DMW)**2
          EMLIM=GMWSQ
          IF (EMMX.LT.EMW) EMLIM=EMLIM+(EMWSQ-EMMX**2)**2
C---GENERATE DECAY
   30     CONTINUE
          CALL HWDTHR(PHEP(1,IHEP),PHEP(1,KHEP),
     &                PHEP(1,LHEP),PHEP(1,MHEP),HWDWWT)
          CALL HWVSUM(4,PHEP(1,KHEP),PHEP(1,LHEP),PW)
          EMTST=(EMWSQ-HWULDO(PW,PW))**2
          IF ((EMTST+GMWSQ)*HWRGEN(0).GT.EMLIM) GO TO 30
C---SET UP NEW JETS: SPECTATOR DOES NOT RADIATE
          ISTHEP(IHEP)=155
          ISTHEP(KHEP)=113
          ISTHEP(LHEP)=114
          ISTHEP(MHEP)=114
          JMOHEP(1,KHEP)=IHEP
          JMOHEP(2,KHEP)=LHEP
          JMOHEP(1,LHEP)=IHEP
          JMOHEP(2,LHEP)=KHEP
          JMOHEP(1,MHEP)=IHEP
          JMOHEP(2,MHEP)=IHEP
          JDAHEP(1,KHEP)=0
          JDAHEP(2,KHEP)=LHEP
          JDAHEP(1,LHEP)=0
          JDAHEP(2,LHEP)=KHEP
          JDAHEP(1,MHEP)=0
          JDAHEP(2,MHEP)=IHEP
          NHEP=NHEP+3
          IF (IST.EQ.151) THEN
C---FIND SPECTATOR
            JHEP=JMOHEP(2,IHEP)
            ISTHEP(JHEP)=115
            JMOHEP(2,JHEP)=MHEP
            JDAHEP(2,JHEP)=MHEP
          ENDIF
          JDAHEP(1,IHEP)=KHEP
          JDAHEP(2,IHEP)=MHEP
        ENDIF
      ENDIF
      IF (IHEP.EQ.NHEP) GO TO 60
   50 CONTINUE
   60 IFIRST=IHEP+1
      IF (FOUND) THEN
C---DO PARTON BRANCHING
        CALL HWBGEN
C---GO BACK TO CHECK FOR HEAVY DECAY PRODUCTS
        GO TO 10
      ENDIF
  999 END
