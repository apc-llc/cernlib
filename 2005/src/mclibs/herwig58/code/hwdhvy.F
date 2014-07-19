*
* $Id: hwdhvy.F,v 1.1.1.1 1996/03/08 17:02:11 mclareni Exp $
*
* $Log: hwdhvy.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWDHVY.
*CMZ :-        -26/04/91  12.19.24  by  Federico Carminati
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWDHVY
C     IDENTIFIES HEAVY HADRONS AND SPLIT
C     THEM INTO THEIR CONSTITUENTS, THEN
C     PERFORMS WEAK DECAYS OF HEAVY QUARKS
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION XF1,XF2
      INTEGER IFIRST,IHEP,MHEP,IST,ID,ID1,ID2
      LOGICAL FOUND,HWEURO,HWCLEO,BPARTI
      IF (IERROR.NE.0) RETURN
      HWEURO=BDECAY.EQ.'EURO'
      HWCLEO=BDECAY.EQ.'CLEO'
      IFIRST=1
   10 FOUND=.FALSE.
      DO 50 IHEP=IFIRST,NMXHEP
      IST=ISTHEP(IHEP)
      ID=IDHW(IHEP)
      IF (IST.EQ.1.AND.ID.GT.220) THEN
        FOUND=.TRUE.
C---FOUND A HEAVY HADRON: IDENTIFY CONSTITUENTS
        BPARTI=.TRUE.
        IF((ID.GE.232.AND.ID.LE.244).OR.(ID.GE.255))
     &     BPARTI=.FALSE.
        IF(HWEURO.AND.BPARTI) THEN
          CALL HWDEUR(IHEP)
          GO TO 50
        ENDIF
        IF(HWCLEO.AND.BPARTI) THEN
          CALL HWDCLE(IHEP)
          GO TO 50
        ENDIF
        IF (ID.LT.245) THEN
          ID1=(ID-161)/12
          ID2= ID-160 -12*ID1
          IF (ID2.LT.4) THEN
            ID2=ID2+6
          ELSEIF (ID2.LT.10) THEN
            ID2=ID2+105
          ENDIF
        ELSE
          ID2=(ID-135)/10
          ID1= ID-134 -10*ID2
          IF (ID1.GT.9) THEN
            ID1=ID1-6
          ELSEIF (ID1.GT.3) THEN
            ID1=ID1+111
          ENDIF
        ENDIF
C---STORE CONSTITUENTS
        MHEP=NHEP+1
        NHEP=NHEP+2
        IDHW(MHEP)=ID1
        IDHEP(MHEP)=IDPDG(ID1)
        IDHW(NHEP)=ID2
        IDHEP(NHEP)=IDPDG(ID2)
        ISTHEP(IHEP)=199
        ISTHEP(MHEP)=151
        ISTHEP(NHEP)=151
        JDAHEP(1,IHEP)=MHEP
        JDAHEP(2,IHEP)=NHEP
        JMOHEP(1,MHEP)=IHEP
        JMOHEP(2,MHEP)=NHEP
        JDAHEP(1,MHEP)=0
        JDAHEP(2,MHEP)=NHEP
        JMOHEP(1,NHEP)=IHEP
        JMOHEP(2,NHEP)=MHEP
        JDAHEP(1,NHEP)=0
        JDAHEP(2,NHEP)=MHEP
C---SHARE MOMENTUM IN PROPORTION TO MASS -
C   MAKE SURE SPECTATOR MASS IS NOT SHIFTED
        XF1=RMASS(ID1)/PHEP(5,IHEP)
        XF2=RMASS(ID2)/PHEP(5,IHEP)
        IF (XF1.LT.XF2) THEN
          XF2=1.-XF1
        ELSE
          XF1=1.-XF2
        ENDIF
        CALL HWVSCA(5,XF1,PHEP(1,IHEP),PHEP(1,MHEP))
        CALL HWVSCA(5,XF2,PHEP(1,IHEP),PHEP(1,NHEP))
      ENDIF
      IF (IHEP.EQ.NHEP) GO TO 60
   50 CONTINUE
   60 IFIRST=NHEP+1
      IF (FOUND) THEN
C---DO HEAVY QUARK DECAYS
        CALL HWDHQK
C---DO CLUSTER FORMATION
        CALL HWCFOR
C---DO CLUSTER DECAY
        CALL HWCDEC
C---DO UNSTABLE PARTICLE DECAYS
        CALL HWDHAD
C---GO BACK TO CHECK FOR HEAVY DECAY PRODUCTS
        GO TO 10
      ENDIF
  999 END
