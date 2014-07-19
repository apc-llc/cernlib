CDECK  ID>, HWSSPC.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWSSPC
C-----------------------------------------------------------------------
C     REPLACES SPACELIKE PARTONS BY SPECTATORS
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWUSQR,EMSQ,EMTR,EPAR,XPAR,QSQ
      INTEGER KHEP,IP,JP,IDH,IDP,ISP,IDSPC
      EXTERNAL HWUSQR
      IF (IERROR.NE.0) RETURN
      DO 50 KHEP=1,NHEP
      IF (ISTHEP(KHEP).EQ.145.OR.ISTHEP(KHEP).EQ.146) THEN
        IP=ISTHEP(KHEP)-144
        JP=IP
        IF (JDAHEP(1,IP).NE.0) JP=JDAHEP(1,IP)
        IDH=IDHW(JP)
        IDP=IDHW(KHEP)
        IF (IDH.NE.IDP) THEN
          IF (IDH.EQ.59) THEN
C---PHOTON CASE
            IF (IDP.LT.7) THEN
              IDSPC=IDP+6
            ELSEIF (IDP.LT.13) THEN
              IDSPC=IDP-6
            ELSE
              CALL HWWARN('HWSSPC',100,*999)
            ENDIF
C---IDENTIFY SPECTATOR
C   (1) QUARK CASE
          ELSEIF (IDP.LE.3) THEN
            DO 10 ISP=1,12
  10        IF (IDH.EQ.NCLDK(LOCN(IDP,ISP))) GOTO 20
            CALL HWWARN('HWSSPC',101,*999)
  20        IF (ISP.LE.3) THEN
              IDSPC=ISP+6
            ELSEIF (ISP.LE.9) THEN
              IDSPC=ISP+105
            ELSE
              IDSPC=ISP
            ENDIF
C---(2) ANTIQUARK CASE
          ELSEIF (IDP.GT.6.AND.IDP.LE.9) THEN
            IDP=IDP-6
            DO 30 ISP=1,12
  30        IF (IDH.EQ.NCLDK(LOCN(ISP,IDP))) GOTO 40
            CALL HWWARN('HWSSPC',103,*999)
            RETURN
  40        IF (ISP.LE.3) THEN
              IDSPC=ISP
            ELSEIF (ISP.LE.9) THEN
              IDSPC=ISP+111
            ELSE
              IDSPC=ISP-6
            ENDIF
C---SPECIAL CASE FOR REMNANT HADRON
          ELSEIF (IDH.EQ.71.OR.IDH.EQ.72) THEN
            IF (IDP.EQ.13) THEN
              IDSPC=IDP
            ELSE
              CALL HWWARN('HWSSPC',106,*999)
            ENDIF
          ELSE
            CALL HWWARN('HWSSPC',105,*999)
          ENDIF
C---REPLACE PARTON BY SPECTATOR
          IDHW(KHEP)=IDSPC
          IDHEP(KHEP)=IDPDG(IDSPC)
          ISTHEP(KHEP)=146+IP
          EMSQ=SIGN(PHEP(5,KHEP)**2,PHEP(5,KHEP))
          EMTR=EMSQ+PHEP(1,KHEP)**2+PHEP(2,KHEP)**2
          EPAR=PHEP(4,KHEP)
          CALL HWVDIF(4,PHEP(1,JP),PHEP(1,KHEP),PHEP(1,KHEP))
          IF (EPAR**2.LT.10000.*ABS(EMTR)) THEN
            CALL HWUMAS(PHEP(1,KHEP))
          ELSE
C---COMPUTE SPECTATOR MASS ELIMINATING ROUNDING ERRORS
            XPAR=EPAR/PHEP(4,JP)
            QSQ=SIGN(PHEP(5,JP)**2,PHEP(5,JP))
            PHEP(5,KHEP)=HWUSQR((1.-XPAR)*QSQ+EMSQ-EMTR/XPAR
     &                 -((QSQ*XPAR**2-EMTR)/(2*EPAR*XPAR**2))**2*XPAR)
          ENDIF
C---CHECK FOR UNPHYSICAL SPECTATOR
          IF (PHEP(4,KHEP).LT.0..OR.PHEP(5,KHEP).LT.0.) FROST=.TRUE.
        ENDIF
      ENDIF
  50  CONTINUE
  999 END
