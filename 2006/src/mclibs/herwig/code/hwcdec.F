CDECK  ID>, HWCDEC.
*CMZ :-        -26/04/91  10.18.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWCDEC
C-----------------------------------------------------------------------
C     DECAYS CLUSTERS INTO PRIMARY HADRONS
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER JCL,KCL,IP,JP,KP,IST,ID1,ID2,ID3
      IF (IERROR.NE.0) RETURN
      IF (IPROC/1000.EQ.9.OR.IPROC/1000.EQ.5) THEN
C---RELABEL CLUSTER CONNECTED TO REMNANT IN DIS
        DO 10 JCL=2,NHEP
        IF (ISTHEP(JCL).EQ.164) GOTO 20
        IF (ISTHEP(JCL).EQ.165) THEN
          IP=JMOHEP(1,JCL)
          JP=JMOHEP(2,JCL)
          KP=IP
          IF (ISTHEP(IP).EQ.162) THEN
            KP=JP
            JP=IP
          ENDIF
          IF (JMOHEP(2,KP).NE.JP) THEN
            IP=JMOHEP(2,KP)
          ELSE
            IP=JDAHEP(2,KP)
          ENDIF
          KCL=JDAHEP(1,IP)
          IF (ISTHEP(KCL)/10.NE.16) CALL HWWARN('HWCDEC',100,*999)
          ISTHEP(KCL)=164
          GOTO 20
        ENDIF
   10   CONTINUE
      ENDIF
   20 CONTINUE
      DO 30 JCL=1,NHEP
      IST=ISTHEP(JCL)
      IF (IST.GT.162.AND.IST.LT.166) THEN
C---DON'T HADRONIZE BEAM/TARGET CLUSTERS
        IF (IST.EQ.163.OR..NOT.GENSOF) THEN
C---SET UP FLAVOURS FOR CLUSTER DECAY
          CALL HWCFLA(IDHW(JMOHEP(1,JCL)),IDHW(JMOHEP(2,JCL)),ID1,ID3)
          CALL HWCHAD(JCL,ID1,ID3,ID2)
        ENDIF
      ENDIF
   30 CONTINUE
      ISTAT=50
  999 END
