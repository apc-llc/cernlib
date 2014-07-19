*
* $Id: hwuepr.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwuepr.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWUEPR.
*CMZ :-        -26/04/91  10.18.58  by  Bryan Webber
*-- Author :    Bryan Webber
C----------------------------------------------------------------------
      SUBROUTINE HWUEPR
C     PRINTS OUT EVENT DATA
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER K,IST,IS,JS,I,J,ID
      CHARACTER*28 TITLE(11)
      LOGICAL FIRST(11)
      DATA TITLE/'     ---INITIAL STATE---    ',
     &           '    ---HARD SUBPROCESS---   ',
     &           '    ---PARTON SHOWERS---    ',
     &           '    ---GLUON SPLITTING---   ',
     &           '   ---CLUSTER FORMATION---  ',
     &           '    ---CLUSTER DECAYS---    ',
     &           ' ---STRONG HADRON DECAYS--- ',
     &           ' ---HEAVY FLAVOUR DECAYS--- ',
     &           '  ---H/W/Z BOSON DECAYS---  ',
     &           ' ---SOFT UNDERLYING EVENT---',
     &           '  ---MULTIPLE SCATTERING--- '/
      WRITE(6,10) NEVHEP,PBEAM1,PART1,PBEAM2,PART2,IPROC,
     &            NRN,ISTAT,IERROR,EVWGT
   10 FORMAT(///'  EVENT',I7,':',F11.2,' GEV/C ',A4,' ON',F11.2,
     &' GEV/C ',A4,'  PROCESS:',I6//'  SEEDS:',I11,' &',I11,
     &'   STATUS:',I4,'  ERROR:',I4,'  WEIGHT:',E11.4)
   20 FORMAT(/23X,A28//'  IHEP ID  IDPDG IST MO1 MO2 DA1 DA2   ',
     &' P-X     P-Y      P-Z    ENERGY    MASS')
      DO 30 K=1,11
   30 FIRST(K)=.TRUE.
      DO 40 J=1,NHEP
      IST=ISTHEP(J)
      IS=IST/10
      ID=IDHW(J)
      IF (IST.EQ.101) THEN
        WRITE (6,20) TITLE(1)
      ELSEIF (FIRST(2).AND.IS.EQ.12) THEN
        WRITE (6,20) TITLE(2)
        FIRST(2)=.FALSE.
      ELSEIF (FIRST(3).AND.IS.EQ.14) THEN
        WRITE (6,20) TITLE(3)
        FIRST(3)=.FALSE.
        FIRST(8)=.TRUE.
        FIRST(9)=.TRUE.
        FIRST(11)=.TRUE.
      ELSEIF (FIRST(4).AND.IST.GE.158.AND.IST.NE.160
     &                .AND.IST.LE.162) THEN
        WRITE (6,20) TITLE(4)
        FIRST(4)=.FALSE.
      ELSEIF (FIRST(5).AND.(IS.EQ.16.OR.IS.EQ.18)
     &                .AND.IST.GT.162) THEN
        WRITE (6,20) TITLE(5)
        FIRST(5)=.FALSE.
      ELSEIF (IS.EQ.19.OR.IST.EQ.1) THEN
        JS=ISTHEP(JMOHEP(1,J))/10
        IF (JS.EQ.15.OR.JS.EQ.16.OR.JS.EQ.18) THEN
          IF (FIRST(6)) THEN
            WRITE (6,20) TITLE(6)
            FIRST(6)=.FALSE.
          ENDIF
        ELSEIF (FIRST(7).AND.(.NOT.FIRST(6))) THEN
          WRITE (6,20) TITLE(7)
          FIRST(7)=.FALSE.
        ENDIF
      ELSEIF (FIRST(8).AND.(IST.EQ.125.OR.IST.EQ.155)) THEN
        WRITE (6,20) TITLE(8)
        FIRST(3)=.TRUE.
        FIRST(4)=.TRUE.
        FIRST(5)=.TRUE.
        FIRST(6)=.TRUE.
        FIRST(7)=.TRUE.
        FIRST(8)=.FALSE.
      ELSEIF (FIRST(9).AND.(IST.EQ.123.OR.IST.EQ.124)) THEN
        JS=ABS(IDHEP(JMOHEP(1,J)))
        IF (JS.EQ.23.OR.JS.EQ.24.OR.JS.EQ.25) THEN
          WRITE (6,20) TITLE(9)
          FIRST(3)=.TRUE.
          FIRST(4)=.TRUE.
          FIRST(5)=.TRUE.
          FIRST(6)=.TRUE.
          FIRST(7)=.TRUE.
          FIRST(8)=.TRUE.
          FIRST(9)=.FALSE.
        ENDIF
      ELSEIF (IST.EQ.170) THEN
        WRITE (6,20) TITLE(10)
      ELSEIF (FIRST(11).AND.(ID.EQ.71.OR.ID.EQ.72)) THEN
        WRITE (6,20) TITLE(11)
        FIRST(3)=.TRUE.
        FIRST(11)=.FALSE.
      ENDIF
      IF (PRNDEC) THEN
        WRITE(6,50) J,RNAME(IDHW(J)),IDHEP(J),IST,JMOHEP(1,J),
     &       JMOHEP(2,J),JDAHEP(1,J),JDAHEP(2,J),(PHEP(I,J),I=1,5)
      ELSE
        WRITE(6,51) J,RNAME(IDHW(J)),IDHEP(J),IST,JMOHEP(1,J),
     &       JMOHEP(2,J),JDAHEP(1,J),JDAHEP(2,J),(PHEP(I,J),I=1,5)
      ENDIF
   40 CONTINUE
   50 FORMAT(I5,1X,A4,I6,5I4,2F8.2,2F9.2,F8.2)
   51 FORMAT(Z5,1X,A4,I6,I4,4Z4,2F8.2,2F9.2,F8.2)
      END
