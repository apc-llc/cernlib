*
* $Id: cskiov.F,v 1.1.1.1 1996/02/26 17:16:23 mclareni Exp $
*
* $Log: cskiov.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:23  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.12/05 20/03/92  16.04.18  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSKIOV(CHIOV,JCHIOV,LCHIOV)
***--------------------------------------------
*    it is interp. command input/output variable
***--------------------------------------------
*IOV IT
      CHARACTER *(*)CHIOV
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
#include "comis/cssysd.inc"
#include "comis/csfmt.inc"
#include "comis/csfres.inc"
#include "comis/csdpvs.inc"
      INTEGER IEQD(6),IEQD1(4),IEQD2(2),IDPRS(2)
      EQUIVALENCE (IEQD,D),(IEQD1,D1),(IEQD2,D2),(IDPRS,DCSRES)
      INTEGER CSIOIV,CSIORV,CSIODV,CSIOXV,CSIOCV
      CSKIOV=1
  151 IT=KD1(IPC)
      IPC=IPC+2
      IF(IFMT.EQ.-1)THEN
        IF(KEYRW.EQ.2)THEN
*         unformatted output
          IF(IT.EQ.3)THEN
            J=MIWORD(IA(ITA)+1)
            LENV=MOD(MIWORD(IA(ITA)),KON3)
            L=(LENV+3)/NBYTPW
            CALL CSUBAD(L)
            I=IUBS+LULIST
            J1=MJCHAR(IQ(I))
            CALL CCOPYS(J,J1,LENV)
            LULIST=LULIST+L
          ELSEIF(IT.EQ.5 .OR. IT.EQ.7)THEN
            JW=IA(ITA)
            CALL CSUBAD(2)
            I=IUBS+LULIST
            IQ(I)=IA(JW-JTOPA)
            IQ(I+1)=IA(JW-JTOPA+1)
            LULIST=LULIST+2
          ELSE
            JW=IA(ITA)
            CALL CSUBAD(1)
            I=IUBS+LULIST
            IQ(I)=IA(JW-JTOPA)
            LULIST=LULIST+1
          ENDIF
        ELSE
*         unformatted input
          I=1
          IF(IT.EQ.3)THEN
            J=MIWORD(IA(ITA)+1)
#if defined(CERNLIB_APOLLO)
            J=RSHFT(J,2)
#endif
#if defined(CERNLIB_UNIX)
            J=ISHFT(J,-2)
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_IBM)
            J=J/NBYTPW
#endif
            LENV=MOD(MIWORD(IA(ITA)),KON3)
            L=(LENV+3)/NBYTPW
          ELSEIF(IT.EQ.5 .OR. IT.EQ.7)THEN
            J=IA(ITA)
            L=2
          ELSE
            J=IA(ITA)
            L=1
          ENDIF
          IF(IUBTOP.GT.2)THEN
            JJ=IUBUF(IUBTOP-2)+IUBUF(IUBTOP-1)
            IF(JJ.EQ.J)THEN
              IUBUF(IUBTOP-1)=IUBUF(IUBTOP-1)+L
            ELSE
              IF(IUBTOP+2.GT.256)THEN
                IFMTST=7
                I=0
       PRINT *,' CSUI: too long unformatted input list(>128)'
              ELSE
                IUBUF(IUBTOP)=J
                IUBUF(IUBTOP+1)=L
                IUBTOP=IUBTOP+2
              ENDIF
            ENDIF
          ELSE
            IUBUF(IUBTOP)=J
            IUBUF(IUBTOP+1)=L
            IUBTOP=IUBTOP+2
          ENDIF
          LULIST=LULIST+L
        ENDIF
        GO TO 1516
      ENDIF
      GO TO (1511,1512,1513,1511,1515,1511,1517),IT
C     I/O INTEGER VAR.
 1511 JW=IA(ITA)
      IIOV=IA(JW-JTOPA)
      I=CSIOIV(IIOV)
      IF (KEYRW.EQ.1) IA(JW-JTOPA)=IIOV
      GO TO 1516
C     I/O REAL VAR.
 1512 JW=IA(ITA)
      RIOV=RA(JW-JTOPA)
      I=CSIORV(RIOV)
      IF (KEYRW.EQ.1) RA(JW-JTOPA)=RIOV
      GO TO 1516
C     I/O CHAR VAR.
 1513 J=MIWORD(IA(ITA)+1)
      LENV=MOD(MIWORD(IA(ITA)),KON3)
      L=MIN0(LENV,LCHIOV)
      CALL CCOPYS(J,JCHIOV,L)
      I=CSIOCV(CHIOV(1:L))
      IF (KEYRW.EQ.1) CALL CCOPYS(JCHIOV,J,L)
      GO TO 1516
C     I/O DOUBLE PR. VAR.
 1515 JW=IA(ITA)
      IEQD(1)=IA(JW-JTOPA)
      IEQD(2)=IA(JW-JTOPA+1)
      I=CSIODV(D)
      IF (KEYRW.EQ.1) THEN
        IA(JW-JTOPA)  =IEQD(1)
        IA(JW-JTOPA+1)=IEQD(2)
      END IF
      GO TO 1516
C     I/O complex VAR.
 1517 JW=IA(ITA)
      IEQD(1)=IA(JW-JTOPA)
      IEQD(2)=IA(JW-JTOPA+1)
      I=CSIOXV(CX)
      IF (KEYRW.EQ.1) THEN
        IA(JW-JTOPA)  =IEQD(1)
        IA(JW-JTOPA+1)=IEQD(2)
      END IF
 1516 ITA=ITA-1
      IF(I.EQ.0)THEN
        IF(JIOSTA.NE.0)IA(JIOSTA-JTOPA)=IFMTST
        IF(IFMTST.LT.0 . AND. LIOEND.NE.-1)THEN
          IPC=LIOEND
        ELSEIF(LIOERR.NE.-1)THEN
          IPC=LIOERR
        ELSE
***          GO TO 704
          CSKIOV=-4
        ENDIF
      ENDIF
***      GO TO 999
      END
