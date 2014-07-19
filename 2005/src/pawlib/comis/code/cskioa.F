*
* $Id: cskioa.F,v 1.1.1.1 1996/02/26 17:16:23 mclareni Exp $
*
* $Log: cskioa.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:23  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.12/05 20/03/92  16.04.18  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSKIOA(CHIOV,JCHIOV,LCHIOV)
***--------------------------------------------
*    it is interp. command input/output array
***--------------------------------------------
*IOA IT,IDARR
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
      CSKIOA=1
  152 IT=KD1(IPC)
      I=KD2(IPC)
      NDIM=IQ(I+3)
      N=IQ(I+NDIM+3)
      IPC=IPC+3
      IF(IFMT.EQ.-1)THEN
        IF(KEYRW.EQ.2)THEN
*         UNFORMATTED OUTPUT
          IF(IT.EQ.3)THEN
            J=MIWORD(IA(ITA)+1)
            LENV=MOD(MIWORD(IA(ITA)),KON3)*N
            L=(LENV+3)/NBYTPW
            CALL CSUBAD(L)
            I=IUBS+LULIST
            J1=MJCHAR(IQ(I))
            CALL CCOPYS(J,J1,LENV)
            LULIST=LULIST+L
          ELSEIF(IT.EQ.5 .OR. IT.EQ.7)THEN
            JW=IA(ITA)
            L=N*2
            CALL CSUBAD(L)
            I=IUBS+LULIST
            CALL CCOPYA(IA(JW-JTOPA),IQ(I),L)
            LULIST=LULIST+L
          ELSE
            JW=IA(ITA)
            L=N
            CALL CSUBAD(L)
            I=IUBS+LULIST
            CALL CCOPYA(IA(JW-JTOPA),IQ(I),L)
            LULIST=LULIST+L
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
            LENV=MOD(MIWORD(IA(ITA)),KON3)*N
            L=(LENV+3)/NBYTPW
          ELSEIF(IT.EQ.5 .OR.IT.EQ.7)THEN
            J=IA(ITA)
            L=2*N
          ELSE
            J=IA(ITA)
            L=N
          ENDIF
          IF(IUBTOP+2.GT.256)THEN
            IFMTST=7
            I=0
       PRINT *,' CSUI: too long unformatted input list(>128)'
          ELSE
            IUBUF(IUBTOP)=J
            IUBUF(IUBTOP+1)=L
            IUBTOP=IUBTOP+2
          ENDIF
          LULIST=LULIST+L
        ENDIF
        GO TO 1526
      ENDIF
      GO TO (1521,1522,1523,1521,1525,1521,1527),IT
C     I/O INTEGER ARR.
 1521 JW=IA(ITA)
      DO 15210 K=1,N
        IIOV=IA(JW-JTOPA)
        I=CSIOIV(IIOV)
        IF (KEYRW.EQ.1) IA(JW-JTOPA)=IIOV
15210 JW=JW+1
      GO TO 1526
C     I/O REAL ARR.
 1522 JW=IA(ITA)
      DO 15220 K=1,N
        RIOV=RA(JW-JTOPA)
        I=CSIORV(RIOV)
        IF (KEYRW.EQ.1) RA(JW-JTOPA)=RIOV
15220 JW=JW+1
      GO TO 1526
C     I/O CHAR ARR.
 1523 J=MIWORD(IA(ITA)+1)
      LENV=MOD(MIWORD(IA(ITA)),KON3)
      L=MIN0(LENV,LCHIOV)
      DO 15230 K=1,N
        CALL CCOPYS(J,JCHIOV,L)
        I=CSIOCV(CHIOV(1:L))
        IF (KEYRW.EQ.1) CALL CCOPYS(JCHIOV,J,L)
15230 J=J+L
      GO TO 1526
C     I/O DOUBLE PR. ARR.
 1525 JW=IA(ITA)
      DO 15250 K=1,N
        IEQD(1)=IA(JW-JTOPA)
        IEQD(2)=IA(JW-JTOPA+1)
        I=CSIODV(D)
        IF (KEYRW.EQ.1) THEN
          IA(JW-JTOPA)=IEQD(1)
          IA(JW-JTOPA+1)=IEQD(2)
        END IF
15250 JW=JW+2
      GO TO 1526
C     I/O COMPLEX ARR.
 1527 JW=IA(ITA)
      DO 15270 K=1,N
        IEQD(1)=IA(JW-JTOPA)
        IEQD(2)=IA(JW-JTOPA+1)
        I=CSIOXV(CX)
        IF (KEYRW.EQ.1) THEN
          IA(JW-JTOPA)=IEQD(1)
          IA(JW-JTOPA+1)=IEQD(2)
        END IF
15270 JW=JW+2
 1526 ITA=ITA-1
      IF(I.EQ.0)THEN
        IF(JIOSTA.NE.0)IA(JIOSTA-JTOPA)=IFMTST
        IF(IFMTST.LT.0 . AND. LIOEND.NE.-1)THEN
          IPC=LIOEND
        ELSEIF(LIOERR.NE.-1)THEN
          IPC=LIOERR
        ELSE
***          GO TO 704
          CSKIOA=-4
        ENDIF
      ENDIF
***      GO TO 999
      END
