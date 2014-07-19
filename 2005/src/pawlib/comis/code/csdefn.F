*
* $Id: csdefn.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: csdefn.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 08/02/94  18.56.54  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   09/12/93
      SUBROUTINE CSDEFN(LUNOUT, ISPAWC, OK)
      LOGICAL ISPAWC, OK
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cskucs.inc"
#include "comis/cspanm.inc"
      COMMON/PAWC/NWPAW,H(10000)
**      INTEGER CSLTGB,CSLTLI
      INTEGER CSLTGB
**      PARAMETER (NQN=5,NIQN=5,NPN=5,NLKN=5)
**      CHARACTER *8 QNM(NQN),IQNM(NIQN),PNM(NPN),LKNM(NLKN)
**      INTEGER LQNM(NQN),LIQNM(NIQN),LPNM(NPN),LLKNM(NLKN)
**      DATA  QNM   /'Q',  'Q_', 'QQ', 'Q_Q', 'Q_Q_'  /
**     +   , LQNM   / 1,    2,    2,    3,     4      /
**     +   , IQNM   /'IQ', 'IQQ', 'I_Q', 'IQ_', 'IQ_Q'/
**     +   ,LIQNM   / 2,    3,     3,     3,     4    /
**     +   ,  PNM   /'H',  'PAW', 'CPAW','PAWPAW','QPAWQ'/
**     +   , LPNM   / 1,    3,     4,     6,       5     /
**     +   , LKNM   /'L',  'LK',  'L_K', 'LK_', 'L_K_'   /
**     +   ,LLKNM   / 1,     2,     3,     3,     4      /
      OK=.FALSE.
**      DO 1 I=1,NQN
**        CALL CSCHID(QNM(I))
**        IF(CSLTLI(IPVS).EQ.0)GO TO 2
** 1    CONTINUE
**                    RETURN
** 2    QNAME=QNM(I)
**      LQN= LQNM(I)
*-------------------------------------
      LQN=0
      CALL CSUNAM(QNAME,LQN)
      IF(LQN.EQ.0)RETURN
*-------------------------------------
**      DO 3 I=1,NIQN
**        CALL CSCHID(IQNM(I))
**        IF(CSLTLI(IPVS).EQ.0)GO TO 4
** 3    CONTINUE
**                    RETURN
** 4    IQNAME=IQNM(I)
**      LIQN= LIQNM(I)
*-------------------------------------
      LIQN=1
      CALL CSUNAM(IQNAME,LIQN)
      IF(LIQN.EQ.0)RETURN
*-------------------------------------
**      DO 5 I=1,NLKN
**        CALL CSCHID(LKNM(I))
**        IF(CSLTLI(IPVS).EQ.0)GO TO 6
** 5    CONTINUE
**                    RETURN
** 6    LKNAME=LKNM(I)
**      LLKN= LLKNM(I)
*-------------------------------------
      LLKN=1
      CALL CSUNAM(LKNAME,LLKN)
      IF(LLKN.EQ.0)RETURN
*-------------------------------------
      IF(ISPAWC)THEN
        CALL CSCHID('PAWC')
        I=CSLTGB(IPVS)
        IF(I.EQ.0)RETURN
        IT=IPLI
  11    IF(IT.EQ.0)RETURN
        CALL CCOPYA(IQ(IT+1),NCIDGI,KSIDL-1)
        IF(IQ(NUMGI).EQ.IADGB)THEN
          IF(MODEGI.GT.1)THEN
            NDIM=IQ(MODEGI+3)
            IF(NDIM.EQ.1)GO TO 12
          ENDIF
        ENDIF
        IT=IQ(IT)
        GO TO 11
 12     J=MJCHAR(IQ(IT+KSIDL))
        CALL CCOPYS(J,MJSCHA(PNAME),NCIDGI)
        LPN=NCIDGI
        IOFFS=18-ISHGI
        IF(IOFFS.LE.0)RETURN
      ELSE
**        DO 7 I=1,NPN
**          CALL CSCHID(PNM(I))
**          IF(CSLTLI(IPVS).EQ.0)GO TO 8
** 7     CONTINUE
**                    RETURN
** 8      PNAME=PNM(I)
**        LPN= LPNM(I)
*-------------------------------------
        LPN=1
        CALL CSUNAM(PNAME,LPN)
        IF(LPN.EQ.0)RETURN
*-------------------------------------
        IOFFS=18
      ENDIF
      OK=.TRUE.
*-------------------------------------
      WRITE(LUNOUT,105,ERR=99)IQNAME(:LIQN),LKNAME(:LLKN)
      WRITE(LUNOUT,106,ERR=99)QNAME(:LQN)
      WRITE(LUNOUT,102,ERR=99)IQNAME(:LIQN),NWPAW-18,
     +                         QNAME(:LQN), NWPAW-18
      IF(.NOT.ISPAWC)THEN
        WRITE(LUNOUT,106,ERR=99)PNAME(:LPN)
        WRITE(LUNOUT,101,ERR=99)PNAME(:LPN)
      ENDIF
      WRITE(LUNOUT,103,ERR=99)IQNAME(:LIQN),QNAME(:LQN),
     +                          PNAME(:LPN),IOFFS
      WRITE(LUNOUT,104,ERR=99)LKNAME(:LLKN), KUVTOT+2
      RETURN
 101  FORMAT(6X,'COMMON/PAWC/',A,'(10000)')
 102  FORMAT(6X,'DIMENSION ',A,'(',I8,'),',A,'(',I8,')')
 103  FORMAT(6X,'EQUIVALENCE(',A,'(1),',A,'(1),',A,'(',I3,'))')
 104  FORMAT(6X,'COMMON/CSKULK/',A,'(',I4,')')
 105  FORMAT('***--comis--***'/6X,'INTEGER ',A,',',A)
 106  FORMAT(6X,'REAL ',A)
 99   OK=.FALSE.
*-------------------------------------
      END
