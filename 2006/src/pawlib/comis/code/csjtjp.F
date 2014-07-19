*
* $Id: csjtjp.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: csjtjp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 17/02/94  16.23.45  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSJTJP(INF,IPCE)
***--------------------------------------------
*     i think: never used in paw inv. routine
***--------------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
#include "comis/cssysd.inc"
#include "comis/cskeys.inc"
#include "comis/csichv.inc"
        COMMON/CSGSCM/IGS,JGSST,NGSST,CSJUNK(3)
        INTEGER INF(2)
        PARAMETER (KJMPT=110)
        CHARACTER *1 K, KCARET*4
        DATA KCARET/'<CR>'/
      JCARET=MJSCHA(KCARET)
      N=0
      I=IPCE+2
      NTB=(LAST-I)*NBYTPW
      JST=MJCHAR(KOD(I))
      JBP=INF(1)
      NSTRP=INF(2)/100
      NBCH=MOD(INF(2),100)
      IF(NSTRP.EQ.NSTRG.OR.(NSTRP.EQ.NSTRG-1.AND.JMC.GT.JBP))THEN
         N=JGSST-JBP
         IF(N.GT.0)THEN
           IF(N.GT.NTB)GO TO 3
           CALL CCOPYS(JBP,JST,N)
           JST=JST+N
         ENDIF
      ELSE
        NSTR=NSTRG-NSTRP
*FIRST STRING
        NN=0
        II=JPMB
 41     NN=NN+1
        IF(NN.EQ.NSTRP)GO TO 42
        II=IQ(II)
        GO TO 41
 42     CALL CSGETL(II,REC1,NB,MARK)
        J=JSR1
        KF=MKBLAN(J,NB)
        J=J+NBCH
        NB=NB-NBCH
        IF(NB.GT.0)THEN
          IF(NB+4.GT.NTB)GO TO 3
          CALL CCOPYS(J,JST,NB)
          JST=JST+NB
          CALL CCOPYS(JCARET,JST,4)
          JST=JST+4
          N=NB+4
        ENDIF
*NEXT STRING
        DO 2 I=1,NSTR-1
        CALL CSGETL(II,REC1,NB,MARK)
        K=REC1(1:1)
        IF(K.EQ.'*')GO TO 2
        IF(IFORS.NE.0 .AND. K.EQ.'C')GO TO 2
        J=JSR1
        NT=NB
        KF=MKBLAN(J,NB)
        IF(IFORS.NE.0)THEN
          NT=NT-NB
          IF(NT.EQ.5 .AND. REC1(5:5).NE.'0')THEN
            J=J+1
            NB=NB-1
          ENDIF
        ENDIF
        N=N+NB+4
        IF(N.GT.NTB)GO TO 3
        CALL CCOPYS(J,JST,NB)
        JST=JST+NB
        CALL CCOPYS(JCARET,JST,4)
        JST=JST+4
  2     CONTINUE
*LAST STRING
        NB=JGSST-JMC
        IF(NB.GT.0)THEN
          N=N+NB
          IF(N.GT.NTB)GO TO 3
          CALL CCOPYS(JMC,JST,NB)
          JST=JST+NB
        ELSEIF(N.GT.0)THEN
          N=N-4
          JST=JST-4
        ENDIF
      ENDIF
      IF(N.LE.0)THEN
        N=4
        IF(N.GT.NTB)GO TO 3
        CALL CCOPYS(MJCHAR(4H*PAR),JST,4)
        JST=JST+4
      ENDIF
      NW=(N-1)/NBYTPW+1
      I=MOD(N,NBYTPW)
      IF(I.NE.0)THEN
        I=NBYTPW-I
        CALL CSSETC(JST,I,ICHBLN)
      ENDIF
      KOD(IPCE)=KJMPT
      KOD(IPCE+1)=NW
      IPCE=IPCE+NW+2
      CSJTJP=1
      RETURN
 3    CSJTJP=-1
      END
