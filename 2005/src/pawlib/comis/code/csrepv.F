*
* $Id: csrepv.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: csrepv.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/03 16/06/94  10.41.59  by  Rene Brun
*-- Author :    Vladimir Berezhnoi   09/12/93
      SUBROUTINE CSREPV(NAME, IPOS, KPOS, LENV)
      CHARACTER *(*)    NAME
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
*+SEQ,CSPNTS.
#include "comis/csfmt.inc"
#include "comis/cskucs.inc"
#include "comis/cspanm.inc"
      CHARACTER *4 LEXNM, CH*1
      INTEGER CSLTLI
      LENV=0
      CALL CSCHID(NAME)
      IT=CSLTLI(IPVS)
      IF(IT.EQ.0)RETURN
      CALL CCOPYA(IQ(IT+1),NCIDGI,KSIDL-1)
      IF(IQ(NUMGI).NE.JKUVBS)RETURN
      NDIM=IQ(MODEGI+3)
      ITYPGI=IABS(ITYPGI)
      IF(ITYPGI.EQ.1)THEN
         BUF(:LIQN)=IQNAME(:LIQN)
         LENV=LIQN
      ELSE
         BUF(:LQN)=QNAME(:LQN)
         LENV=LQN
      ENDIF
      LP=LENV+LLKN+2
      BUF(LENV+1:LP)='(' // LKNAME(:LLKN) // '('
      IND=ISHGI+2
*      WRITE(BUF(LP+1:LP+4),'(I3)') ISHGI+2
      WRITE(BUF(LP+1:LP+4),'(I3)') IND
      BUF(LP+5:LP+6)=')+'
      LENV=LP+6
*  Q(LINK(I)+
      CALL CSNLEX(FMT(:KPOS),IPOS,LEXNM,LLEX,LEXEM)
      IF(LEXNM(1:1).EQ.'(' )THEN
         NNDIM=1
   10    KETLEV=1
         IPF=IPOS
         IF(NNDIM.EQ.NDIM)THEN
   20       CALL CSNLEX(FMT(:KPOS),IPOS,LEXNM,LLEX,LEXEM)
            IF(LLEX.LE.0)GO TO 40
            CH=LEXNM(1:1)
            IF(CH.EQ.')')THEN
               IF(KETLEV.GT.1)THEN
                  KETLEV=KETLEV-1
                  GO TO 20
               ENDIF
            ELSEIF(CH.EQ.'(')THEN
               KETLEV=KETLEV+1
               GO TO 20
            ELSE
               GO TO 20
            ENDIF
* add index to BUF
            L=IPOS-IPF+1
            BUF(LENV+1:LENV+L)='(' // FMT(IPF:IPOS-2) // ')'
            LENV=LENV+L
            IF(NDIM.GT.1)THEN
               LENV=LENV+1
               BUF(LENV:LENV)='*'
               WRITE(BUF(LENV+1:LENV+5),'(I5)')IQ(MODEGI+NNDIM+2)
               LENV=LENV+5
            ENDIF
* test for positive offset
            II=KUVOFS-IQ(MODEGI+2)+1
            IF(II.GE.0)THEN
              LENV=LENV+1
              BUF(LENV:LENV)='+'
            ENDIF
            WRITE(BUF(LENV+1:LENV+5),'(I5)')II
            LENV=LENV+5
         ELSE
   30       CALL CSNLEX(FMT(:KPOS),IPOS,LEXNM,LLEX,LEXEM)
            IF(LLEX.LE.0)GO TO 40
            CH=LEXNM(1:1)
            IF(CH.EQ.',')THEN
               IF(KETLEV.GT.1)GO TO 30
            ELSEIF(CH.EQ.'(')THEN
               KETLEV=KETLEV+1
               GO TO 30
            ELSEIF(CH.EQ.')')THEN
               KETLEV=KETLEV-1
               GO TO 30
            ELSE
               GO TO 30
            ENDIF
* add index to BUF
            L=IPOS-IPF+1
            BUF(LENV+1:LENV+L)='(' // FMT(IPF:IPOS-2) // ')'
            LENV=LENV+L
            IF(NNDIM.GT.1)THEN
               LENV=LENV+1
               BUF(LENV:LENV)='*'
               WRITE(BUF(LENV+1:LENV+5),'(I5)')IQ(MODEGI+NNDIM+2)
               LENV=LENV+5
            ENDIF
            LENV=LENV+1
            BUF(LENV:LENV)='+'
            NNDIM=NNDIM+1
            GO TO 10
         ENDIF
      ELSE
         WRITE(BUF(LENV+1:LENV+3),'(I2)')KUVOFS+1
         LENV=LENV+3
         IPOS=IPOS-LLEX
      ENDIF
      LENV=LENV+1
      BUF(LENV:LENV)=')'
***      CALL HCOMPR(BUF,LENV)
      CALL CSRBLK(BUF(:LENV),LENV)
      RETURN
   40 LENV=-1
      END
