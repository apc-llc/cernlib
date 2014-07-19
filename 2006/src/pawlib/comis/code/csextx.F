*
* $Id: csextx.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: csextx.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 13/01/95  16.02.28  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSEXTX(CONSNM,N,IEXT)
***------------------------------------
*       csext calls this routine
*consnm-> name[.type][(type[(d1,..,dn)] , ...)] , ...
*type  -> i|r|c*inumber|l|d|[se]|x|?
*dim   -> inumber|*
* parelem=(next,itype,isize,ndim,d1,...dn)
***------------------------------------
      INTEGER CSLTGP,CSITGP,CSKIDN
         CHARACTER *(*) CONSNM,NM*72,REPL*8,CHLEX*72
         INTEGER IEXT(N),STATE,IDIM(7)
#include "comis/cspar.inc"
#include "comis/cstab.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
         PARAMETER (LUNDEF=0, LIDENT=1, LINUM=2,
     +              LDELIM=8, LNOMORE=9)
         DATA REPL/'IRCLDSXE'/
      NM=CONSNM
      CALL CLTOU(NM)
#if defined(CERNLIB_VAX)||defined(CERNLIB_IBM)||defined(CERNLIB_UNIX)
      LT=LEN(NM)
#endif
#if defined(CERNLIB_APOLLO)
      LT=80
#endif
      STATE=1
      NN=0
      IPOS=1
 100  CALL CSNLEX(NM,IPOS,CHLEX,LCHLEX,LEXEM)
 101  GO TO (1,2,3,4,5,6,7,8,9,10,11),STATE
*
*- routine's name
*
 1    IF(LEXEM.NE.LIDENT)GO TO 999
* copy chlex into iden
      CALL CSCHID(CHLEX(:LCHLEX))
      NOPAR=0
      LIST=0
      NN=NN+1
      STATE=2
      GO TO 100
*
*- test for ".type" or "(" or "," or "#"
*
 2    ITYP=0
      IF(CHLEX(1:1).EQ.'.')THEN
          STATE=3
      ELSEIF(CHLEX(1:1).EQ.'(')THEN
          STATE=5
          CALL CSNLEX(NM,IPOS,CHLEX,LCHLEX,LEXEM)
          IF(CHLEX(1:1).EQ.')' )THEN
            LIST=-1
            STATE=11
          ELSE
            GO TO 101
          ENDIF
      ELSE
          GO TO 11
      ENDIF
      GO TO 100
*
*- test for "type"
*
 3    IF(LEXEM.NE.LIDENT)GO TO 999
      ITYP=INDEX( REPL, CHLEX(1:1) )
      IF(ITYP.EQ.0)GO TO 999
      IF(ITYP.EQ.8)ITYP=6
      STATE=4
      GO TO 100
*
*- test for "(" or "," or "#" or " "
*
 4    IF(CHLEX(1:1).EQ.'(')THEN
          STATE=5
          CALL CSNLEX(NM,IPOS,CHLEX,LCHLEX,LEXEM)
          IF(CHLEX(1:1).EQ.')' )THEN
            LIST=-1
            STATE=11
          ELSE
            GO TO 101
          ENDIF
      ELSE
          GO TO 11
      ENDIF
      GO TO 100
*
*- parameter's type
*
 5    IF(LEXEM.NE.LIDENT)GO TO 999
      ITYPAR=INDEX( REPL, CHLEX(1:1) )
      IF(ITYPAR.EQ.0)GO TO 999
      IF(ITYPAR.EQ.6)ITYPAR=8
      ISIZE=4
      IF(ITYPAR.EQ.5 .OR. ITYPAR.EQ.7)THEN
        ISIZE=8
      ELSEIF(ITYPAR.EQ.3)THEN
*-- CHAR *inum
*   ??   *(*)
       CALL CSNLEX(NM,IPOS,CHLEX,LCHLEX,LEXEM)
       IF(CHLEX(1:1).NE.'*')GO TO 999
       CALL CSNLEX(NM,IPOS,CHLEX,LCHLEX,LEXEM)
       IF(LEXEM.NE.LINUM)GO TO 999
       READ(CHLEX(1:LCHLEX), * )ISIZE
      ENDIF
      NDIM=0
      STATE=6
      GO TO 100
*
*- test for "(2" or ",1" or ")1"
*
 6    IF( CHLEX(1:LCHLEX).EQ.'(' )THEN
        STATE=7
      ELSE
        GO TO 10
      ENDIF
      GO TO 100
*
*- inumber or *
*
 7    NDIM=NDIM+1
      IF(NDIM.GT.7)GO TO 998
      IF(LEXEM.EQ.LINUM )THEN
        READ(CHLEX(1:LCHLEX),*)IDIM(NDIM)
        STATE=8
      ELSEIF( CHLEX(1:1).EQ.'*' )THEN
        IDIM(NDIM)=0
        STATE=9
      ELSE
        GO TO 999
      ENDIF
      GO TO 100
*
*- ,2 or )2
*
 8    IF( CHLEX(1:LCHLEX).EQ.',' )THEN
        STATE=7
      ELSEIF( CHLEX(1:LCHLEX).EQ.')' )THEN
        STATE=10
      ELSE
        GO TO 999
      ENDIF
      GO TO 100

*
*- must be )2
*
 9    IF(CHLEX(1:1).NE.')')GO TO 999
      STATE=10
      GO TO 100
*
*- ,1 or )1
*
 10   I=MHLOC(NDIM+4)
      IQ(I)  = 0
      IQ(I+1)=ITYPAR
      IQ(I+2)=ISIZE
      IQ(I+3)=NDIM
      DO J=1,NDIM
        IQ(I+J+3)=IDIM(J)
      ENDDO
      IF(LIST.EQ.0)THEN
        LIST=I
      ELSE
        J=LIST
 1000   IF(IQ(J).NE.0)THEN
          J=IQ(J)
          GO TO 1000
        ENDIF
        IQ(J)=I
      ENDIF
      IF(CHLEX(1:1).EQ.',')THEN
        STATE=5
      ELSEIF(CHLEX(1:1).EQ.')')THEN
        STATE=11
      ELSE
        GO TO 999
      ENDIF
      GO TO 100
*
*- end of desc or ,0
*
 11   IF(LEXEM.EQ.LDELIM)THEN
        IF(CHLEX(:LCHLEX).EQ.',')THEN
          STATE=1
        ELSEIF(CHLEX(:LCHLEX).EQ.'#')THEN
          STATE=0
        ELSE
          GO TO 999
        ENDIF
      ELSE
        IF(LEXEM.NE.LNOMORE)GO TO 999
        STATE=0
      ENDIF

 900  I=CSLTGP(IPVS)
      IF(I.EQ.0)THEN
         IADGP=IEXT(NN)
         IF(ITYP.GT.0)THEN
           ITYPGP=ITYP
         ELSE
           ITYPGP=CSKIDN(JID,L)
         ENDIF
         IFCS=-1
         IDESGP=LIST
         I=CSITGP(IPVS)
      ELSE
        CALL CSDPRO(I)
        IADGP=IEXT(NN)
        IFCS=-1
        IF(ITYP.GT.0)ITYPGI=ITYP
        IF(IDESGP.GT.0)CALL CSLFRE(IDESGP)
        IDESGP=LIST
        CALL CSRTGP(I)
      ENDIF
      IF(STATE.NE.0)GO TO 100
      RETURN
 998  PRINT *,' more then 7 dimensions'
 999  CALL CSLFRE(LIST)
      WRITE(*,*)'CSEXT:error in ',NM(1:IPOS)
      END

