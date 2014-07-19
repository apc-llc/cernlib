*
* $Id: csunam.F,v 1.1.1.1 1996/02/26 17:16:31 mclareni Exp $
*
* $Log: csunam.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 11/02/94  10.26.24  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   01/02/94
      SUBROUTINE CSUNAM(NAME,LNAME)
*----------------------------------
*   find out next unused name
*   for first call lname must be 0
*----------------------------------
      CHARACTER *(*) NAME
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
      INTEGER CSLTLI,CSITLI
      INTEGER ID1(26)
      CHARACTER  LETS*26,DIGS*10,CHID*32
      DATA LETS/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA DIGS/'0123456789'/
      SAVE ID1,LETS,DIGS
*
*-- for fist call find out all one letter names used in routine
*
      IF(LNAME.EQ.0)THEN
        DO 1 I=1,26
          ID1(I)=0
 1      CONTINUE
        I=IPLI
 2      IF(I.GT.0)THEN
          IF(IQ(I+1).EQ.1)THEN
            CALL CSGTIDL(I,CHID,NC)
            K=INDEX(LETS,CHID(1:1))
            ID1(K)=1
          ENDIF
          I=IQ(I)
          GO TO 2
        ENDIF
      ENDIF
*
*-- try find out one letter name
*
      DO 3 I=1,26
        IF(ID1(I).EQ.0)THEN
          ID1(I)=1
          NAME=LETS(I:I)
          LNAME=1
          RETURN
        ENDIF
 3    CONTINUE
*
*-- all one letter idents used in routine, try letter+digit
*
      DO 5 I=1,26
        DO 4 J=1,10
          NAME=LETS(I:I)//DIGS(J:J)
          CALL CSCHID(NAME(:2))
          IF(CSLTLI(IPVS).EQ.0)THEN
            K=CSITLI(IPVS)
            LNAME=2
            RETURN
          ENDIF
 4      CONTINUE
 5    CONTINUE
*
*-- for very strange routine which used all(286) l and l+d names
*
      LNAME=0
      NAME=' '
      END
