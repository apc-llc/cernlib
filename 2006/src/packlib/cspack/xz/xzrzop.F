*
* $Id: xzrzop.F,v 1.1.1.1 1996/03/08 15:44:32 mclareni Exp $
*
* $Log: xzrzop.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZRZOP(LUN,NODE,CHFILE,CHOPT,LRECL,IRC)
      CHARACTER*(*) CHFILE,NODE
 
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
#include "cspack/czunit.inc"
#include "cspack/czopts.inc"
      IRC = 0
*
*     Does a link already exist to node in question
*
      LN    = LENOCC(NODE)
      LF    = LENOCC(CHFILE)
      INODE = ICNTH(NODE(1:LN),CHNODE,MAXSOC)
*
*     Always fill in node name
*
      CHNODE(LUN) = NODE(1:LN)
      IF(INODE.EQ.0) THEN
*
*     Request OPEN
*
         CALL CZOPEN('zserv',NODE(1:LN),IRC)
         IF(IRC.NE.0) THEN
            PRINT *,'XZRZOP. cannot open communications with ',
     +      NODE(1:LN)
            GOTO 99
            ENDIF
         JSKIN(LUN)  = ISKIN
         JSKOUT(LUN) = ISKOUT
      ELSE
*
*     Swap to current node
*
         CALL CZSWAP(NODE(1:LN),0,ISTAT)
         IF(ISTAT.NE.0) THEN
            PRINT *,'XZOPEN. cannot swap to node ',NODE(1:LN)
            RETURN
         ENDIF
         JSKIN(LUN)  = ISKIN
         JSKOUT(LUN) = ISKOUT
      ENDIF
      WRITE(CHMAIL,9001) CHFILE(1:LF)
9001  FORMAT('XZIO :RZOP',A64)
      CALL CZPUTA(CHMAIL,ISTAT)
      CHMAIL = ' '
      WRITE(CHMAIL,9002) LUN,LRECL,CHOPT
9002  FORMAT('XZIO :OLUN',I3,I6,'/',A8,'/')
      CALL CZPUTA(CHMAIL,ISTAT)
*
*     Get status and record length
*
      CALL CZGETA(CHMAIL,ISTAT)
 
      READ(CHMAIL,9003) IRC,LRECL
9003  FORMAT(10X,I3,I6)
 
      CALL CZSWAP(' ',0,ISTAT)
99    CONTINUE
      END
