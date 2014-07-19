*
* $Id: xzopen.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzopen.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZOPEN(LUN,FNAME,NODE,LRECL,CHOPT,IRC)
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
#include "cspack/czunit.inc"
      CHARACTER*(*) FNAME,NODE
      CHARACTER*8   CHOPE
*
*     Open file on unit LUN. If no connection exists to specified
*     node, start one using CZOPEN.
*     CHOPT: C - respect case
*            D - D/A file, LRECL must be specified if N is also given
*            F - formatted file (default=unformatted)
*            N - new file
*
#include "cspack/czopts.inc"
      IRC = 0
*
*     Does a link already exist to node in question
*
      LN    = LENOCC(NODE)
      LF    = LENOCC(FNAME)
      LC    = LENOCC(CHOPT)
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
            PRINT *,'XZOPEN. cannot open communications with ',
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
*
*     Handle long file names...
*
      IF(LF.LE.64) THEN
         WRITE(CHMAIL,9001) FNAME(1:LF)
9001     FORMAT('XZIO :OPEN',A64)
         CALL CZPUTA(CHMAIL,ISTAT)
      ELSE
         WRITE(CHMAIL,9001) '_'
         CALL CZPUTA(CHMAIL,ISTAT)
         CALL CZPUTA(FNAME(1:LF),ISTAT)
      ENDIF
      CHOPE = ' '
      LCH = LENOCC(CHOPT)
      IF(LCH.GT.0) CHOPE = CHOPT(1:LCH)
      CALL CFILL(' ',CHMAIL,1,80)
      WRITE(CHMAIL,9002) LUN,LRECL,CHOPE
9002  FORMAT('XZIO :OLUN',I3,I6,'/',A8,'/')
      CALL CZPUTA(CHMAIL,ISTAT)
*
*     Check status
*
#include "cspack/czmess.inc"
 
      CALL CZSWAP(' ',0,ISTAT)
99    CONTINUE
      END
