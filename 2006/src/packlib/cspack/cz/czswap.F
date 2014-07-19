*
* $Id: czswap.F,v 1.1.1.1 1996/03/08 15:44:23 mclareni Exp $
*
* $Log: czswap.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:23  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZSWAP(NODE,LUN,ISTAT)
#include "cspack/czsock.inc"
#include "cspack/czunit.inc"
      CHARACTER*(*) NODE
      SAVE JSVIN,JSVOUT
*
*     Swap according to LUN or NODE
*
      ISTAT  = 0
      LN     = LENOCC(NODE)
 
      IF(IDEBXZ.GE.3) PRINT *,'CZSWAP. enter for NODE,LUN = ',NODE,LUN
 
      IF(LN.NE.0) THEN
 
         IF(IDEBXZ.GE.3) PRINT *,'CZSWAP. swap by node...'
         INODE = ICNTH(NODE(1:LN),CHNODE,MAXSOC)
         IF(INODE.EQ.0) THEN
            ISTAT = -1
            RETURN
          ENDIF
*
*     Reset input and output socket numbers, saving old settings
*
         JSVIN  = ISKIN
         JSVOUT = ISKOUT
         ISKIN  = JSKIN (INODE)
         ISKOUT = JSKOUT(INODE)
 
      ELSEIF(LUN.GT.0) THEN
*
*     Reset input and output socket numbers, saving old settings
*
         IF(IDEBXZ.GE.3) PRINT *,'CZSWAP. swap by LUN...'
         JSVIN  = ISKIN
         JSVOUT = ISKOUT
         ISKIN  = JSKIN (LUN)
         ISKOUT = JSKOUT(LUN)
         ELSE
*
*     Restore input and output socket numbers
*
         ISKIN  = JSVIN
         ISKOUT = JSVOUT
         ENDIF
 
      END
 
