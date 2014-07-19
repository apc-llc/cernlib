*
* $Id: cdtree.F,v 1.2 1996/03/12 13:09:09 cernlib Exp $
*
* $Log: cdtree.F,v $
* Revision 1.2  1996/03/12 13:09:09  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:29  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDTREE
#include "hepdb/cdunit.inc"
#include "hepdb/quest.inc"
      CHARACTER*255 PATH,PATH1
      CHARACTER*8   CHOPT
 
#include "hepdb/cdopen.inc"
 
      CALL KUGETC(PATH1,LPATH)
      IF(LPATH.EQ.0) THEN
         CALL RZCDIR(PATH,'R')
      ELSE
         CALL CDFIXF(PATH1,PATH)
      ENDIF
      LPATH = LENOCC(PATH)
 
      CALL KUGETI(NLEVEL)
 
      CALL KUGETC(CHOPT,LCHOPT)
*
*     Get tree
*
      WRITE(LOUTCD,*) 'CDTREK. directory tree structure below ',
     +   PATH(1:LPATH),' down ',NLEVEL,' levels'
 
      CALL CDTREK(PATH(1:LPATH),NLEVEL,CHOPT,IRC)
 
      WRITE(LOUTCD,*)
      WRITE(LOUTCD,*) IQUEST(11),' subdirectories found'
 
      END
