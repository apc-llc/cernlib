*
* $Id: cdkupk.F,v 1.2 1996/03/12 13:08:42 cernlib Exp $
*
* $Log: cdkupk.F,v $
* Revision 1.2  1996/03/12 13:08:42  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:33  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKUPK
#include "hepdb/cdunit.inc"
#include "hepdb/slate.inc"
      CHARACTER*20 CHOPT
 
      CALL KUGETI(IPACK)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LCHOPT.EQ.0) THEN
         IOPTS = 0
      ELSE
         IOPTS = INDEX(CHOPT(1:LCHOPT),'S')
      ENDIF
 
      IF(IOPTS.EQ.0) THEN
 
         CALL CDUPTM(IDATE,ITIME,IPACK,IRC)
 
      ELSE
 
         CALL CDUPTS(IDATE,ITIME,IPACK,IRC)
 
      ENDIF
 
      IF(IRC.EQ.0) THEN
 
         WRITE(LOUTCD,9001) IPACK,IDATE,ITIME
 
      ELSE
 
         WRITE(LOUTCD,9002) IRC,IPACK
 
      ENDIF
 
9001  FORMAT(' CDUKPK. unpacked value of ',I10,' is ',I6,1X,I6)
 
9002  FORMAT(' CDUKPK. error ',I3,' unpacking ',I10)
 
      END
