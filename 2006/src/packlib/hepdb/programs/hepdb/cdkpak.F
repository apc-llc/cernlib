*
* $Id: cdkpak.F,v 1.2 1996/03/12 13:08:39 cernlib Exp $
*
* $Log: cdkpak.F,v $
* Revision 1.2  1996/03/12 13:08:39  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKPAK
#include "hepdb/cdunit.inc"
#include "hepdb/slate.inc"
      CHARACTER*20 CHOPT
 
      CALL KUGETI(IDATE)
      CALL KUGETI(ITIME)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LCHOPT.EQ.0) THEN
         IOPTS = 0
      ELSE
         IOPTS = INDEX(CHOPT(1:LCHOPT),'S')
      ENDIF
 
      IF(IDATE.EQ.0.AND.ITIME.EQ.0) THEN
         CALL DATIME(IDATE,ITIME)
         IF(IOPTS.NE.0) ITIME = ITIME*100 + IS(6)
      ENDIF
 
      IF(IOPTS.EQ.0) THEN
 
         CALL CDPKTM(IDATE,ITIME,IPACK,IRC)
 
      ELSE
 
         CALL CDPKTS(IDATE,ITIME,IPACK,IRC)
 
      ENDIF
 
      IF(IRC.EQ.0) THEN
 
         WRITE(LOUTCD,9001) IDATE,ITIME,IPACK
 
      ELSE
 
         WRITE(LOUTCD,9002) IRC,IDATE,ITIME
 
      ENDIF
 
9001  FORMAT(' CDKPAK. packed value of ',I6,1X,I6,' is ',I10)
 
9002  FORMAT(' CDKPAK. error ',I3,' packing ',I6,1X,I6)
 
      END
