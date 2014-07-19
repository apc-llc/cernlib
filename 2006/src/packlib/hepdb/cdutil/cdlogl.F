*
* $Id: cdlogl.F,v 1.1.1.1 1996/02/28 16:24:42 mclareni Exp $
*
* $Log: cdlogl.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:42  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLOGL (CHPATH,LOGL,CHOPT,IRC)
*
*     Set the RZ loglevel for the specified path (or all if 'A')
*     and the CD loglevel
*
      CHARACTER*(*)   CHPATH
#include "hepdb/cdcblk.inc"
#include "hepdb/cinitl.inc"
      CHARACTER       TOP*16, TOPN*16, PATH*20
 
#include "hepdb/hdbopts.inc"
*
*     ------------------------------------------------------------------
*
 
      IRC    = 0
      LPATH  = LENOCC(CHPATH)
      ISTART = 1
      IF(CHPATH(1:2).EQ.'//') ISTART = 3
*
      LFIXCD = LTOPCD
   10 IF (LFIXCD.EQ.0)  GO TO 20
      NCH    = IQ(KOFUCD+LFIXCD+MUPNCH)
      CALL UHTOC (IQ(KOFUCD+LFIXCD+MUPNAM), 4, TOP, NCH)
      IF(IOPTA.NE.0.OR.TOP(1:NCH).EQ.CHPATH(ISTART:LPATH)) THEN
         LUN   = IQ(KOFUCD+LFIXCD+MUPLUN)
         CALL RZLOGL (LUN, LOGL)
      ENDIF
      LFIXCD = LQ(KOFUCD+LFIXCD)
      GO TO 10
*
   20 CONTINUE
      IDEBCD = LOGL
*                                                              END CDLOGL
  999 END
