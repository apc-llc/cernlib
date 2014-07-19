*
* $Id: cdend.F,v 1.1.1.1 1996/02/28 16:24:41 mclareni Exp $
*
* $Log: cdend.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:41  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDEND (CHPATH, CHOPT, IRC)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDEND (CHPATH, CHOPT, IRC*)                             *
*                                                                      *
*   Terminates the Data Base utility package                           *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CHPATH   Name of the top directory                               *
*     CHOPT    Character option                                        *
*          A   Closes all directories                                  *
*          C   Journal file is closed and deleted (default: reopen)    *
*              - pending updates will be sent                          *
*          P   Journal file is purged                                  *
*          S   Server mode - don't call CDSAVE                         *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
      CHARACTER*(*)   CHPATH
#include "hepdb/cdcblk.inc"
#include "hepdb/cinitl.inc"
      CHARACTER       TOP*16, TOPN*16, PATH*20
 
#include "hepdb/hdbopts.inc"
*
*     ------------------------------------------------------------------
*
 
      IRC = 0
      LPATH = LENOCC(CHPATH)
*     ICLOSE = 0
*     IF(IOPTA.NE.0.OR.IOPTC.NE.0) ICLOSE = -1
 
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
*
*     Close and send journal file
*
      IF(IOPTS.EQ.0) CALL CDSAVE (CHPATH,CHOPT,IRC)
*     CALL CDSTSV (' ', ICLOSE, IRC)
      INSRCD = 0
#endif
*
* *** Call RZEND for all the files opened (option A) or just the
*     file specified
*
      LFIXCD = LTOPCD
   10 IF (LFIXCD.EQ.0)  GO TO 20
      NCH    = IQ(KOFUCD+LFIXCD+MUPNCH)
      CALL UHTOC (IQ(KOFUCD+LFIXCD+MUPNAM), 4, TOP, NCH)
      IF (IOPTA.NE.0.OR.TOP(1:NCH).EQ.CHPATH(1:LPATH)) THEN
         TOPN = TOP(1:NCH)
         PATH = '//'//TOPN
         CALL RZCDIR (PATH,' ')
         CALL RZCLOS (TOPN,' ')
         IF (LTOPCD.EQ.LFIXCD) LTOPDB = LQ(KOFUCD+LFIXCD)
*
*     Drop also specified bank
*
         IF (TOP(1:NCH).EQ.CHPATH(1:LPATH)) THEN
            CALL MZDROP (IDIVCD, LFIXCD, ' ')
         ENDIF
      ENDIF
      LFIXCD = LQ(KOFUCD+LFIXCD)
      GO TO 10
*
   20 CONTINUE
*
*     Drop top bank if IOPTA
*
      IF (IOPTA.GT.0.AND.LTOPCD.GT.0) THEN
         CALL MZDROP (IDIVCD, LTOPCD, 'L')
         LTOPCD = 0
      ENDIF
*                                                              END CDEND
  999 END
