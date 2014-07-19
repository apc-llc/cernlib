*
* $Id: cdupky.F,v 1.1.1.1 1996/02/28 16:24:52 mclareni Exp $
*
* $Log: cdupky.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:52  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUPKY (KEYX, KEYO, PATHN, CHOPT, IRC)
*     =================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUPKY (KEYX, KEYO, PATHN, CHOPT, IRC*)                 *
*                                                                      *
*   Insert/Update Keys in CWD                                          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KEYX     Key-Vector containing the new Key values                *
*     KEYO     Key-Vector containing the new Key values                *
*     PATHN    Pathname of the directory                               *
*     CHOPT    Character options                                       *
*         A    Insert objects from Ascii file                          *
*         E    Object is new - Data-template shown                     *
*         I    Insert new Key                                          *
*         W    Insert objects from Ascii file without data             *
*         X    Replace (update) old key                                *
*                                                                      *
*   Called by CDEDAS, CDEDKY                                           *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/cxlink.inc"
      DIMENSION       IOPTS(5), KEYX(9), KEYO(9), IOWDS(16)
      CHARACTER       YESNO*1, CHIDH*120
      CHARACTER       PATHN*(*), CHOPT*(*), CHOP*1
      EQUIVALENCE     (IOPTA, IOPTS(1)), (IOPTE, IOPTS(2)),
     +                (IOPTI, IOPTS(3)), (IOPTW, IOPTS(4)),
     +                (IOPTX, IOPTS(5))
*
*     ------------------------------------------------------------------
*
* *** Decode the character option
*
      CALL UOPTC (CHOPT, 'AEIWX', IOPTS)
      IF (IOPTA.NE.0.AND.IOPTW.NE.0) THEN
        NDAT = 0
        CALL CDBANK (IDIVCD, LASTCX, LASTCX, 2, 'USER', 0, 0, NDAT,
     +               2, 0,IRC)
        IF (IRC.NE.0)         GO TO 999
        GO TO 10
      ENDIF
*
*  ** Insert Data ?
*
      CALL KUPROC ('Data Insert/Update ? (Y/N)', YESNO, LYN)
      IF (YESNO.EQ.'Y') THEN
        IF (IOPTE.NE.0) THEN
          CHOP = 'E'
        ELSE
          IF (IOPHCC.EQ.0) THEN
            CHOP = 'V'
          ELSE
            CHOP = 'H'
          ENDIF
        ENDIF
        CALL CDRDIO (PATHN, KEYX, NDAT, CHIDH, LCHID, CHOP, IRC)
        IF (IRC.NE.0)         GO TO 999
        IF (NDAT.GT.0) THEN
          CALL MZIOCH (IOWDS, 16, CHIDH(1:LCHID))
          CALL CDBANK (IDIVCD, LASTCX, LASTCX, 2, 'USER', 0, 0, NDAT,
     +                 IOWDS, 0, IRC)
          IF (IRC.NE.0)       GO TO 999
          CALL CDRDDA (Q(KOFUCD+LASTCX+1))
        ELSE
          NDAT = 0
          CALL CDBANK (IDIVCD, LASTCX, LASTCX, 2, 'USER', 0, 0, NDAT,
     +                 2, 0, IRC)
          IF (IRC.NE.0)       GO TO 999
        ENDIF
      ELSE
        NDAT = 0
        CALL CDBANK (IDIVCD, LASTCX, LASTCX, 2, 'USER', 0, 0, NDAT,
     +               2, 0, IRC)
        IF (IRC.NE.0)         GO TO 999
      ENDIF
*
   10 CONTINUE
      IF (IOPTX.NE.0) THEN
        CALL CDSTOM (PATHN, LASTCX, LKVWCX(1), IDIVCD, NWKYCK, 1,
     +               KEYO, KEYX, 0, 'PDR', IRC)
      ELSE IF (IOPTI.NE.0) THEN
        CALL CDSTOM (PATHN, LASTCX, LKVWCX(1), IDIVCD, NWKYCK, 1,
     +               KEYO, KEYX, 0, 'PD',  IRC)
      ENDIF
      CALL MZDROP (IDIVCD, LASTCX, ' ')
*                                                             END CDUPKY
  999 END
