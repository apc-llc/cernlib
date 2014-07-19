*
* $Id: cddisd.F,v 1.1.1.1 1996/02/28 16:24:47 mclareni Exp $
*
* $Log: cddisd.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDISD (LUN, PATHN, KEYX, CHOPT, IRC)
*     ================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDISD (LUN, PATHN, KEYX, CHOPT, IRC*)                  *
*                                                                      *
*   Display the data attached to the directory PATHN and KEY           *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Unit number of file for display                         *
*     PATHN    Character string describing the pathname                *
*     KEYX     Key-vector                                              *
*     CHOPT    Character option                                        *
*          E   Only example for display to be shown                    *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDRDIO                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/cxlink.inc"
      DIMENSION       KEYX(29)
      CHARACTER       PATHN*(*), CHOPT*(*), CHEAD*300, PATHY*80
      CHARACTER       CB*1, CC*1, CI*1, CF*1, DC*4, BLANK*10
      INTEGER         DB, DI
      DATA            CB /'B'/, CC /'H'/, CI /'I'/, CF /'F'/
      DATA            BLANK /' '/
      DATA            DB /101/, DC /'DATA'/, DI /77/, DF /777.777/
*
*     ------------------------------------------------------------------
*
* *** Set the current directory
*
      CALL UOPTC (CHOPT, 'E', IOPTE)
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)                              GO TO 999
      PATHY  = PAT1CT
      CALL CDKYTG
*
*  ** Display keys
*
      NCHR   = LENOCC (PATHY)
      WRITE (LUN, 1001) PATHY(1:NCHR)
      IF ((IOPTE.NE.0) .OR. (IOPHCC.EQ.0)) THEN
        WRITE (LUN, 1002) CTAGCK(1)
      ELSE
        CHEAD = ' '
        CALL CDHEAD (IOTYCC(1), NWKYCK, CHEAD)
        WRITE (LUN, 1003) CHEAD(1:MXDPCC)
      ENDIF
*
      IF (IOPTE.NE.0) THEN
        IF (IOPXCA.NE.0) THEN
          CALL CDUPTS (IDV1, ITV1, KEYX(NOF1CK+1),        IRC)
          CALL CDUPTS (IDV2, ITV2, KEYX(NOF1CK+NPARCD+1), IRC)
          WRITE (LUN, 1004) IDV1, ITV1, IDV2, ITV2
        ELSE
          WRITE (LUN, 1004) (KEYX(NOF1CK+I), I = 1, 2*NPARCD)
        ENDIF
*
*  **   Write steering lines as format indication
*
        WRITE (LUN, 1005) CB, DB, CC, DC, CI, DI, CF, DF
      ELSE
*
*  **   Read in the data
*
        IF (LFRSCX.NE.0) THEN
          CALL MZDROP (IDIVCD, LFRSCX, 'L')
          LFRSCX = 0
        ENDIF
        IF (NKEYCK.EQ.0) THEN
          CALL UCOPY (KEYX, KEYVCK, NWKYCK)
          GO TO 20
        ENDIF
        CALL CDSGET (KEYX(IDHKSN), IDIVCD, LFRSCX, IRC)
*
   20   IF (IOPHCC.EQ.0) THEN
          WRITE (LUN, 1006) KEYX(IDHKSN)
        ELSE
          CALL CDDKYH (LUN, NWKYCK, KEYVCK, IOTYCC, ICONCK, IKEY, IRC)
        ENDIF
*
*  **   Display data if exists, if it does not, display dummy data
*
        IF (IRC.EQ.0.AND.LFRSCX.GT.0.AND.IQ(KOFUCD+LFRSCX-1).GT.0) THEN
          CALL CDWRDP (LUN, LFRSCX)
        ELSE
          WRITE (LUN, 1005) CB, DB, CC, DC, CI, DI, CF, DF
        ENDIF
        IF (LFRSCX.NE.0)  CALL MZDROP (IDIVCD, LFRSCX, ' ')
        IF (IRC.NE.0)                            GO TO 999
      ENDIF
*
      WRITE (LUN, 1007)
*
 1001 FORMAT ('     Data in Directory and Key-Vector'/A)
 1002 FORMAT (/3X,A8,3X)
 1003 FORMAT (/A)
 1004 FORMAT ('Object inserted with validity range ',6I9)
 1005 FORMAT (1X,A,4X,'1',4X,Z10,10X, 'This line is only an example !'
     +      /,1X,A,4X,'2',4X,A4 ,16X, 'This line is only an example !'
     +      /,1X,A,4X,'3',4X,I10,10X, 'This line is only an example !'
     +      /,1X,A,4X,'4',4X,E12.4,8X,'This line is only an example !')
 1006 FORMAT (I10)
 1007 FORMAT (4X,'We have reached the End of File Position - ',
     +        'do not delete this line !!')
*                                                             END CDDISD
  999 END
