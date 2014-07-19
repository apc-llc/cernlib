*
* $Id: cddish.F,v 1.1.1.1 1996/02/28 16:24:47 mclareni Exp $
*
* $Log: cddish.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDISH (LUN, PATHN, MINKY, MAXKY, IFLG, CHOPT, IRC)
*     ==============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDISH (LUN, PATHN, MINKY, MAXKY, IFLG, CHOPT, IRC*)    *
*                                                                      *
*   Displays the keys in horizontal mode                               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Unit number of file for display                         *
*     PATHN    Character string describing the pathname                *
*     MINKY    Minimum object number to be displayed (for IFLG=0)      *
*     MAXKY    Maximum object number to be displayed (for IFLG=0)      *
*     IFLG     Flag to decide if objects (=0) or only template (=1)    *
*              to be shown                                             *
*     CHOPT    Character with any of the follwing characters           *
*          K   Display all the keys (Default)                          *
*          U   Display only user keys and validities                   *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDDISP                                                   *
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
#include "hepdb/clinks.inc"
#include "hepdb/ckkeys.inc"
      PARAMETER       (JBIAS=2)
      CHARACTER       PATHN*(*), CHOPT*(*), CHEAD*300
      DIMENSION       IDPFL(100), ITIME(MXPACD)
*
*     ------------------------------------------------------------------
*
      NCHR   = LENOCC (PATHN)
      IRC    = 0
      WRITE (LUN, 1001) PATHN(1:NCHR)
      IF (IFLG.NE.0) GO TO 20
*
* *** Range of the keys to be displayed
*
    5 CONTINUE
      IF (CHOPT.EQ.'U') THEN
        CALL CDDPRG (IDPFL, NWKYCK, 'EDIT')
      ELSE
        CALL CDDPRG (IDPFL, NWKYCK, 'TERM')
      ENDIF
      CALL CDRGCK (IDPFL, NWKYCK, NDISP, IOTYCC)
      IF (NDISP.GT.MXDPCC) THEN
        CALL CDPRNT (LPRTCD, '(''   CDDISH : Not enough space to disp'//
     +       'lay.'',/,''   Enlarge display range by CDSETD or use V-'//
     +       'mode.'')', IARGCD, 0)
        IF (CHOPT.EQ.'U') THEN
          WRITE (LUN, 1002)
          GO TO 999
        ENDIF
        GO TO 5
      ENDIF
*
*  ** Display header with a template
*
      CHEAD  = ' '
      CALL CDHEAD (IOTYCC(1), NWKYCK, CHEAD)
      CALL CDTEMP (IOTYCC(1), NWKYCK, CHEAD)
      WRITE (LUN, 1003) CHEAD(1:MXDPCC)
*
*  ** Display keys
*
      IOLD1  = IOKYCA(IDHKSN)
      IOLDK  = IOPKCA
      IOPKCA = 1
      IOKYCA(IDHKSN) = 1
      DO 10 I = 1, NPARCD
        ITIME(I) = 1
   10 CONTINUE
      DO 15 IKEY = MINKY, MAXKY
        KEYVCK(IDHKSN) = IKEY
        CALL CDKXIN (ITIME, IDISCD, LAUXCL(7), LAUXCL(7), JBIAS, NWKEY,
     +               KEYVCK, IPREC, IRC)
        IF (IRC.EQ.0) THEN
          CALL CDDKYH (LUN, NWKYCK, KEYVCK, IOTYCC, ICONCK, IKEY, IRC)
        ENDIF
   15 CONTINUE
*
      IRC    = 0
      IOKYCA(IDHKSN) = IOLD1
      IOPKCA = IOLDK
      WRITE (LUN, 1004)
      GO TO 999
*
* *** Display the template
*
   20 CONTINUE
      CALL CDDPRG (IDPFL, NWKYCK, ' ')
      CALL CDRGCK (IDPFL, NWKYCK, NDISP, IOTYCC)
      IF (NDISP.GT.MXDPCC) THEN
        CALL CDPRNT (LPRTCD, '(''   CDDISH : Not enough space to disp'//
     +       'lay.'',/,''   Enlarge display range by CDSETD or use V-'//
     +       'mode.'')', IARGCD, 0)
        WRITE (LUN, 1002)
        GO TO 999
      ENDIF
*
*  ** Display header
*
      CHEAD  = ' '
      CALL CDHEAD (IOTYCC(1), NWKYCK, CHEAD)
      WRITE (LUN, 1003) CHEAD(1:MXDPCC)
*
*  ** Display template
*
      CHEAD  = 'N'
      CHEAD(9:9) = '1'
      CALL CDTEMP (IOTYCC(1), NWKYCK, CHEAD)
      WRITE (LUN, 1003) CHEAD(1:MXDPCC)
      WRITE (LUN, 1005)
      IRC    = 0
      WRITE (LUN, 1004)
*
 1001 FORMAT ('   Keys in Directory to be displayed/updated: '/A)
 1002 FORMAT ('   CDDISH : Not enough space to display.'
     +       /'   Enlarge display range by CDSETD or use V-mode.')
 1003 FORMAT (/A)
 1004 FORMAT ('    We have reached the End of File Position')
 1005 FORMAT ('    The Directory does not contain any object  '
     +       /'    Insert the value of the keys for the first object'
     +       /'    between the vertical bars on the template')
*                                                             END CDDISH
  999 END
