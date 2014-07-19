*
* $Id: cddisv.F,v 1.1.1.1 1996/02/28 16:24:48 mclareni Exp $
*
* $Log: cddisv.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:48  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDISV (LUN, PATHN, MINKY, MAXKY, IFLG, CHOPT, IRC)
*     ==============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDISV (LUN, PATHN, MINKY, MAXKY, IFLG, CHOPT, IRC*)    *
*                                                                      *
*   Displays the keys in vertical mode                                 *
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
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
      PARAMETER       (JBIAS=2)
      CHARACTER       DEFST(6)*20, CHPRT*40, CFORS(6)*1
      CHARACTER       PATHN*(*), CHOPT*(*)
      DIMENSION       ITIME(MXPACD)
*
      DATA         DEFST /'                    ','     1              ',
     +                    '                    ','810101          0   ',
     +                    '921231     235959   ','                    '/
      DATA            CHPRT /'  Object Number :               ==>    D'/
      DATA            CFORS  /'B', 'I', 'H', 'S', 'M', 'U'/
*
*     ------------------------------------------------------------------
*
      NCHR   = LEN (PATHN)
      WRITE (LUN, 1001) PATHN(1:NCHR)
*
      IF (IOPXCA.NE.0) THEN
        INDXT  = 7
      ELSE
        INDXT  = 2
      ENDIF
      DO 5 I = 1, 2*NPARCD
        IOTYCC(NOF1CK+I) = INDXT
    5 CONTINUE
      IOTYCC(IDHINS) = 8
      IF (CHOPT.EQ.'U'.OR.IFLG.NE.0) THEN
        IOTYCC(IDHKSN) = 9
        IOTYCC(IDHPTR) = 9
        IOTYCC(IDHFLG) = 9
      ENDIF
      IF (IFLG.NE.0)     GO TO 20
*
*  ** Display keys
*
      CHPRT(40:40) = 'D'
      IOLD1  = IOKYCA(IDHKSN)
      IOLDK  = IOPKCA
      IOPKCA = 1
      IOKYCA(IDHKSN) = 1
      DO 10 I = 1, NPARCD
        ITIME(I)  = 1
   10 CONTINUE
      DO 15 IKEY = MINKY, MAXKY
        KEYVCK(IDHKSN) = IKEY
        CALL CDKXIN (ITIME, IDISCD, LAUXCL(7), LAUXCL(7), JBIAS, NWKEY,
     +               KEYVCK, IPREC, IRC)
        IF (IRC.EQ.0) THEN
          WRITE (CHPRT(21:30), 1002) IKEY
          WRITE (LUN, 1003) CHPRT
          CALL CDDKYV (LUN, NWKYCK, CTAGCK, KEYVCK, IOTYCC, ICONCK, IRC)
        ENDIF
   15 CONTINUE
*
      IRC    = 0
      IOKYCA(IDHKSN) = IOLD1
      IOPKCA = IOLDK
      WRITE (LUN, 1004)
      GO TO 999
*
* *** No keys prepare the template
*
   20 IKEY   = 1
      CHPRT(40:40) = 'N'
      WRITE (CHPRT(21:30), 1002) IKEY
      WRITE (LUN, 1003) CHPRT
      IPR    = 1
   25 IF (IPR.LE.NWKYCK) THEN
        NREP   = 0
        IF (IOTYCC(IPR).EQ.1) THEN
          ITYP   = 1
        ELSE IF (IOTYCC(IPR).EQ.5.OR.IOTYCC(IPR).EQ.6) THEN
          ITYP   = 3
          NREP   = ICONCK(IPR)
        ELSE IF (IOTYCC(IPR).GE.7.AND.IOTYCC(IPR).LE.9) THEN
          ITYP   = IOTYCC(IPR) - 3
        ELSE
          ITYP   = 2
        ENDIF
        WRITE (LUN, 1005) CTAGCK(IPR), CFORS(ITYP), DEFST(ITYP)
        IPR   = IPR + NREP + 1
        GO TO 25
      ENDIF
      WRITE (LUN, 1006)
      IRC    = 0
*
 1001 FORMAT ('   Keys in Directory to be displayed/updated: '/A)
 1002 FORMAT (I10)
 1003 FORMAT (A)
 1004 FORMAT ('    We have reached the End of File Position')
 1005 FORMAT ('  ',A8,'  ',A1,'                ',A)
 1006 FORMAT ('   The Directory does not contain any object - '
     +       /'  The above line contains the default values for the'
     +       /'  system keys and a format specification for the others')
*                                                             END CDDISV
  999 END
