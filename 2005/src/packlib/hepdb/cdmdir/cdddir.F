*
* $Id: cdddir.F,v 1.1.1.1 1996/02/28 16:24:15 mclareni Exp $
*
* $Log: cdddir.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:15  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDDDIR (PATHN, CHOPT, IRC)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDDIR (PATHN, CHOPT, IRC*)                             *
*                                                                      *
*   Deletes the directory from the current level downwards as given    *
*   in the path name                                                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Path name to be deleted                                 *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by user,   CDFZUP, CDKEEP                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =171 : Illegal Path name                               *
*               =172 : Cannot find the top directory for the path name *
*               =173 : Error in RZ for reading the dictionary object   *
*               =174 : Error in FZOUT for saving the journal file      *
*               =175 : Error in RZ in writing the dictionary object    *
*               =176 : Error in RZ in purging the dictionary directory *
*               =177 : Error in RZ in deleting the tree                *
*               =178 : Error in RZ in deleting Name/Help information   *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (NLEVM=20)
      DIMENSION       NCHD(NLEVM), ISDI(NLEVM)
      DIMENSION       NKEY(NLEVM), IOPT(NLEVM), NSDI(NLEVM)
      DIMENSION       IHDIR(4)
      CHARACTER       PATHN*(*), CHOPT*(*), PATHY*255, PATHL*255
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Decode the character option
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)                   GO TO 999
      KEY7   = KEY7CK
      KEY7CK = 0
      PATHL  = ' '
*
* *** Load the top directory information
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)                   GO TO 999
      PATHY  = PAT1CT
      NCHR   = LENOCC (PATHY)
      PAT3CT = '//'//TOPNCD(1:NCHRCD)//'/HELP'
      IF (PATHY.EQ.PAT3CT)            GO TO 999
      PAT2CT = '//'//TOPNCD(1:NCHRCD)//'/DICTIONARY'
      IF (PATHY.EQ.PAT2CT)            GO TO 999
      IF (IOPBCA.EQ.0) THEN
        LUFZCF = LUFZCD
      ELSE
        LUFZCF = LUBKCD
      ENDIF
*
* *** Find the dictionary record
*
      CALL RZCDIR (PAT2CT, ' ')
      IF (IQUEST(1).NE.0) THEN
        LFIXCD = 0
        IRC    = 173
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDDIR : '//
     +  'RZCDIR error for path name '//PAT2CT//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      ISTP   = NWKYCK + 1
      IF (NKEYCK.GT.0) THEN
        IPNT   = KOFSCD + LCDRCD + IKDRCD
        IMIN   = IUHUNT (-1, IQ(IPNT+IDHKSN), NKEYCK*ISTP, ISTP)
        IF (IMIN.GT.0) THEN
          IMIN   = (IMIN - IDHKSN) / ISTP + 1
          CALL CDKEYT
          CALL CDKEYR (IMIN, NWKYCK, KEYVCK)
          IF (LQ(KOFUCD+LBUPCD-KLDICD).LE.0) THEN
            CALL CDRZIN (IDIVCD, LBUPCD,-KLDICD, IMIN,ICYCL, PAT2CT,IRC)
            IF (IRC.NE.0)             GO TO 999
          ENDIF
          LFIXCD = LQ(KOFUCD+LBUPCD-KLDICD)
        ELSE
          LFIXCD = 0
        ENDIF
      ELSE
        LFIXCD = 0
      ENDIF
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
      IF (IOPPCD.NE.0) THEN
        IRC    = 0
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
        CALL CDWLOK (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
        CALL CDSTSV (TOPNCD, 0, IRC)
#endif
#if (defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__SERVER))
        LUFZCF = LODBP3
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        IF (IRC.NE.0)                 GO TO 999
      ENDIF
#endif
*
* *** Prepare the journal file if needed
*
      IF (LUFZCF.GT.0) THEN
        IF (IOPBCA.EQ.0) THEN
          NWDOP  = 0
        ELSE
          NWDOP  = 1
        ENDIF
        NDOP   = (NCHR + 3) / 4
        IHEDCF(MACTCF) = 4
        IHEDCF(MNKYCF) = 0
        IHEDCF(MOPTCF) = NWDOP
        IHEDCF(MPATCF) = NDOP
        IHEDCF(MPRECF) = 0
        IF (KEY7.LE.0) THEN
          CALL DATIME (IDATE, ITIME)
          CALL CDPKTM (IDATE, ITIME, KEY7, IRC)
        ENDIF
        IHEDCF(MINSCF) = KEY7
        IF (NWDOP.EQ.1) CALL UCTOH ('B   ', IHEDCF(MINSCF+1), 4, 4)
        CALL UCTOH (PATHY, IHEDCF(MINSCF+NWDOP+1), 4, 4*NDOP)
        CALL MZIOCH (IOFMCF, NWFMCF, '6I -H')
        NWDH   = NWDOP + NDOP + MINSCF
*
*  **   Now write on the sequential output
*
#if defined(CERNLIB__P3CHILD)
        RNDBP3 = 'CDDDIR  '
        NWDBP3 = 2
        CALL UCTOH ('JOURNAL ', IWDBP3, 4, 8)
        CALL CDCHLD
        IRC    = IQDBP3
        IF (IRC.NE.0)                 GO TO 999
#endif
        CALL FZOUT (LUFZCF, IDIVCD, 0, 1, 'Z', IOFMCF, NWDH, IHEDCF)
        IF (IQUEST(1).NE.0) THEN
          IRC    = 174
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDDIR : FZOUT'//
     +    ' error for path name '//PATHY(1:NCHR)//''')', IARGCD, 0)
#endif
          GO TO 999
        ENDIF
      ENDIF
*
      IRC    = 0
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
* *** Server environment, Public mode
*
      IF (IOPPCD.NE.0) THEN
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
        CALL CDCWSV (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        GO TO 999
      ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
*
* *** Find the level below which all directories are deleted
*
      NLEV   = 1
      NCHD(NLEV) = NCHR
      NCHL   = 0
      DO 10 I = 1, NCHR
        NCH    = NCHR - I + 1
        IF (PATHY(NCH:NCH).EQ.'/') THEN
          NCHL   = NCH
          GO TO 15
        ENDIF
   10 CONTINUE
   15 IF (NCHL.LE.2)                  GO TO 999
      CALL RZCDIR (PATHY, ' ')
      IF (IQUEST(1).NE.0) THEN
        IRC    = 171
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDDIR : Illegal'//
     +  ' path name '//PATHY(1:NCHR)//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
        GO TO 999
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      IF (KEY7.GT.0) THEN
        CALL CDPKTM (IQUEST(16), IQUEST(17), KEY7N, IRC)
        IF (KEY7.LT.KEY7N)            GO TO 999
      ENDIF
*
* *** Update the dictionary information if it exists
*
      IF (LFIXCD.GT.0) THEN
        NITEM  = IQ(KOFUCD+LFIXCD+MDCNTM)
        NCHAR  = 0
        CALL VZERO (IPURCK, NITEM)
        NDEL   = 0
        IF (NKEYCK.GT.0) THEN
          IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
        ELSE
          IOPTP  = 0
        ENDIF
*
*  **   Now scan down to find all the subdirectories
*
   20   IF (NLEV.GT.1) THEN
          PATHY  = PATHY(1:NCHD(NLEV-1))//'/'//TOP1CT
          NCHD(NLEV) = NCHD(NLEV-1) + NCHAR + 1
          CALL RZCDIR (PATHY, ' ')
          NKEYCK = IQUEST(7)
          NWKYCK = IQUEST(8)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          IF (NKEYCK.GT.0) THEN
            IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
          ELSE
            IOPTP  = 0
          ENDIF
        ENDIF
        NKEY(NLEV) = NKEYCK
        IOPT(NLEV) = IOPTP
        ISDI(NLEV) = 0
        NSDI(NLEV) = IQ(KOFSCD+LCDRCD+KNSDCD)
*
   25   ISDI(NLEV) = ISDI(NLEV) + 1
        IF (ISDI(NLEV).LE.NSDI(NLEV)) THEN
*
*  **     If a new subdirectory go down one level
*
          LS    = IQ(KOFSCD+LCDRCD+KLSDCD)
          IPNT  = LS + 7 * (ISDI(NLEV) - 1)
          CALL ZITOH (IQ(KOFSCD+LCDRCD+IPNT), IHDIR, 4)
          CALL UHTOC (IHDIR, 4, TOP1CT, 16)
          NCHAR = LENOCC (TOP1CT)
          IF (IOPTP.NE.0) THEN
            DO 35 IK = 1, NKEYCK
              KK     = IK
              IDIG   = 0
   30         IF (KK.GT.0) THEN
                KK     = KK / 10
                IDIG   = IDIG + 1
                GO TO 30
              ENDIF
              IF (NCHAR.EQ.IDIG) THEN
                WRITE (PAT4CT, '(I8)') IK
                I1     = 8 - IDIG + 1
                TOP2CT = PAT4CT(I1:8)
                IF (TOP1CT.EQ.TOP2CT) GO TO 25
              ENDIF
   35       CONTINUE
          ENDIF
          NLEV   = NLEV +1
          GO TO 20
*
        ELSE
*
*  **     Check if the name is to be deleted
*
          PAT1CT = PATHY(NCHRCD+3:NCHD(NLEV))
          NCHRU  = NCHD(NLEV) - NCHRCD - 2
          IF (NITEM.GT.0) THEN
            DO 40 I = 1, NITEM
              IPNT   = KOFUCD + LFIXCD + (I - 1) * NWITCD + 1
              NCHF   = IQ(IPNT+MDCNCH)
              IF (NCHRU.EQ.NCHF) THEN
                CALL UHTOC (IQ(IPNT+MDCNAM), 4, PAT4CT, NCHF)
                PAT4CT = PAT4CT(1:NCHF)
                IF (PAT1CT.EQ.PAT4CT) THEN
                  IF (IQ(IPNT+MDCITM).GT.0) THEN
                    NDEL   = NDEL + 1
                    IPURCK(NDEL) = IQ(IPNT+MDCITM)
                  ENDIF
                  IQ(IPNT+MDCITM) = -1
                  GO TO 45
                ENDIF
              ENDIF
   40       CONTINUE
          ENDIF
*
   45     NLEV   = NLEV - 1
          IF (NLEV.GE.1) THEN
            LUP    = LQ(KOFSCD+LCDRCD+1)
            CALL MZDROP (0, LCDRCD, ' ')
            LCDRCD = LUP
            NKEYCK = NKEY(NLEV)
            IOPTP  = IOPT(NLEV)
            GO TO 25
          ENDIF
        ENDIF
*
*  **   Delete help information if any
*
        IF (NDEL.GT.0) THEN
          CALL RZCDIR (PAT3CT, ' ')
          IF (IQUEST(1).EQ.0) THEN
            NKEYCK = IQUEST(7)
            NWKYCK = IQUEST(8)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            ISTP   = NWKYCK + 1
            IF (NKEYCK.GT.0) THEN
              IPNT   = KOFSCD + LCDRCD + IKDRCD
              NKEYS  = 0
              DO 50 I = 1, NDEL
                IMIN   = IUHUNT (IPURCK(I), IQ(IPNT+IDHKSN),
     +                           NKEYCK*ISTP, ISTP)
                IF (IMIN.GT.0) THEN
                  NKEYS  = NKEYS + 1
                  KEY1CK(NKEYS) = (IMIN - IDHKSN) / ISTP + 1
                ENDIF
   50         CONTINUE
              IF (NKEYS.GT.0) THEN
                CALL SORTZV (KEY1CK(1), KEY1CK(NKEYS+1), NKEYS, -1, 0,0)
                IF (IOPSCD.NE.0) THEN
                  CALL RZLOCK ('CDDDIR')
                  PATHL  = PAT3CT
                ENDIF
                CALL CDKEYT
                DO 55 I = 1, NKEYS
                  II     = KEY1CK(2*NKEYS-I+1)
                  IK     = KEY1CK(II)
                  CALL CDKEYR (IK, NWKYCK, KEYNCK)
                  CALL RZDELK (KEYNCK, ICDUM, 'C')
                  IF (IQUEST(1).NE.0) THEN
                    IRC    = 178
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
                    IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD,
     +              '(/,'' CDDDIR : RZDELK error for path name '//PAT3CT
     +              //''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
                    GO TO 998
                  ENDIF
   55           CONTINUE
              ENDIF
            ENDIF
          ENDIF
        ENDIF
*
*  **   Lock the dictionary directory if necessary
*
        CALL RZCDIR (PAT2CT, ' ')
        NKEYCK = IQUEST(7)
        NWKYCK = IQUEST(8)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        ISTP   = NWKYCK + 1
        IF (IOPSCD.NE.0) THEN
          CALL RZLOCK ('CDDDIR')
          PATHL  = PAT2CT
        ENDIF
*
*  **   Delete name information if any
*
        IF (NDEL.GT.0.AND.NKEYCK.GT.0) THEN
          IPNT   = KOFSCD + LCDRCD + IKDRCD
          NKEYS  = 0
          DO 60 I = 1, NDEL
            IMIN   = IUHUNT (IPURCK(I), IQ(IPNT+IDHKSN), NKEYCK*ISTP,
     +                       ISTP)
            IF (IMIN.GT.0) THEN
              NKEYS  = NKEYS + 1
              KEY1CK(NKEYS) = (IMIN - IDHKSN) / ISTP + 1
            ENDIF
   60     CONTINUE
          IF (NKEYS.GT.0) THEN
            CALL SORTZV (KEY1CK(1), KEY1CK(NKEYS+1), NKEYS, -1, 0, 0)
            CALL CDKEYT
            DO 65 I = 1, NKEYS
              II     = KEY1CK(2*NKEYS-I+1)
              IK     = KEY1CK(II)
              CALL CDKEYR (IK, NWKYCK, KEYNCK)
              CALL RZDELK (KEYNCK, ICDUM, 'C')
              IF (IQUEST(1).NE.0) THEN
                IRC    = 178
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDDIR :'//
     +          'RZDELK error for path name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
                GO TO 998
              ENDIF
   65       CONTINUE
          ENDIF
        ENDIF
*
*  **   All subdirectories looked at; now store dictionary
*
        CALL RZOUT (IDIVCD, LFIXCD, KEYVCK, ICYCLE, 'S')
        IF (IQUEST(1).NE.0) THEN
          IRC    = 175
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDDIR : RZOUT'//
     +    ' error for path name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 998
        ENDIF
        IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
        NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
        CALL RZPURG (0)
        IF (IQUEST(1).NE.0) THEN
          IRC    = 176
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDDIR : RZPUR'//
     +    'G error for path name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 998
        ENDIF
*
*  **   Free the directory if locked
*
        IF (PATHL.NE.' ') THEN
          CALL RZCDIR (PATHL, ' ')
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          CALL RZFREE ('CDDDIR')
          PATHL  = ' '
        ENDIF
      ENDIF
*
  100 IQUEST(1) = 0
      PAT4CT = PATHY(NCHL+1:NCHR)
      PAT1CT = PATHY(1:NCHL-1)
*
*  ** Lock the directory if necessary
*
      CALL RZCDIR (PAT1CT, ' ')
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      IF (IOPSCD.NE.0) THEN
        CALL RZLOCK ('CDDDIR')
        PATHL  = PAT1CT
      ENDIF
      CALL RZDELT (PAT4CT)
      IF (IQUEST(1).NE.0) THEN
        IRC    = 177
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDDIR : RZDELT '//
     +  'error for path name '//PATHY(1:NCHR)//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
      ENDIF
*
* *** Free the directory if locked
*
  998 IF (PATHL.NE.' ') THEN
        CALL RZCDIR (PATHL, ' ')
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        CALL RZFREE ('CDDDIR')
      ENDIF
#endif
*                                                             END CDDDIR
  999 END
