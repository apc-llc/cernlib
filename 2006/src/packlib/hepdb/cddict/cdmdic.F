*
* $Id: cdmdic.F,v 1.1.1.1 1996/02/28 16:24:10 mclareni Exp $
*
* $Log: cdmdic.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:10  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDMDIC (TOPN, LTOP, JBIAS, IRC)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDMDIC (TOPN, LTOP, JBIAS, IRC*)                        *
*                                                                      *
*   Updates the dictionary information if not yet available in the     *
*   disk file                                                          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     TOPN     Name of the top directory                               *
*     LTOP     Support link for the dictionary bank                    *
*     JBIAS    Bias for the creating the dictionary bank in memory     *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDRDIC, CDUDIC                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =140 : Illegal Top directory name                      *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/cuserf.inc"
      CHARACTER       TOPN*(*)
      PARAMETER       (NLEVM=20)
      DIMENSION       LTOP(9), IHDIR(4), NCHD(NLEVM), ISDI(NLEVM)
      DIMENSION       NKEY(NLEVM), IOPT(NLEVM), NSDI(NLEVM)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      LREFCD(3) = LTOP(1)
      IRC    = 0
      NLEV   = 1
      PAT4CT = '//'//TOPN
      NCHD(NLEV) = LENOCC (PAT4CT)
      CALL RZCDIR (PAT4CT, 'Q')
      IF (IQUEST(1).NE.0) THEN
        IRC    = 140
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDMDIC : Illegal'//
     +  ' top directory '//PAT4CT//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      IOPTP  = 0
      IF (JBIAS.LE.0) THEN
        LFIXCD = LQ(KOFUCD+LREFCD(3)+JBIAS)
        IF (LFIXCD.NE.0) CALL MZDROP (IDIVCD, LFIXCD, ' ')
      ENDIF
      NDWD   = NPUSCD*NWITCD + 1
      CALL CDBANK (IDIVCD, LFIXCD, LREFCD(3), JBIAS, 'DICD', 0, 0, NDWD,
     +             IODICD, 0, IRC)
      IF (IRC.NE.0)                                        GO TO 999
      NITEM  = 0
      NKEEP  = NPUSCD
*
* *** Now scan down to find all the subdirectories
*
   10 IF (NLEV.GT.1) THEN
        PAT4CT = PAT4CT(1:NCHD(NLEV-1))//'/'//TOP1CT
        NCHD(NLEV) = NCHD(NLEV-1) + NCHR + 1
        CALL RZCDIR (PAT4CT, ' ')
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
   20 ISDI(NLEV) = ISDI(NLEV) + 1
      IF (ISDI(NLEV).LE.NSDI(NLEV)) THEN
*
*  **   If a new subdirectory go down one level
*
        LS    = IQ(KOFSCD+LCDRCD+KLSDCD)
        IPNT  = LS + 7 * (ISDI(NLEV) - 1)
        CALL ZITOH (IQ(KOFSCD+LCDRCD+IPNT), IHDIR, 4)
        CALL UHTOC (IHDIR, 4, TOP1CT, 16)
        NCHR  = INDEX (TOP1CT, ' ') - 1
        IF (NCHR.LE.0.OR.NCHR.GT.16) NCHR = 16
        IF (IOPTP.NE.0) THEN
          DO 35 IK = 1, NKEYCK
            KK     = IK
            IDIG   = 0
   30       IF (KK.GT.0) THEN
              KK     = KK / 10
              IDIG   = IDIG + 1
              GO TO 30
            ENDIF
            IF (NCHR.EQ.IDIG) THEN
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
              WRITE (PAT2CT, '(I8)') IK
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
              PAT2CT = ' '
              CALL UTWRIT (PAT2CT, '(I8)', IK ,1)
#endif
              I1     = 8 - IDIG + 1
              IF (PAT2CT(I1:8).EQ.TOP1CT)                  GO TO 20
            ENDIF
   35     CONTINUE
        ELSE IF (NLEV.EQ.1) THEN
          IF (NCHR.EQ.10) THEN
            IF (TOP1CT.EQ.'DICTIONARY')                    GO TO 20
          ENDIF
        ENDIF
        NLEV   = NLEV +1
        GO TO 10
*
      ELSE
*
*  **   Check if the name is to be entered
*
        IF (NLEV.EQ.1)                                     GO TO 60
        IF (NLEV.EQ.2) THEN
          IF ((NCHD(2)-NCHD(1)-1).EQ.10) THEN
            IF (PAT4CT(NCHD(1)+2:NCHD(2)).EQ.'DICTIONARY') GO TO 50
          ENDIF
        ENDIF
        PAT2CT = PAT4CT(NCHD(1)+1:NCHD(NLEV))
        NCHR   = NCHD(NLEV) - NCHD(1)
        NFREE  = 0
        IF (NITEM.GT.0) THEN
          DO 40 I = 1, NITEM
            IPNT   = KOFUCD + LFIXCD + (I - 1) * NWITCD + 1
            NCHF   = IQ(IPNT+MDCNCH)
            IF (IQ(IPNT+MDCITM).GT.0) THEN
              IF (NCHR.EQ.NCHF) THEN
                CALL UHTOC (IQ(IPNT+MDCNAM), 4, PAT3CT, NCHF)
                PAT3CT = PAT3CT(1:NCHF)
                IF (PAT2CT.EQ.PAT3CT)  GO TO 50
              ENDIF
            ELSE
              IF (NFREE.EQ.0) NFREE = I
            ENDIF
   40     CONTINUE
        ENDIF
*
*  **   Enter the new path name in the dictionary
*
        IF (NFREE.EQ.0) THEN
          NITEM  = NITEM + 1
          NFREE  = NITEM
          IF (NITEM.GT.NKEEP) THEN
            CALL ZSHUNT (IDIVCD, LFIXCD, LBDACD, 2, 0)
            LBDACD = LFIXCD
            LFIXCD = 0
            ND     = NDWD + NPUSCD * NWITCD
            CALL CDBANK (IDIVCD, LFIXCD, LREFCD(3), JBIAS, 'DICD', 0, 0,
     +                   ND, IODICD, -1, IRC)
            IF (IRC.NE.0) THEN
              CALL MZDROP (IDIVCD, LBDACD, ' ')
              GO TO 999
            ENDIF
            CALL UCOPY (IQ(KOFUCD+LBDACD+1), IQ(KOFUCD+LFIXCD+1), NDWD)
            CALL MZDROP (IDIVCD, LBDACD, ' ')
            NDWD   = IQ(KOFUCD+LFIXCD-1)
            NKEEP  = NKEEP + NPUSCD
          ENDIF
          IQ(KOFUCD+LFIXCD+MDCNTM) = NITEM
        ENDIF
        IPNT   = KOFUCD + LFIXCD + (NFREE - 1) * NWITCD + 1
        IQ(IPNT+MDCITM) = NFREE
        IQ(IPNT+MDCNCH) = NCHR
        IQ(IPNT+MDCLUP) = 0
        IQ(IPNT+MDCCMP) = IDCMCU
        IQ(IPNT+MDCPRC) = IPRECU
         Q(IPNT+MDCDEL) = DELTCU
        CALL UCTOH ('        ', IQ(IPNT+MDCALI), 4, 8)
        CALL UCTOH (PAT2CT, IQ(IPNT+MDCNAM), 4, NCHR)
*
   50   NLEV   = NLEV - 1
        IF (NLEV.GE.1) THEN
          LUP    = LQ(KOFSCD+LCDRCD+1)
          CALL MZDROP (0, LCDRCD, ' ')
          LCDRCD = LUP
          NKEYCK = NKEY(NLEV)
          IOPTP  = IOPT(NLEV)
          GO TO 20
        ENDIF
      ENDIF
*
   60 IRC    = 0
*                                                             END CDMDIC
  999 END
