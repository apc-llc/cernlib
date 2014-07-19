*
* $Id: cdfpat.F,v 1.1.1.1 1996/02/28 16:24:15 mclareni Exp $
*
* $Log: cdfpat.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:15  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if (defined(CERNLIB__P3CHILD))&&(defined(CERNLIB_IBM))
* Ignoring t=dummy
#endif
      SUBROUTINE CDFPAT (PATHN, LEVMX, LAD, IRC)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDFPAT (PATHN, LEVMX, LAD*, IRC*)                       *
*                                                                      *
*   Store the information of the directory tree PATHN in a bank        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    The pathname of the directory tree about which the      *
*              information has to be provided                          *
*     LEVMX    Number of levels below PATHN about which information    *
*              has to be accumulated                                   *
*     LAD      Address of the bank containing the information          *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDKEEP                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =212 : Illegal path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (NLEVM=20, NWITM=28, NPUSH=10)
      PARAMETER       (NWLEV=8, NWNOD=6)
      DIMENSION       NKEY(NLEVM), NCHD(NLEVM), ISDI(NLEVM)
      DIMENSION       NSDI(NLEVM), IOPT(NLEVM)
      DIMENSION       NODES(NLEVM), IHDIR(4), LAD(9)
      CHARACTER       PATHN*(*), PATHY*255
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      IF (LAD(1).NE.0) THEN
        CALL MZDROP (IDIVCD, LAD(1), 'L')
        LAD(1) = 0
      ENDIF
      IF (LEVMX.LE.0) THEN
        LEVEL  = NLEVM
      ELSE
        LEVEL  = LEVMX
      ENDIF
      CALL VZERO (NODES, NLEVM)
      CALL CDSBLC (PATHN, PATHY, NCHAR)
      CALL RZCDIR (PAT4CT, 'R')
      CALL RZCDIR (PATHY,  ' ')
      IF (IQUEST(1).NE.0) THEN
        IRC    = 212
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFPAT : Illegal'//
     +  ' path name '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 100
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      IF (NKEYCK.LE.0) THEN
        IOPTP  = 0
      ELSE
        IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      ENDIF
      CALL RZCDIR (PATHY, 'R')
      NCHR   = LENOCC (PATHY)
      NLEV   = 1
      NCHD(NLEV) = NCHR
*
* *** Create a temporary bank for storing information
*
      NDWD   = NPUSH * NWITM
      NKEEP  = NPUSH
      NITEM  = 0
      NLEVT  = NLEV
      NWDSN  = 0
      CALL CDBANK (IDIVCD, LFIXCD, LFIXCD, 2, 'TEMP', 0, 0, NDWD, 0, -1,
     +             IRC)
      IF (IRC.NE.0)                       GO TO 100
*
* *** Now scan down to find all the subdirectories
*
   10 IF (NLEV.GT.1) THEN
        PATHY  = PATHY(1:NCHD(NLEV-1))//'/'//TOP1CT
        NCHD(NLEV) = NCHD(NLEV-1) + NCHR + 1
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
      IF (NLEV.EQ.LEVEL) NSDI(NLEV) = 0
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
        NCHR  = LENOCC (TOP1CT)
        IF (IOPTP.NE.0) THEN
          DO 30 IK = 1, NKEYCK
            KK     = IK
            IDIG   = 0
   25       IF (KK.GT.0) THEN
              KK     = KK / 10
              IDIG   = IDIG + 1
              GO TO 25
            ENDIF
            IF (NCHR.EQ.IDIG) THEN
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
              WRITE (PAT2CT, '(I8)') IK
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
              CALL UTWRIT (PAT2CT, '(I8)', IK, 1)
#endif
              I1     = 8 - IDIG + 1
              TOP2CT = PAT2CT(I1:8)
              IF (TOP2CT.EQ.TOP1CT)       GO TO 20
            ENDIF
   30     CONTINUE
        ENDIF
        NLEV   = NLEV +1
        IF (NLEV.GT.NLEVT) NLEVT = NLEV
        GO TO 10
*
      ELSE
*
*  **   Enter the new path name in the temporary space
*
        IF (NLEV.EQ.1) THEN
          NSTR   = 2
          DO 35 I = 3, NCHD(1)
            IF (PATHY(I:I).EQ.'/') NSTR = I
   35     CONTINUE
          TOP2CT = PATHY(NSTR+1:NCHD(NLEV))
          NCHR   = NCHD(NLEV) - NSTR
          NCHRT  = NSTR - 1
          PAT3CT = PATHY(1:NCHRT)
        ELSE
          TOP2CT = PATHY(NCHD(NLEV-1)+2:NCHD(NLEV))
          NCHR   = NCHD(NLEV) - NCHD(NLEV-1) - 1
          PAT3CT = PATHY(1:NCHD(NLEV-1))
          NCHRT  = NCHD(NLEV-1)
        ENDIF
        NODES(NLEV) = NODES(NLEV) + 1
*
        NITEM  = NITEM + 1
        IF (NITEM.GT.NKEEP) THEN
          LBDACD = LFIXCD
          LFIXCD = 0
          ND     = NDWD + NPUSH * NWITM
          CALL CDBANK (IDIVCD, LFIXCD, LFIXCD, 2, 'TEMP', 0,0, ND, 0,-1,
     +                 IRC)
          IF (IRC.NE.0) THEN
            CALL MZDROP (IDIVCD, LBDACD, ' ')
            GO TO 100
          ENDIF
          CALL UCOPY (IQ(KOFUCD+LBDACD+1), IQ(KOFUCD+LFIXCD+1), NDWD)
          CALL MZDROP (IDIVCD, LBDACD, ' ')
          NDWD   = IQ(KOFUCD+LFIXCD-1)
          NKEEP  = NKEEP + NPUSH
        ENDIF
        NWDN   = (NCHR + 3) / 4
        NWDSN  = NWDSN + NWDN
        IPNT   = KOFUCD + LFIXCD + (NITEM - 1) * NWITM
        IQ(IPNT+1) = NLEV
        IQ(IPNT+2) = NODES(NLEV)
        IQ(IPNT+3) = NCHR
        IQ(IPNT+4) = NCHRT
        CALL UCTOH (TOP2CT, IQ(IPNT+5), 4, 16)
        CALL UCTOH (PAT3CT, IQ(IPNT+9), 4, 80)
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
* *** All subdirectories looked at; now store dictionary if permitted
*
   60 IF (NLEVT.GT.0) THEN
        ND     = 3 + NWLEV*NLEVT + NWNOD*NITEM + NWDSN
        CALL CDBANK (IDIVCD, LAD(1), LAD(1), 2, 'NAME', 0, 0, ND, 0, -1,
     +                 IRC)
        IF (IRC.NE.0) THEN
          CALL MZDROP (IDIVCD, LFIXCD, ' ')
          GO TO 100
        ENDIF
        IOFF   = KOFUCD + LAD(1)
        KPNTL  = 3
        IPNTL  = KPNTL
        IQ(IOFF+MPNLCD) = KPNTL
        IQ(IOFF+MNLVCD) = NLEVT
         Q(IOFF+MHFMCD) = 0.
        IQ(IOFF+KPNTL+MPPLCD) = 0
        DO 90 ILEV = 1, NLEVT
          KPNTN  = KPNTL + NWLEV
          IPNTN  = KPNTN
          NODET  = 0
          DO 80 IN = 1, NITEM
            IPNT   = KOFUCD + LFIXCD + (IN - 1) * NWITM
            IF (IQ(IPNT+1).NE.ILEV)       GO TO 80
            NODE   = IQ(IPNT+2)
            NCHR   = IQ(IPNT+3)
            NCHRT  = IQ(IPNT+4)
            CALL UHTOC (IQ(IPNT+5), 4, TOP1CT, 16)
            CALL UHTOC (IQ(IPNT+9), 4, PAT2CT, 80)
            PAT3CT = PAT2CT(1:NCHRT)//'/'//TOP1CT
            NCHRP  = NCHRT + NCHR + 1
            IUP    = 0
            IF (ILEV.GT.1) THEN
              DO 65 IM = 1, NITEM
                IPNT   = KOFUCD + LFIXCD + (IM - 1) * NWITM
                IF (IQ(IPNT+1).NE.ILEV-1) GO TO 65
                NCHRU  = IQ(IPNT+3)
                NCHRF  = IQ(IPNT+4)
                IF (NCHRT.EQ.NCHRU+NCHRF+1) THEN
                  CALL UHTOC (IQ(IPNT+5), 4, TOP2CT, 16)
                  CALL UHTOC (IQ(IPNT+9), 4, PAT2CT, 80)
                  PAT2CT = PAT2CT(1:NCHRF)//'/'//TOP2CT
                  IF (PAT3CT(1:NCHRT).EQ.PAT2CT(1:NCHRT)) THEN
                    IUP    = IQ(IPNT+2)
                    GO TO 70
                  ENDIF
                ENDIF
   65         CONTINUE
            ENDIF
   70       NDOWN   = 0
            IF (ILEV.LT.NLEVT) THEN
              DO 75 IM = 1, NITEM
                IPNT   = KOFUCD + LFIXCD + (IM - 1) * NWITM
                IF (IQ(IPNT+1).NE.ILEV+1) GO TO 75
                NCHRU  = IQ(IPNT+4)
                IF (NCHRP.EQ.NCHRU) THEN
                  CALL UHTOC (IQ(IPNT+9), 4, PAT2CT, 80)
                  IF (PAT3CT(1:NCHRP).EQ.PAT2CT(1:NCHRP)) NDOWN=NDOWN+1
                ENDIF
   75         CONTINUE
            ENDIF
            NWDN   = (NCHR + 3) / 4
            NTOT   = NWDN + NWNOD
            NODET  = NODET + 1
            IPNTN  = KPNTN
            KPNTN  = IPNTN + NTOT
            IQ(IOFF+IPNTN+MPNLCD) = KPNTN
            IQ(IOFF+IPNTN+MNNUCD) = NODE
            IQ(IOFF+IPNTN+MNFNCD) = IUP
            IQ(IOFF+IPNTN+MNDWCD) = NDOWN
            IQ(IOFF+IPNTN+MNCHCD) = NCHR
             Q(IOFF+IPNTN+MYFNCD) = 0.
            CALL UCTOH (TOP1CT, IQ(IOFF+IPNTN+MNAMCD), 4, NCHR)
   80     CONTINUE
          IPNTL  = KPNTL
          KPNTL  = KPNTN
          IQ(IOFF+IPNTN+MPNLCD) = 0
          IQ(IOFF+IPNTL+MLEVCD) = ILEV
          IQ(IOFF+IPNTL+MNODCD) = NODET
           Q(IOFF+IPNTL+MXOFCD) = 0.
           Q(IOFF+IPNTL+MXWDCD) = 0.
           Q(IOFF+IPNTL+MYFLCD) = 0.
          IF (ILEV.LT.NLEVT) IQ(IOFF+KPNTL+MPPLCD) = IPNTL
          IF (NODET.GT.0) THEN
            IQ(IOFF+IPNTL+MPNNCD) = IPNTL + NWLEV
            IQ(IOFF+IPNTL+MPNLCD) = KPNTL
          ELSE
            IQ(IOFF+IPNTL+MPNNCD) = 0
            IQ(IOFF+IPNTL+MPNLCD) = KPNTL
          ENDIF
   90   CONTINUE
        IQ(IOFF+IPNTL+MPNLCD) = 0
      ENDIF
      IF (LFIXCD.GT.0) CALL MZDROP (IDIVCD, LFIXCD, ' ')
*
      IRC    = 0
  100 CALL RZCDIR (PAT4CT, ' ')
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
*                                                             END CDFPAT
  999 END
