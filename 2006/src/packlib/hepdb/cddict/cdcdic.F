*
* $Id: cdcdic.F,v 1.2 1999/08/17 12:42:31 mclareni Exp $
*
* $Log: cdcdic.F,v $
* Revision 1.2  1999/08/17 12:42:31  mclareni
* Y2k mods submitted by Rob Komar
*
* Revision 1.1.1.1  1996/02/28 16:24:08  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if !defined(CERNLIB__P3CHILD)
      SUBROUTINE CDCDIC (PATHX, ICMPF, IPREC, DELTA, IRC)
*     ===================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCDIC (PATHX, ICMPF, IPREC, DELTA, IRC*)               *
*                                                                      *
*   Create or updates the dictionary table for the Data base file      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHX    Path name of the subdirectory                           *
*     ICMPF    Compression flag (0 no compression; 1 standard          *
*              compression using IPREC; 2 compression with zero        *
*              supression with DELTA)                                  *
*     IPREC    Precision word; (If IPREC > 0, data are stored with     *
*              IPREC significant digits right to the decimal points;   *
*              if IPREC < 0, data are stored with IPREC insignificant  *
*              digits left to the decimal point.)                      *
*     DELTA    Variable specifying the absolute value below which data *
*              is treated as zero                                      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDMKDI                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =142 : Error in RZ in writing the dictionary object    *
*               =143 : Error in RZ in purging the dictionary directory *
*               =144 : Dictionary directory cannot be loaded           *
*               =145 : Pathname already exists in dictionary           *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHD*80, PATHX*(*)
*
*     ------------------------------------------------------------------
*
* *** The top directory information is assumed to be loaded
*     Top directory name is TOPNCD and LBUPCD the address of UPCD bank
*
      CALL RZCDIR (PAT3CT, 'R')
*
* *** Get the name of the name of directory without top name
*
      NCH    = LENOCC (PAT3CT)
      PAT3CT = PAT3CT(1:NCH)//'/'//PATHX
      CALL CDWTOP (PAT3CT, PAT2CT, NCHR)
      PATHD  = '//'//TOPNCD(1:NCHRCD)//'/'//'DICTIONARY'
      IF (IOUTCD.EQ.0) IOPSCD = 0
*
* *** Load the dictionary directory again just as a safe measure
*
      CALL RZCDIR (PATHD, ' ')
      IF (IQUEST(1).NE.0) THEN
        IRC    = 144
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCDIC : Cannot '//
     +  'set current directory to '//PATHD//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
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
          CALL CDKEYR (IMIN, NWKYCK, KEYNCK)
          LFIXCD = LQ(KOFUCD+LBUPCD-KLDICD)
          IF (LFIXCD.NE.0) CALL MZDROP (IDIVCD, LFIXCD, ' ')
          ICYCL  = 9999
          CALL CDRZIN (IDIVCD, LBUPCD, -KLDICD, IMIN, ICYCL, PATHD, IRC)
          IF (IRC.NE.0) GO TO 999
          LFIXCD = LQ(KOFUCD+LBUPCD-KLDICD)
          NDWD   = IQ(KOFUCD+LFIXCD-1)
          GO TO 10
        ENDIF
      ENDIF
*
* *** Recreate the dictionary directory
*
      CALL VZERO (KEYNCK, NSYSCK)
      KEYNCK(IDHKSN) = -1
      KEYNCK(IDHFLG) = 1
      CALL DATIME (IDATE, ITIME)
      CALL CDPKTM (IDATE,  ITIME,  KEYNCK(IDHINS), IRC)
      LFIXCD = LQ(KOFUCD+LBUPCD-KLDICD)
      IF (LFIXCD.NE.0) CALL MZDROP (IDIVCD, LFIXCD, ' ')
      NDWD   = NPUSCD*NWITCD + 1
      CALL CDBANK (IDIVCD, LFIXCD, LBUPCD, -KLDICD, 'DICD', 0, 0, NDWD,
     +             IODICD, 0, IRC)
      IF (IRC.NE.0)           GO TO 999
*
* *** Check if the path name already exists in the dictionary
*
   10 NITEM  = IQ(KOFUCD+LFIXCD+MDCNTM)
      NKEEP  = (NDWD - 1) / NWITCD
      NFREE  = 0
      IF (NITEM.GT.0) THEN
        DO 50 I = 1, NITEM
          IPNT   = KOFUCD + LFIXCD + (I - 1) * NWITCD + 1
          NCHF   = IQ(IPNT+MDCNCH)
          IF (IQ(IPNT+MDCITM).LE.0) THEN
            IF (NFREE.EQ.0) NFREE = I
          ELSE
            IF (NCHR.EQ.NCHF) THEN
              CALL UHTOC (IQ(IPNT+MDCNAM), 4, PAT3CT, NCHF)
              IF (PAT2CT(1:NCHR).EQ.PAT3CT(1:NCHF)) THEN
                IRC        = 145
                IQUEST(11) = I
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCDIC :'//
     +          ' Path name '//PAT2CT//' already in dictionary'')',IQ,0)
#endif
                GO TO 999
              ENDIF
            ENDIF
          ENDIF
   50   CONTINUE
      ENDIF
*
* *** Enter the new path name in the dictionary
*
      IF (NFREE.EQ.0) THEN
        NITEM  = NITEM + 1
        NFREE  = NITEM
        IF (NITEM.GT.NKEEP) THEN
          CALL ZSHUNT (IDIVCD, LFIXCD, LBDACD, 2, 0)
          LBDACD = LFIXCD
          LFIXCD = 0
          ND     = NDWD + NPUSCD * NWITCD
          CALL CDBANK (IDIVCD, LFIXCD, LBUPCD, -KLDICD, 'DICD', 0,0, ND,
     +                 IODICD, -1, IRC)
          IF (IRC.NE.0) THEN
            CALL MZDROP (IDIVCD, LBDACD, ' ')
            GO TO 999
          ENDIF
          CALL UCOPY (IQ(KOFUCD+LBDACD+1), IQ(KOFUCD+LFIXCD+1), NDWD)
          CALL MZDROP (IDIVCD, LBDACD, ' ')
        ENDIF
        IQ(KOFUCD+LFIXCD+MDCNTM) = NITEM
      ENDIF
      IPNT   = KOFUCD + LFIXCD + (NFREE - 1) * NWITCD + 1
      IQ(IPNT+MDCITM) = NFREE
      IQ(IPNT+MDCNCH) = NCHR
      IQ(IPNT+MDCLUP) = 0
      IQ(IPNT+MDCCMP) = ICMPF
      IQ(IPNT+MDCPRC) = IPREC
       Q(IPNT+MDCDEL) = DELTA
      CALL UCTOH ('        ', IQ(IPNT+MDCALI), 4, 8)
      CALL UCTOH (PAT2CT, IQ(IPNT+MDCNAM), 4, MAXLCD)
*
* *** Now update the disk file
*
      IF (IOPSCD.NE.0) CALL RZLOCK ('CDCDIC')
      CALL RZOUT (IDIVCD, LFIXCD, KEYNCK, ICYCLE, 'S')
      IF (IQUEST(1).NE.0) THEN
        IF (IOPSCD.NE.0) CALL RZFREE ('CDCDIC')
        IRC    = 142
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCDIC : RZOUT '//
     +  'error for path name '//PATHD//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
      NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
      CALL RZPURG (0)
      IERR   = IQUEST(1)
      IF (IOPSCD.NE.0) CALL RZFREE ('CDCDIC')
      IF (IERR.NE.0) THEN
        IRC    = 143
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCDIC : RZPURG '//
     +  'error for path name '//PATHD//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
  998 IRC    = 0
*                                                             END CDCDIC
  999 END
#endif
