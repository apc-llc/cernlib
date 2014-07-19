*
* $Id: cdildu.F,v 1.1.1.1 1996/02/28 16:24:12 mclareni Exp $
*
* $Log: cdildu.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:12  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if !defined(CERNLIB__P3CHILD)
      SUBROUTINE CDILDU (LUNI, TOPNM, CHOPT, IRC)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDILDU (LUNI, TOPNM, CHOPT, IRC*)                       *
*                                                                      *
*   Initializes the List of Directories to be Updated from the         *
*   journal file.                                                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUNI     Logical unit number from which the list is read off     *
*     TOPNM    Name of the Top Directory                               *
*     CHOPT    Character string with any of the following characters   *
*          A   Add to the previous information                         *
*          I   Ignore all previous information (also current file)     *
*     Default  Overwrite previous information with the current list    *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =135 : Illegal top directory name                      *
*               =136 : Illegal logical unit number                     *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (NWDS=50)
      CHARACTER       CHOPT*(*), TOPNM*(*)
*
*     ------------------------------------------------------------------
*
* *** Decode the character option and analyse the top directory name
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)      GO TO 999
      CALL CDSBLC (TOPNM, TOP1CT, NCHT)
      IF (NCHT.LE.0) THEN
        IRC    = 135
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDILDU : Illegal'//
     +  ' top directory name'')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Find the address of the bank containing the list of directory
*  ** for the given topdirectory
*
      LREFCD(1) = 0
      LREFCD(2) = 0
      LTOP   = LTOPCD
   10 IF (LTOP.GT.0) THEN
        NCHR   = IQ(KOFUCD+LTOP+MUPNCH)
        CALL UHTOC (IQ(KOFUCD+LTOP+MUPNAM), 4, TOP2CT, NCHR)
        IF (TOP2CT(1:NCHR).NE.TOP1CT(1:NCHT)) THEN
          LTOP   = LQ(KOFUCD+LTOP)
          GO TO 10
        ELSE
          LREFCD(1) = LQ(KOFUCD+LTOP-KLFZCD)
          LREFCD(2) = LTOP
        ENDIF
      ENDIF
*
      IF (LREFCD(1).EQ.0) THEN
        LREFCD(1) = LBADCD
   20   IF (LREFCD(1).GT.0) THEN
          CALL UHTOC (IQ(KOFUCD+LREFCD(1)+MFZTOP), 4, TOP2CT, 16)
          IF (TOP1CT.NE.TOP2CT) THEN
            LREFCD(1) = LQ(KOFUCD+LREFCD(1))
            GO TO 20
          ENDIF
        ELSE
          JBIAS  = 1
          ND     = NWDS * (MXLWCD + 1) + MFZDIR - 1
          CALL CDBANK (IDIVCD, LBADCD, LBADCD, JBIAS, 'FZDB', 0, 0, ND,
     +                 IOFZCD, -1, IRC)
          IF (IRC.NE.0)  GO TO 999
          IQ(KOFUCD+LBADCD-5) = 0
          LREFCD(1) = LBADCD
          IF (LREFCD(2).GT.0) LQ(KOFUCD+LREFCD(2)-KLFZCD) = LBADCD
          CALL UCTOH (TOP1CT, IQ(KOFUCD+LBADCD+MFZTOP), 4, 16)
        ENDIF
      ENDIF
*
* *** Reset the counter if not needed
*
      IF (IOPICA.NE.0.OR.IOPACA.EQ.0) THEN
        IQ(KOFUCD+LREFCD(1)-5) = 0
        IF (IOPICA.NE.0) GO TO 999
      ENDIF
*
* *** Check input file number
*
      IF (LUNI.LE.0) THEN
        IRC    = 136
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDILDU : Illegal'//
     +  ' logical unit number'',I12)', LUNI, 1)
#endif
        GO TO 999
      ENDIF
*
* *** Now read in the file
*
      NWMX   = (IQ(KOFUCD+LREFCD(1)-1) - MFZDIR + 1) / (MXLWCD + 1)
      NCNT   = 0
   25 READ (LUNI, 1000, ERR=30, END=30) PAT2CT
      CALL CDSBLC (PAT2CT, PAT3CT, NCHR)
      IF (NCHR.LE.0)     GO TO 25
      CALL CLTOU (PAT3CT)
      NCUR   = IQ(KOFUCD+LREFCD(1)-5) + 1
      NCNT   = NCNT + 1
      IF (NCUR.GT.NWMX) THEN
        CALL ZSHUNT (IDIVCD, LREFCD(1), LREFCD(3), 2, 0)
        ND     = IQ(KOFUCD+LREFCD(3)-1) + NWDS * (MXLWCD + 1)
        JBIAS  = 1
        CALL CDBANK (IDIVCD, LBADCD, LBADCD, JBIAS, 'FZDB', 0, 0, ND,
     +               IOFZCD, -1, IRC)
        IF (IRC.NE.0) THEN
          CALL MZDROP (IDIVCD, LREFCD(3), ' ')
          IF (LREFCD(2).GT.0) LQ(KOFUCD+LREFCD(2)-KLFZCD) = 0
          GO TO 999
        ENDIF
        CALL UCOPY (IQ(KOFUCD+LREFCD(3)+1), IQ(KOFUCD+LBADCD+1),
     +              IQ(KOFUCD+LREFCD(3)-1))
        LREFCD(1) = LBADCD
        IF (LREFCD(2).GT.0) LQ(KOFUCD+LREFCD(2)-KLFZCD) = LBADCD
        CALL MZDROP (IDIVCD, LREFCD(3), ' ')
        NWMX   = NWMX + NWDS
      ENDIF
      IQ(KOFUCD+LREFCD(1)-5) = NCUR
      IPNT   = KOFUCD + LREFCD(1) + MFZDIR + (NCUR - 1) * (MXLWCD + 1)
      IQ(IPNT) = NCHR
      CALL UCTOH (PAT3CT, IQ(IPNT+1), 4, 80)
      GO TO 25
*
   30 IQUEST(11)= NCNT
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.3) THEN
        NCUR   = IQ(KOFUCD+LREFCD(1)-5)
        CALL UHTOC (IQ(KOFUCD+LREFCD(1)+MFZTOP), 4, TOP2CT, 16)
        CALL CDPRNT (LPRTCD, '(/,'' CDILDU : '',I8,'' directory names'//
     +       ' stored for top directory '//TOP2CT//''')', NCUR, 1)
        DO 40 I = 1, NCUR
          IPNT   = KOFUCD + LREFCD(1) + MFZDIR + (I - 1) * (MXLWCD + 1)
          CALL UHTOC (IQ(IPNT+1), 4, PAT2CT, 80)
          CALL CDPRNT (LPRTCD, '(10X,''Directory '',I5,'' '//PAT2CT//
     +         ''')', I, 1)
   40   CONTINUE
      ENDIF
#endif
*
 1000 FORMAT (A80)
*                                                             END CDILDU
  999 END
#endif
