*
* $Id: cdildf.F,v 1.1.1.1 1996/02/28 16:24:11 mclareni Exp $
*
* $Log: cdildf.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:11  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if !defined(CERNLIB__P3CHILD)
      SUBROUTINE CDILDF (LUNI, CHOPT, IRC)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDILDF (LUNI, CHOPT, IRC*)                              *
*                                                                      *
*   Initializes the List of Directories to be always updated from the  *
*   journal file (ignore the check of the key 1).                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUNI     Logical unit number from which the list is read off     *
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
*               =136 : Illegal logical unit number                     *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (NWDS=50)
      CHARACTER       CHOPT*(*)
*
*     ------------------------------------------------------------------
*
* *** Decode the character option
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)      GO TO 999
*
* *** Find the address of the bank containing the list of directory
*
      LREFCD(1) = LBAFCD
      IF (LREFCD(1).EQ.0) THEN
        JBIAS  = 1
        ND     = NWDS * (MXLWCD + 1)
        CALL CDBANK (IDIVCD, LBAFCD, LBAFCD, JBIAS, 'FDDB', 0, 0, ND,
     +               IOFDCD, -1, IRC)
        IF (IRC.NE.0)    GO TO 999
        IQ(KOFUCD+LBAFCD-5) = 0
        LREFCD(1) = LBAFCD
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
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDILDF : Illegal'//
     +  ' logical unit number'',I12)', LUNI, 1)
#endif
        GO TO 999
      ENDIF
*
* *** Now read in the file
*
      NWMX   = IQ(KOFUCD+LREFCD(1)-1) / (MXLWCD + 1)
      NCNT   = 0
   10 READ (LUNI, 1000, ERR=20, END=20) PAT2CT
      CALL CDSBLC (PAT2CT, PAT3CT, NCHR)
      IF (NCHR.LE.0)     GO TO 10
      CALL CLTOU (PAT3CT)
      NCUR   = IQ(KOFUCD+LREFCD(1)-5) + 1
      NCNT   = NCNT + 1
      IF (NCUR.GT.NWMX) THEN
        CALL ZSHUNT (IDIVCD, LREFCD(1), LREFCD(3), 2, 0)
        ND     = IQ(KOFUCD+LREFCD(3)-1) + NWDS * (MXLWCD + 1)
        JBIAS  = 1
        CALL CDBANK (IDIVCD, LBAFCD, LBAFCD, JBIAS, 'FDDB', 0, 0, ND,
     +               IOFDCD, -1, IRC)
        IF (IRC.NE.0) THEN
          CALL MZDROP (IDIVCD, LREFCD(3), ' ')
          GO TO 999
        ENDIF
        CALL UCOPY (IQ(KOFUCD+LREFCD(3)+1), IQ(KOFUCD+LBAFCD+1),
     +              IQ(KOFUCD+LREFCD(3)-1))
        LREFCD(1) = LBAFCD
        CALL MZDROP (IDIVCD, LREFCD(3), ' ')
        IQUEST(1) = 0
        NWMX   = NWMX + NWDS
      ENDIF
      IQ(KOFUCD+LREFCD(1)-5) = NCUR
      IPNT   = KOFUCD + LREFCD(1) + (NCUR - 1) * (MXLWCD + 1) + 1
      IQ(IPNT) = NCHR
      CALL UCTOH (PAT3CT, IQ(IPNT+1), 4, 80)
      GO TO 10
*
   20 IQUEST(11)= NCNT
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.3) THEN
        NCUR   = IQ(KOFUCD+LREFCD(1)-5)
        CALL CDPRNT (LPRTCD, '(/,'' CDILDF : '',I8,'' directory names'//
     +       ' stored for forced updating'')', NCUR, 1)
        DO 30 I = 1, NCUR
          IPNT   = KOFUCD + LREFCD(1) + (I - 1) * (MXLWCD + 1) + 1
          CALL UHTOC (IQ(IPNT+1), 4, PAT2CT, 80)
          CALL CDPRNT (LPRTCD, '(10X,''Directory '',I5,'' '//PAT2CT//
     +         ''')', I, 1)
   30   CONTINUE
      ENDIF
#endif
*
 1000 FORMAT (A80)
*                                                             END CDILDF
  999 END
#endif
