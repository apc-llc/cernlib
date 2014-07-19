*
* $Id: cdfree.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cdfree.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFREE (PATHN, LBK, MASK, KEYS, CHOPT, IRC)
*     ======================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDFREE (PATHN, LBK, MASK, KEYS, CHOPT, IRC*)            *
*                                                                      *
*   Declares the given data bank(s) as candidates to be dropped in     *
*   case space is needed in the database division. Optionally it       *
*   deletes the Data bank(s) (with option D) or the Keys as well as    *
*   the Data bank(s) (with option K).                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LBK      Address(es) of Keys bank(s) KYCD                        *
*     MASK     Integer vector indicating which elements of KEYS are    *
*              significant for selection. If MASK corresponding to     *
*              one of the fields of 'Beginning' validity range is set, *
*              it will select objects with start validity smaller than *
*              those requested in KEYS. If MASK corresponding to one   *
*              of the fields of 'End' validity range is set, it will   *
*              select objects with end validity larger than those in   *
*              KEYS. If MASK corresponding to time of insertion is set,*
*              objects inserted earlier than KEYS(IDHINS) are selected *
*     KEYS     Vector of keys                                          *
*     CHOPT    Character string with any of the following characters   *
*          A   trust LBK address(es) if non-zero                       *
*          D   drop the Data bank(s) supported at link 1 of Key bank(s)*
*          K   drop the Key bank(s) as well as Data bank(s)            *
*          M   expect multiple Key banks                               *
*          S   Key/data was retrieved using 'S' option in DBUSE        *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 51 : Illegal character option                        *
*               = 52 : No access to the Key banks                      *
*               = 54 : Pathname not matched to that found in bank NOCD *
*               = 57 : Illegal pathname                                *
*               = 58 : Database structure in memory clobbered          *
*               = 59 : Some of the expected key banks not found        *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/cmulop.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (NLEVM=20)
      CHARACTER       PATHN*(*), CHOPT*(*), PATHY*80, CNODE(NLEVM)*16
      DIMENSION       LBK(9), KEYS(9), MASK(9), NCHL(NLEVM)
*
*     ------------------------------------------------------------------
*
* *** Decode all the options
*
      LREFCD(1) = LBK(1)
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)                GO TO 999
      CALL UCOPY (MASK, IOKYCA, MXDMCK)
      IF (IOPMCA.NE.0 .AND. IOPSCA.NE.0)  THEN
        IRC    = 51
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFREE : Illegal'//
     +  ' character option -- Options S/M are mutually exclusive'')',
     +  IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Check if Key bank address is usable
*
      NCHAR  = LENOCC (PATHN)
      IF (PATHN(1:1).EQ.'*'.OR.NCHAR.EQ.0) THEN
        IPUSE = 0
      ELSE
        IPUSE = 1
      ENDIF
      IF (LREFCD(1).NE.0.AND.IOPACA.NE.0) THEN
        IKUSE = 1
      ELSE
        IKUSE = 0
      ENDIF
*
* *** Check if the options are consistent
*
      IF (IPUSE+IKUSE.EQ.0)  THEN
        IRC    = 52
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFREE : Illegal'//
     +  ' entry -- No access to Key bank '')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Load top directory information
*
      IF (IKUSE.NE.0)  THEN
        LBNOCD = LQ(KOFUCD+LREFCD(1)-KLNOCD)
        NWKYCK = IQ(KOFUCD+LBNOCD+MNDNWK)
        NCHAR  = IQ(KOFUCD+LBNOCD+MNDNCH)
        IF (NCHAR.GT.MAXLCD) NCHAR  = MAXLCD
        CALL UHTOC (IQ(KOFUCD+LBNOCD+MNDNAM), 4, PAT1CT, NCHAR)
        PATHY  = PAT1CT(1:NCHAR)
        CALL CDLDUP (PATHY, 0, IRC)
      ELSE
        CALL CDLDUP (PATHN, 0, IRC)
        PATHY  = PAT1CT
        NCHAR  = LENOCC (PATHY)
      ENDIF
*
      IF (IDEBCD.GT.0.AND.(IPUSE.NE.0.AND.IKUSE.NE.0))  THEN
        CALL CDSBLC (PATHN, PAT2CT, NCHR)
        NF1    = NCHAR - NCHR + 1
        IF (NF1.LE.0.OR.PATHY(NF1:NCHAR).NE.PAT2CT)  THEN
          IRC    = 54
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFREE : Pathn'//
     +    'ame '//PAT2CT//' not matched to '',/,18X,'' '//PATHY//' fo'//
     +    'und in bank NOCD'')', IARGCD, 0)
#endif
          GO TO 999
        ENDIF
      ENDIF
*
* *** Get number of key banks needed
*
      CALL CDKMUL (KEYS, NKYMX, IRC)
      IF (IRC.NE.0)                GO TO 999
*
* *** Get the L-address of the first Key bank
*
      IF (IKUSE.NE.0)  THEN
        LBNOCD = LQ(KOFUCD+LREFCD(1)-KLNOCD)
        LBKYCD = LQ(KOFUCD+LBNOCD-KLKYCD)
        ND     = IQ(KOFUCD+LBNOCD+MNDNWD)
      ELSE
*
*  **   Get it from the pathname
*  **   First find the list of Nodes
*
        CALL CDPARS (PATHY, NLEVM, CNODE, NCHL, NODES)
*
*  **   Number of Nodes found should be nonzero
*
        IF (NODES.EQ.0)  THEN
          IRC    = 57
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFREE : Illeg'//
     +    'al pathname '//PATHY//''')', IARGCD, 0)
#endif
          GO TO 999
        ENDIF
*
*  **   Go through the Nodes to reach the bottom level
*
        IN     = 0
   10   IN     = IN + 1
*
*  *    Construct the pathname from the Node names
*
        NCHR   = 1
        DO 20 I = 1, IN
          NMAX   = NCHL (I)
          IF (NCHR+NMAX .GT. MAXLCD) THEN
            NMAX   = MAXLCD - NCHR - 1
            IF (NMAX.LE.0)         GO TO 20
          ENDIF
          NCHR   = NCHR + NMAX + 1
          IF (NCHR.EQ.MAXLCD)      GO TO 30
   20   CONTINUE
   30   PAT4CT = PATHY(1:NCHR)
*
*  *   The first Node should be at the top directory
*
        I0     = 0
        IF (IN.EQ.1) THEN
          NMAX   = 1
        ELSE
          NMAX   = IQ(KOFUCD+LBNOCD-2)
        ENDIF
*
   40   I0     = I0 + 1
*
*  *    No link left for the Node bank
*
        IF (I0.GT.NMAX)  THEN
          IRC        = 58
          IQUEST(11) = IN
          IQUEST(12) = NODES
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFREE : Node'//
     +    ' '//CNODE(IN)//' at level '',I3,'' out of '',I3,'' not fou'//
     +    'nd'')', IQUEST(11), 2)
#endif
          GO TO 999
        ENDIF
*
        IF (IN.EQ.1)  THEN
          LSAVCD = LQ(KOFUCD+LBUPCD-1)
        ELSE
          LSAVCD = LQ(KOFUCD+LBNOCD-I0)
        ENDIF
*
*  *    Check the full pathname in the Node bank name against
*  *    the current pathname
*
        IF (LSAVCD.NE.0) THEN
          MCHAR  = IQ(KOFUCD+LSAVCD+MNDNCH)
          IF (MCHAR.GT.MAXLCD) MCHAR  = MAXLCD
          CALL UHTOC (IQ(KOFUCD+LSAVCD+MNDNAM), 4, PAT2CT, MCHAR)
          PAT2CT = PAT2CT(1:MCHAR)
          IF (PAT2CT.NE.PAT4CT)    GO TO 40
          LBNOCD = LSAVCD
          IF (IN.LT.NODES)         GO TO 10
        ELSE
          GO TO 40
        ENDIF
*
        LBKYCD = LQ(KOFUCD+LBNOCD-KLKYCD)
        ND     = IQ(KOFUCD+LBNOCD+MNDNWD)
      ENDIF
*
* *** Number of Key banks to be serviced with option S
*
      IF (IOPSCA.NE.0)  THEN
        NKYMX  = NZBANK (IDIVCD, LBKYCD)
*
        IF (IPRBCA.EQ.0.AND.IPRECA.EQ.0) THEN
          IFLG   = 99
        ELSE
          IFLG   = 0
        ENDIF
      ENDIF
*
* *** Loop over all possible Key Banks
*
      NK     = 0
   50 NK     = NK + 1
*
*  ** For option 'M', find the corresponding key values
*
      CALL CDKMLD (NK, KEYS)
*
*  ** Check if this Key bank already exists
*
      IF (IOPSCA.EQ.0) LBKYCD = LQ(KOFUCD+LBNOCD-KLKYCD)
   60 CONTINUE
      IF (LBKYCD.NE.0)  THEN
        IF (IOPSCA.EQ.0)  THEN
          CALL CDKSEL (ITIME,KYVMCK,IQ(KOFUCD+LBKYCD+1),99,ISEL,INBR)
          IF (ISEL.NE.0)           GO TO 70
*
        ELSE
*
          CALL CDKSEL (ITIME,KYVMCK,IQ(KOFUCD+LBKYCD+1),IFLG,ISEL,INBR)
          IF (ISEL.NE.0)           GO TO 70
        ENDIF
*
*  **   Take proper action according to character option
*
        IF (IOPKCA.NE.0)  THEN
*
*  *      Drop the Key bank
*
          LKEY   = LBKYCD
          LBKYCD = LQ(KOFUCD+LKEY+2)
          CALL MZDROP (IDIVCD, LKEY, ' ')
        ELSE IF (IOPDCA.NE.0) THEN
*
*  *      Drop the data bank
*
          CALL MZDROP (IDIVCD, LBKYCD, 'V')
        ELSE
*
*  *      Set the free bit
*
          IQ(KOFUCD+LBKYCD+ND+MKYFRI) = 1
        ENDIF
        GO TO 80
   70   LBKYCD = LQ(KOFUCD+LBKYCD)
        GO TO 60
*
      ELSE
*
        IF (IOPSCA.EQ.0) THEN
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0.AND.IRC.EQ.0) CALL CDPRNT (LPRTCD,'(/,'' '//
     +      'CDFREE : At least one of the required key bank(s) not '//
     +      'found'')', IARGCD, 0)
#endif
          IRC    = 59
          IF (IOPMCA.NE.0) LBKYCD = LQ(KOFUCD+LBNOCD-KLKYCD)
        ELSE
          GO TO 999
        ENDIF
      ENDIF
*
   80 CONTINUE
      IF (IOPSCA.EQ.0)  THEN
        IF (NK.LT.NKYMX)           GO TO 50
      ELSE
        IF (LBKYCD.NE.0) LBKYCD = LQ(KOFUCD+LBKYCD)
        IF (LBKYCD.NE.0)           GO TO 50
      ENDIF
*                                                             END CDFREE
  999 END
