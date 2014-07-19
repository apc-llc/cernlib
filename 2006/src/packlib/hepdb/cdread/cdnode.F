*
* $Id: cdnode.F,v 1.1.1.1 1996/02/28 16:24:26 mclareni Exp $
*
* $Log: cdnode.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:26  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDNODE (PATHN, IRC)
*     ==============================
*
************************************************************************
*                                                                      *
*        SUBR. CDNODE (PATHN, IRC*)                                    *
*                                                                      *
*   Creates a Node structure in memory for given directory pathname    *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDBOOK, CDGETDB,CDPURK, CDUSE,  CDUSEDB,CDUSEM           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 12 : Illegal pathname                                *
*               = 13 : Not enough structural link to support a new Node*
*               = 15 : Cannot define IO descriptor for Key bank        *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (NLEVM=20, NWFXM=5, MAXD=1)
      CHARACTER       CHFRM(5)*1, CFORM*80, CHCUR(NLEVM)*1
      CHARACTER       PATHN*(*), PATHY*255, CNODE(NLEVM)*16
      INTEGER         NUCUR(NLEVM), NCHL(NLEVM)
      DATA            CHFRM / 'B', 'I', 'F', 'D', 'H'/,  NZ / 0/
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Sets current directory and Get the full pathname
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)              GO TO 999
      PATHY  = PAT1CT
      MAXL   = LENOCC (PATHY)
*
* *** Get list of Nodes
*
      CALL CDPARS (PATHY, NLEVM, CNODE, NCHL, NODES)
*
* *** Number of Nodes found should be nonzero
*
      IF (NODES.EQ.0)  THEN
        IRC    = 12
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDNODE : Illegal'//
     +  ' pathname '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Find the dictionary bank
*
      IF (LBUPCD.GT.0) THEN
        LBKYCD  = LQ(KOFUCD+LBUPCD-KLDICD)
      ELSE
        LBKYCD  = 0
      ENDIF
      IF (LBKYCD.NE.0) THEN
        NITEM  = IQ(KOFUCD+LBKYCD+MDCNTM)
      ELSE
        NITEM  = 0
      ENDIF
*
*  ** Check if all Nodes already exist in memory
*
      IN     = 0
   10 IN     = IN + 1
*
*  ** Construct the pathname from the Node names
*
      NCHAR  = 1
      DO 15 I = 1, IN
        NMAX   = NCHL (I)
        IF (NCHAR+NMAX .GT. MAXLCD) THEN
          NMAX  = MAXLCD - NCHAR - 1
          IF (NMAX.LE.0)         GO TO 15
        ENDIF
        NCHAR  = NCHAR + NMAX + 1
        IF (NCHAR.EQ.MAXLCD)     GO TO 20
   15 CONTINUE
   20 PAT4CT = PATHY(1:NCHAR)
*
*  ** The first Node should be at the top directory
*
      I0     = 0
      IF (IN.EQ.1) THEN
        NMAX   = 1
      ELSE
        NMAX   = IQ(KOFUCD+LBNOCD-2)
      ENDIF
*
   25 I0     = I0 + 1
*
*  *  No link left to insert a new Node bank
*
      IF (I0.GT.NMAX)  THEN
        IF (IN.EQ.1.OR.IOUTCD.EQ.0)  THEN
          IRC        = 13
          IQUEST(11) = IN
          IQUEST(12) = NODES
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDNODE : No li'//
     +    'nk left to insert the Node '//CNODE(IN)//' at level '',I3'//
     +    ','' out of '',I3)', IQUEST(11), 2)
#endif
          GO TO 999
*
        ELSE
*
          CALL MZPUSH (IDIVCD, LBNOCD, 1, 0, ' ')
          LSAVCD = LBNOCD
          JBIAS  = -I0
          IDIC   = 0
          GO TO 40
        ENDIF
      ENDIF
*
      IF (IN.EQ.1)  THEN
        LSAVCD = LQ(KOFUCD+LBUPCD-1)
      ELSE
        LSAVCD = LQ(KOFUCD+LBNOCD-I0)
      ENDIF
*
*  *  Check the full pathname in the Node bank name against
*  *  the current pathname
*
      IF (LSAVCD.NE.0)  THEN
        MCHAR  = IQ(KOFUCD+LSAVCD+MNDNCH)
        IF (MCHAR.GT.MAXLCD) MCHAR  = MAXLCD
        CALL UHTOC (IQ(KOFUCD+LSAVCD+MNDNAM), 4, PAT2CT, MCHAR)
        PAT2CT = PAT2CT(1:MCHAR)
        IF (PAT2CT.NE.PAT4CT)    GO TO 25
        LBNOCD = LSAVCD
        IF (IN.EQ.NODES) THEN
          CALL RZCDIR (PAT4CT, ' ')
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          NKEYCK = IQUEST(7)
          NWKYCK = IQUEST(8)
        ENDIF
        GO TO 60
      ELSE
*
*  *    Find the dictionary number
*
        IF (IN.EQ.1)  THEN
          IDIC   = ITOPCD * 10000
          LSAVCD = LBUPCD
          JBIAS  = -1
        ELSE
          LSAVCD = LBNOCD
          JBIAS  = -I0
          IDIC   = ITOPCD * 10000
          IF (NITEM.GT.0) THEN
            PAT2CT = PAT4CT(NCHL(1)+3:NCHAR)
            MCHAR  = NCHAR - NCHL(1) - 2
            DO 30 ITEM = 1, NITEM
              IPNT   = KOFUCD + LBKYCD + (ITEM - 1) * NWITCD + 1
              IF (IQ(IPNT+MDCITM).GT.0) THEN
                IF (MCHAR.EQ.IQ(IPNT+MDCNCH)) THEN
                  CALL UHTOC (IQ(IPNT+MDCNAM), 4, PAT3CT, MCHAR)
                  PAT3CT = PAT3CT(1:MCHAR)
                  IF (PAT2CT.EQ.PAT3CT) THEN
                    IDIC   = IDIC + IQ(IPNT+MDCITM)
                    GO TO 35
                  ENDIF
                ENDIF
              ENDIF
   30       CONTINUE
          ENDIF
   35     CONTINUE
        ENDIF
      ENDIF
*
*  *  The Node bank does not exist; create it
*
   40 ND     = (NCHAR + 3) / 4
      ND     = ND + NWNOCD + 4
      CALL RZCDIR (PAT4CT, ' ')
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      CALL RZRDIR (MAXD, PAT3CT, NL)
      IF (IQUEST(1).EQ.1) NL = IQUEST(11)
      IF (NKEYCK.GT.0) THEN
        IOPTP  = IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG)
        IOPTP  = JBIT (IOPTP, JPRTCD)
        IF (IOPTP.NE.0) THEN
          NL     = NL - NKEYCK
          IF (NL.LT.0) NL = 0
        ENDIF
      ENDIF
*
      CALL CDBANK (IDIVCD, LBNOCD, LSAVCD, JBIAS, 'NOCD', NL, NL, ND,
     +             IONOCD, NZ, IRC)
      IF (IRC.NE.0)              GO TO 999
*
*  *  Find IO descriptor for the Key banks attached to this Node
*
      NLEV   = 0
      NCUR   = 0
      IFORO  = 0
      CALL CDKEYT
      DO 50 I = 1, NWKYCK
        IFORM  = IOTYCK(I)
        IF (IFORM.EQ.6) IFORM = 5
        IF (IFORM.EQ.IFORO)  THEN
          NCUR   = NCUR + 1
        ELSE
          IF (NLEV.GT.0) NUCUR(NLEV) = NCUR
          IF (NLEV.GE.NLEVM)  THEN
            IRC        = 15
            IQUEST(11) = NLEVM
            IQUEST(12) = I
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) THEN
              CALL CDPRNT (LPRTCD, '(/,'' CDNODE : Too many type of'//
     +             ' variables for IO descriptor of Key bank at Key'//
     +             ' '',I3,'' Maximum level '',I3)', IQUEST(11), 2)
            ENDIF
#endif
            GO TO 999
          ENDIF
          NLEV   = NLEV + 1
          CHCUR(NLEV) = CHFRM(IFORM)
          NCUR   = 1
          IFORO  = IFORM
        ENDIF
   50 CONTINUE
*
      IF (NLEV.GT.0)  THEN
        NUCUR(NLEV) = NCUR
        IF (CHCUR(NLEV).EQ.CHFRM(2))  THEN
          NUCUR(NLEV) = NCUR + NWFXM + NPARCD
        ELSE
          IF (NLEV.GE.NLEVM)  THEN
            IRC        = 15
            IQUEST(11) = NLEVM
            IQUEST(12) = NWKYCK + 1
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) THEN
              CALL CDPRNT (LPRTCD, '(/,'' CDNODE : Too many type of'//
     +             ' variables for IO descriptor of Key bank at Key'//
     +             ' '',I3,'' Maximum level '',I3)', IQUEST(11), 2)
            ENDIF
#endif
            GO TO 999
          ENDIF
          NLEV   = NLEV + 1
          CHCUR(NLEV) = CHFRM(2)
          NUCUR(NLEV) = NWFXM + NPARCD
        ENDIF
      ELSE
        NLEV   = 1
        CHCUR(NLEV) = CHFRM(2)
        NUCUR(NLEV) = NWFXM + NPARCD
      ENDIF
*
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
      WRITE (CFORM, 2001) (NUCUR(I), CHCUR(I), I = 1, NLEV)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
      CFORM  = ' '
      II     = 1
      DO 55 I = 1, NLEV
        CALL UTWRIT (CFORM(II:II+1), '(I2)', NUCUR(I))
        II     = II + 2
        CFORM(II:II) = CHCUR(I)
        II     = II + 2
   55 CONTINUE
#endif
      I      = 4*NLEV
*
      IQ(KOFUCD+LBNOCD+MNDNWK) = NWKYCK
      IQ(KOFUCD+LBNOCD+MNDNWD) = NWKYCK + NWFXM + NPARCD
      IQ(KOFUCD+LBNOCD+MNDNCH) = NCHAR
      IQ(KOFUCD+LBNOCD+MNDDIC) = IDIC
      CALL MZIOCH (IQ(KOFUCD+LBNOCD+MNDIOF), NWNOCD, CFORM(1:I))
      CALL UCTOH (PAT4CT, IQ(KOFUCD+LBNOCD+MNDNAM), 4, NCHAR)
*
   60 LSAVCD = 0
      IF (IN.LT.NODES)           GO TO 10
*
* *** The Node exists; Normal return
*
  100 IRC    = 0
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2001 FORMAT (20(I2,A1,1X))
#endif
*                                                             END CDNODE
  999 END
