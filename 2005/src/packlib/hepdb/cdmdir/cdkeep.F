*
* $Id: cdkeep.F,v 1.1.1.1 1996/02/28 16:24:16 mclareni Exp $
*
* $Log: cdkeep.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:16  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKEEP (PATHS, NPATH, CHOPT, IRC)
*     ============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDKEEP (PATHS, NPATH, CHOPT, IRC*)                      *
*                                                                      *
*   Deletes all directory trees from the data base except the ones     *
*   specified by the user                                              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHS    Path names to be kept                                   *
*     NPATH    Number of paths to be kept                              *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =211 : Illegal number of paths                         *
*               =212 : Illegal path name                               *
*               =213 : Conflicting top directory names                 *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (NLEVM=20)
      DIMENSION       NCHL(NLEVM)
      CHARACTER       PATHY*255
      CHARACTER       CNODE(NLEVM)*16
      CHARACTER*(*)   PATHS(*), CHOPT
*
*     ------------------------------------------------------------------
*
* *** Find the top directory name
*
      IF (NPATH.LE.0) THEN
        IRC    = 211
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKEEP : Illegal'//
     +  ' number of paths to be kept '',I12)', NPATH, 1)
#endif
        GO TO 999
      ENDIF
      TOP1CT = ' '
      DO 10 IP = 1, NPATH
        CALL CDLDDI (PATHS(IP), PATHY, NCHR)
        IF (NCHR.LT.3.OR.PATHY(1:2).NE.'//') THEN
          IRC    = 212
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKEEP : Illeg'//
     +    'al path name '//PATHY//''')', IARGCD, 0)
#endif
          GO TO 999
        ENDIF
        NCHRT  = INDEX (PATHY(3:NCHR), '/') - 1
        IF (NCHRT.LE.0) NCHRT = NCHR - 2
        TOP2CT = PATHY(3:NCHRT+2)
        IF (TOP1CT.EQ.' ') THEN
          TOP1CT = TOP2CT
        ELSE IF (TOP2CT.NE.TOP1CT) THEN
          IRC    = 213
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKEEP : Confl'//
     +    'icting top-directories '//TOP1CT//' and '//TOP2CT//''')',
     +    IARGCD, 0)
#endif
          GO TO 999
        ENDIF
   10 CONTINUE
*
* *** Now prepare the bank with all node names
*
      LEVM   = NLEVM
      PATHY  = '//'//TOP1CT
      LSAVCD = 0
      CALL CDFPAT (PATHY, LEVM, LSAVCD, IRC)
      IF (IRC.NE.0)                                          GO TO 100
      IF (LSAVCD.EQ.0) THEN
        IRC    = 212
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKEEP : Illegal'//
     +  ' path name '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      NLEV   = IQ(KOFUCD+LSAVCD+MNLVCD)
      IF (NLEV.LE.0)                                         GO TO 100
*
* *** Loop over all the paths to be kept
*
      DO 50 IP = 1, NPATH
        CALL CDLDDI (PATHS(IP), PATHY, NCHR)
*
*  **   Find all the nodes in the path name
*
        CALL CDPARS (PATHY, NLEVM, CNODE, NCHL, NODES)
*
*  **   Now look up the complete list and mark the nodes to be kept
*
        IPNL   = IQ(KOFUCD+LSAVCD+MPNLCD)
        NLEV0  = MIN0 (NLEV, NODES)
        IUPN   = 0
        DO 40 ILEV = 1, NLEV0
          NNODE  = IQ(KOFUCD+LSAVCD+IPNL+MNODCD)
          IPNN   = IQ(KOFUCD+LSAVCD+IPNL+MPNNCD)
          DO 20 INOD = 1, NNODE
            IF ((IQ(KOFUCD+LSAVCD+IPNN+MNCHCD).EQ.NCHL(ILEV)) .AND.
     +          (IQ(KOFUCD+LSAVCD+IPNN+MNFNCD).EQ.IUPN)) THEN
              CALL UHTOC (IQ(KOFUCD+LSAVCD+IPNN+MNAMCD), 4, TOP2CT,
     +                    NCHL(ILEV))
              IF (TOP2CT(1:NCHL(ILEV)).EQ.CNODE(ILEV)) THEN
                IUPN   = INOD
                Q(KOFUCD+LSAVCD+IPNN+MYFNCD) = 1.0
                GO TO 30
              ENDIF
            ENDIF
            IPNN   = IQ(KOFUCD+LSAVCD+IPNN+MPNLCD)
   20     CONTINUE
          GO TO 50
   30     IPNL   = IQ(KOFUCD+LSAVCD+IPNL+MPNLCD)
   40   CONTINUE
*
   50 CONTINUE
*
* *** Now delete the unmarked directories
*
      IPNL   = IQ(KOFUCD+LSAVCD+MPNLCD)
      DO 90 ILEV =1, NLEV
        NNODE  = IQ(KOFUCD+LSAVCD+IPNL+MNODCD)
        IPNN   = IQ(KOFUCD+LSAVCD+IPNL+MPNNCD)
        DO 85 INOD = 1, NNODE
          IF (Q(KOFUCD+LSAVCD+IPNN+MYFNCD).EQ.0.) THEN
            IF (ILEV.EQ.2) THEN
              NCHR   = IQ(KOFUCD+LSAVCD+IPNN+MNCHCD)
              CALL UHTOC (IQ(KOFUCD+LSAVCD+IPNN+MNAMCD), 4, TOP2CT,NCHR)
              IF ((NCHR.EQ.10.AND.TOP2CT(1:NCHR).EQ.'DICTIONARY').OR.
     +            (NCHR.EQ.4 .AND.TOP2CT(1:NCHR).EQ.'HELP')) GO TO 80
            ENDIF
*
*  **       Delete the tree from here; construct the path name first
*
            IUPN   = INOD
            NCHRT  = 0
            PAT4CT = ' '
            DO 65 KLEV = 1, ILEV
              JLEV   = ILEV + 1 - KLEV
              IPNLC  = IQ(KOFUCD+LSAVCD+MPNLCD)
              IF (JLEV.GT.1) THEN
                DO 55 IL = 2, JLEV
   55           IPNLC  = IQ(KOFUCD+LSAVCD+IPNLC+MPNLCD)
              ENDIF
              IPNNC  = IQ(KOFUCD+LSAVCD+IPNLC+MPNNCD)
              IF (IUPN.GT.1) THEN
                DO 60 IN = 2, IUPN
   60           IPNNC   = IQ(KOFUCD+LSAVCD+IPNNC+MPNLCD)
              ENDIF
              NCHR   = IQ(KOFUCD+LSAVCD+IPNNC+MNCHCD)
              IUPN   = IQ(KOFUCD+LSAVCD+IPNNC+MNFNCD)
              CALL UHTOC (IQ(KOFUCD+LSAVCD+IPNNC+MNAMCD), 4,TOP2CT,NCHR)
              PAT1CT = '/'//TOP2CT(1:NCHR)
              NCHR   = NCHR + 1
              IF (NCHRT.GT.0) THEN
                PATHY  = PAT1CT(1:NCHR)//PAT4CT(1:NCHRT)
              ELSE
                PATHY  = PAT1CT
              ENDIF
              PAT4CT = PATHY
              NCHRT  = NCHRT + NCHR
   65       CONTINUE
            PATHY  = '/'//PAT4CT
*
*  **       Now delete the tree
*
            CALL CDDDIR (PATHY, CHOPT, IRC)
            IF (IRC.NE.0)                                    GO TO 100
*
*  **       Now mark all down nodes from here to be nonexistent
*
            NND    = IQ(KOFUCD+LSAVCD+IPNN+MNDWCD)
            IF (ILEV.LT.NLEV.AND.NND.GT.0) THEN
              IPNLC  = IQ(KOFUCD+LSAVCD+IPNL+MPNLCD)
              KLEV   = ILEV + 1
              NCHL(KLEV) = INOD
   70         IF (KLEV.GT.ILEV) THEN
                NODES  = IQ(KOFUCD+LSAVCD+IPNLC+MNODCD)
                IPNNC  = IQ(KOFUCD+LSAVCD+IPNLC+MPNNCD)
                DO 75 IN = 1, NODES
                  IF ((Q(KOFUCD+LSAVCD+IPNNC+MYFNCD).EQ.0.0) .AND.
     +               (NCHL(KLEV).EQ.IQ(KOFUCD+LSAVCD+IPNNC+MNFNCD)))THEN
                    Q(KOFUCD+LSAVCD+IPNNC+MYFNCD) = 1.0
                    NND    = IQ(KOFUCD+LSAVCD+IPNNC+MNDWCD)
                    IF (KLEV.LT.NLEV.AND.NND.GT.0) THEN
                      KLEV   = KLEV + 1
                      IPNLC  = IQ(KOFUCD+LSAVCD+IPNLC+MPNLCD)
                      NCHL(KLEV) = IN
                      GO TO 70
                    ENDIF
                  ENDIF
                  IPNNC  = IQ(KOFUCD+LSAVCD+IPNNC+MPNLCD)
   75           CONTINUE
                KLEV   = KLEV - 1
                IPNLC  = IQ(KOFUCD+LSAVCD+IPNLC+MPPLCD)
                GO TO 70
              ENDIF
            ENDIF
*
          ENDIF
   80     IPNN   = IQ(KOFUCD+LSAVCD+IPNN+MPNLCD)
   85   CONTINUE
        IPNL   = IQ(KOFUCD+LSAVCD+IPNL+MPNLCD)
   90 CONTINUE
      IRC    = 0
*
  100 IF (LSAVCD.NE.0) THEN
        CALL MZDROP (IDIVCD, LSAVCD, 'L')
        LSAVCD = 0
      ENDIF
*                                                             END CDKEEP
  999 END
