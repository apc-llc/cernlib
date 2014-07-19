*
* $Id: cdsumy.F,v 1.1.1.1 1996/02/28 16:24:45 mclareni Exp $
*
* $Log: cdsumy.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:45  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSUMY (PATH, NLEVL, IRC)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSUMY (PATH, NLEVL, IRC*)                              *
*                                                                      *
*   Prints statistics of the directory tree given by PATH              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATH     Pathname of the directory                               *
*     NLEVL    Number of levels below PATH about which information     *
*              has to be printed                                       *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =131 : Illegal pathname                                *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (NLEVM=20, KRUSE=13, KWUSE=14, KMEGA=15)
      DIMENSION       NCHL(NLEVM), ISDI(NLEVM), NSDI(NLEVM)
      DIMENSION       NKEY(NLEVM), IOPT(NLEVM), NKUSE(NLEVM)
      DIMENSION       NRUSE(NLEVM), NWUSE(NLEVM), IHDIR(4)
      CHARACTER       PATHN*80, PATHX*16, PATHY*80, PATHZ*16
      CHARACTER       CNODE(NLEVM)*16, PATH*(*)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Set the current directory to pathname
*
      NLEV   = NLEVL
      IF (NLEV.LE.0) NLEV = 99
      CALL CDLDUP (PATH, 1, IRC)
      IF (IRC.NE.0)               GO TO 999
      PATHN   = PAT1CT
*
* *** Get list of Nodes
*
      CALL CDPARS (PATHN, NLEVM, CNODE, NCHL, NODES)
      ITIME  = 0
      DO 10 I = 1, NLEVM
        NRUSE(I) = 0
        NWUSE(I) = 0
        NKUSE(I) = 0
   10 CONTINUE
      NLPAT0 = NODES
      CALL CDPRNT (LPRTDB, '(//,''     NREC     NWORDS    NOBJECT   D'//
     +    'IR. NAME'')', IARGCD, 0)
*
* *** Now scan down to find all the subdirectories
*
   20 IF (ITIME.NE.0) THEN
        CALL RZPAFF (CNODE, NODES, PATHN)
        IF (IQUEST(1).NE.0) THEN
          NODES  = NODES - 1
          GO TO 30
        ENDIF
        CALL RZCDIR (PATHN, 'Q')
        IF (IQUEST(1).NE.0) THEN
          NODES  = NODES - 1
          GO TO 30
        ENDIF
        NKEYCK = IQUEST(7)
        NWKYCK = IQUEST(8)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
      ENDIF
      IF (NKEYCK.GT.0) THEN
        IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      ELSE
        IOPTP  = 0
      ENDIF
      NKEY(NODES) = NKEYCK
      IOPT(NODES) = IOPTP
      ISDI(NODES) = 0
      NSDI(NODES) = IQ(KOFSCD+LCDRCD+KNSDCD)
*
   30 IF (NODES.LE.0)               GO TO 100
      ISDI(NODES) = ISDI(NODES) + 1
      IF (ISDI(NODES).LE.NSDI(NODES)) THEN
*
*  **   If a new subdirectory go down one level
*
        LS    = IQ(KOFSCD+LCDRCD+KLSDCD)
        IPNT  = LS + 7 * (ISDI(NODES) - 1)
        CALL ZITOH (IQ(KOFSCD+LCDRCD+IPNT), IHDIR, 4)
        CALL UHTOC (IHDIR, 4, PATHX, 16)
        NCHR  = LENOCC (PATHX)
        IF (IOPT(NODES).NE.0) THEN
          NCHRM  = LENOCC (PATHN)
          DO 45 IK = 1, NKEY(NODES)
            KK     = IK
            IDIG   = 0
   35       IF (KK.GT.0) THEN
              KK     = KK / 10
              IDIG   = IDIG + 1
              GO TO 35
            ENDIF
            IF (NCHR.EQ.IDIG) THEN
              WRITE (PATHY, '(I8)') IK
              I1     = 8 - IDIG + 1
              PATHZ  = PATHY(I1:8)
              IF (PATHZ.EQ.PATHX) THEN
                PATHY  = PATHN(1:NCHRM)//'/'//PATHX
                CALL RZCDIR (PATHY, 'Q')
                IF (IQUEST(1).NE.0) GO TO 45
                NKEYDK = IQUEST(7)
                LCDRDB = IQUEST(11)
                IKDRDB = IQUEST(13)
                DO 40 K = NLPAT0, NODES
                  NRUSE(K) = NRUSE(K) + IQ(KOFSCD+LCDRCD+KRUSE)
                  NWUSE(K) = NWUSE(K) + IQ(KOFSCD+LCDRCD+KWUSE)
     +                     + 1000000  * IQ(KOFSCD+LCDRCD+KMEGA)
   40           CONTINUE
                NKUSE(NODES) = NKUSE(NODES) + NKEYCK
                CALL RZCDIR (PATHN, 'Q')
                IF (IQUEST(1).NE.0) THEN
                  NODES   = NODES - 1
                ELSE
                  NKEYCK = IQUEST(7)
                  LCDRCD = IQUEST(11)
                  IKDRCD = IQUEST(13)
                ENDIF
                GO TO 30
              ENDIF
            ENDIF
   45     CONTINUE
        ENDIF
        ITIME  = ITIME + 1
        NODES  = NODES + 1
        CNODE(NODES) = PATHX
        GO TO 20
*
      ELSE
*
*  **   Check if the name is to be entered
*
        CALL RZPAFF (CNODE, NODES, PATHY)
        NCHR  = LENOCC (PATHY)
        DO 50 K = NLPAT0, NODES
           NRUSE(K) = NRUSE(K) + IQ(KOFSCD+LCDRCD+KRUSE)
           NWUSE(K) = NWUSE(K) + IQ(KOFSCD+LCDRCD+KWUSE)
     +              + 1000000  * IQ(KOFSCD+LCDRCD+KMEGA)
   50   CONTINUE
        IF (IOPT(NODES).EQ.0) NKUSE(NODES) = NKEY(NODES)
        IF (NODES.LE.(NLPAT0+NLEV)) THEN
          IARGCD(1) = NRUSE(NODES)
          IARGCD(2) = NWUSE(NODES)
          IARGCD(3) = NKUSE(NODES)
          CALL CDPRNT (LPRTDB, '(I9,2I11,''   '//PATHY(1:NCHR)//' '')',
     +         IARGCD, 3)
        ENDIF
        NRUSE(NODES) = 0
        NWUSE(NODES) = 0
        NKUSE(NODES) = 0
*
        NODES   = NODES - 1
        IF (NODES.GE.NLPAT0) THEN
          LUP    = LQ(KOFSCD+LCDRCD+1)
          CALL MZDROP (0, LCDRCD, ' ')
          LCDRCD = LUP
          GO TO 30
        ENDIF
      ENDIF
*
  100 IRC    = 0
*                                                             END CDSUMY
  999 END
