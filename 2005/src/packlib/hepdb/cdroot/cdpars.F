*
* $Id: cdpars.F,v 1.1.1.1 1996/02/28 16:24:32 mclareni Exp $
*
* $Log: cdpars.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPARS (PATHY, NMAX, CNODE, NCHL, NODES)
*     ===================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPARS (PATHY, NMAX, CNODE*, NCHL*, NODES*)             *
*                                                                      *
*   Splits a pathname into its component pieces (nodes)                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHY    Input  path name                                        *
*     NMAX     Maximum number of nodes                                 *
*     CNODE    Node names                                              *
*     NCHL     Number of characters in CNODE                           *
*     NODES    Number of nodes in PATHY                                *
*                                                                      *
*   Called by CDFREE, CDKEEP, CDNODE                                   *
*                                                                      *
************************************************************************
*
      CHARACTER*(*)   PATHY, CNODE(9)
      DIMENSION       NCHL(9)
*     ------------------------------------------------------------------
*
* *** Get list of Nodes
*
      NODES  = 0
      MAXL   = LEN (PATHY)
      I0     = 0
   10 I1     = I0 + 1
      IF (I1.LE.MAXL) THEN
        DO I0 = I1, MAXL
          IF (PATHY(I0:I0).EQ.' ') GO TO 20
          IF (PATHY(I0:I0).EQ.'/') GO TO 15
        ENDDO
        I0     = MAXL + 1
   15   I2     = I0 - 1
        GO TO 30
   20   I2     = I0 - 1
        I0     = MAXL + 1
   30   IF (I2.LT.I1)              GO TO 10
        NODES  = NODES + 1
        IF (NODES.LE.NMAX) THEN
          CNODE(NODES) = PATHY(I1:I2)
          NCHL (NODES) = I2 - I1 + 1
        ENDIF
        GO TO 10
      ENDIF
*                                                             END CDPARS
      END
