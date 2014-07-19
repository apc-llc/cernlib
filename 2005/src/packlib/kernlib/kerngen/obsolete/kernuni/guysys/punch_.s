*
* $Id: punch_.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: punch_.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
. ********************************
.
.     SUBROUTINE  PUNCH( TEXT, NWORDS )
.     PUNCHES CARD IMAGE  TEXT(1) TO TEXT(NWORDS)
. *   ERROR EXIT  GERRB$  IF  NWORDS.LT.-0 .OR. NWORDS.GT.14
.
. ********************************
 
          AXR$
          FORT$
 DO FOR ,MXWRD  EQU 14
 DO FTN ,MXWRD  EQU 20
 DO FOR ,PPUNCH  EQU PUNCH$
 DO FTN ,PPUNCH  EQU APUNCH$
$(1),FIRS01
          +         'PUNCH '
PUNCH*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A0,0,XARG          . LOC(TEXT), LOC(NW)
          LN        A1,0,A1            . -NWORDS
          TLE,U     A1,1
          TLE,XU    A1,-MXWRD
LMJER1    SLJ       GERR$              . NWORDS OUT OF RANGE (0,MXWRD)
          LN        A1,A1
          LXI       A0,A1
          ER        PPUNCH
          J         RETUR2
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,PUNCH
          +         0,DYN$
          +         'PUNCH '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'PUNCH( A,N ),N NOT WITHIN 0,14'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
