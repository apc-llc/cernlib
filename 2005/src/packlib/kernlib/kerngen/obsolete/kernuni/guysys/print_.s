*
* $Id: print_.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: print_.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
. *************************************
.
.     SUBROUTINE PRINT( TEXT, NWORDS, NSKIP )
.     FIRST SKIP NSKIP LINES ON PRINT FILE,THEN PRINT NWORDS WORDS
.     STARTING AT ADDRESS TEXT
.     ALLOWED VALUES FOR NSKIP ARE  +0 TO +63
. *   ERROR EXIT  GERRB$  IF NWORDS.LT.-0 .OR. NWORDS.GT.22
.
. *************************************
 
          AXR$
          FORT$
 DO FOR ,MXWRD  EQU 22
 DO FTN ,MXWRD  EQU 33
 DO FOR ,PPRINT  EQU PRINT$
 DO FTN ,PPRINT  EQU APRINT$
$(1),FIRS01
          +         'PRINT '
PRINT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,1,XARG          . LOC(NW), LOC(NSKIP)
          L         A0,0,XARG          . LOC(TEXT)
          LN        A2,0,A2            . -NWORDS
          LM        A3,0,A3            . IABS(NSKIP)
          TLE,U     A2,1
          TLE,XU    A2,-MXWRD
LMJER1    SLJ       GERR$              . NWORDS OUT OF RANGE (0,MXWRD)
          LSSC      A3,6
          AM        A3,A2
          LXI       A0,A3
          ER        PPRINT
          J         RETUR3
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,PRINT
          +         0,DYN$
          +         'PRINT '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'PRINT( A,NW,NS ),NW NOT WITHIN 0,22'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
 END
