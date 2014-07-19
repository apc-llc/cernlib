*
* $Id: iulast.s,v 1.1.1.1 1996/02/15 17:54:02 mclareni Exp $
*
* $Log: iulast.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:02  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     FUNCTION IULAST(IT,IVEC,N)                 *** V 305 ***
.     IULAST (W,V,N) IS EQUIVALENT TO LNOTEQ (W,V,-N)
.     COMPARES THE WORD IT TO THE WORDS NUMBER N,N-1,...,1 OF ARRAY IVEC
.     THE SCANNING STOPS WITH IULAST=J AT THE FIRST WORD IVEC(J).NE.IT
.     RETURNS IULAST=0 IF ALL WORDS OF ARRAY IVEC CONTAIN IT
.     RETURNS  IULAST=0  IF N.EQ.0
. *   ERROR EXIT GERRB$ IF N.LT.0
. *   SINGLE PRECISION IT,IVEC
.
. *****$$$*****$$$*****$$$*****
          AXR$
          FORT$
$(1),FIRS01
          +         'IULAST'
IULAST*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A3,2,XARG
          DL        A0,0,XARG
          L         A2,0,A0
          LN        A0,0,A3
          JN        A0,LNOT6
          JZ        A0,RETUR3          . RETURNS IULAST=0 IF N.EQ.0
LMJER1    SLJ       GERR$              . IF N.LT.0
 
. *****$$$*****$$$*****$$$*****
.
.     FUNCTION  LNOTEQ( IT, IVEC, N )            *** V 305 ***
.     SCAN VECT FROM VECT(1) TO VECT(N) IF N.GT.0
.     SCAN VECT FROM VECT(N) TO VECT(1) IF N.LT.0
.     RETURNS THE LOCATION IN ARRAY VECT OF THE FIRST WORD .NE. IT
.     RETURNS 0 IF VECT(1) THRU VECT(N)  =  WORD
.     RETURNS LNOTEQ=0 IF N.EQ.0
. *   INTEGER OR SINGLE PRECISION  IT, IVEC  (SHOULD BE INTEGERS )
. *   BIT TO BIT COMPARISON  EXCEPTED IF  IT = +0 OR -0
.
. *****$$$*****$$$*****$$$*****
 
LNOTEQ*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A3,2,XARG          . LOC(N)
          DL        A0,0,XARG          . LOC(IT), LOC(IVEC)
          L         A2,0,A0            . IT
          L         A0,0,A3            . N
LNOT6
          JZ        A0,RETUR3          . RETURNS 0 IF N.EQ.0
          SMA       A0,R1
          JN        A0,LNT3
.                                      IT .NE. 0  AND  N.GT.0
          LXI,U     A1,1
          JZ        A2,LNT4
          SNE       A2,0,*A1
LNT1      L         A0,R1              . NO FIND
LNT2      AN        A0,R1              . FORTRAN LOCATION OF RESULT
          J         RETUR3
.                                      IT .NE. 0  AND  N.LT.0
LNT3      A,U       A0,1               . -N+1
          ANH       A1,A0              . LOC (VEC(N))
          LXI,XU    A1,-1
          JZ        A2,LNT6
          L,U       A0,1
          SNE       A2,0,*A1
          L,XU      R1,-1
          A         A0,R1
          J         RETUR3
.                                      IT = +0 OR -0  AND N.GT.0
LNT4      JGD       R1,LNT5            . N-1
LNT5      TZ        0,*A1              . LOOP
          J         LNT2
          JGD       R1,LNT5
          J         LNT1               . NO FIND
.                                      IT = +0 OR -0  AND N.LT.0
LNT6      LN        A0,A0              . N-1
LNT7      TNZ       0,*A1              . LOOP
          JGD       A0,LNT7
          A,U       A0,1               . FORTRAN LOCATION OF RESULT
          J         RETUR3
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,IULAST
          +         0,DYN$
          +         'IULAST'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'IULAST( IT,IV,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,067,2,LMJERR+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_IULAST
#undef CERNLIB_TCGEN_IULAST
#endif
