*
* $Id: ucopiv.s,v 1.1.1.1 1996/02/15 17:54:05 mclareni Exp $
*
* $Log: ucopiv.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:05  mclareni
* Kernlib
*
*
. **************************************
.
.         SUBROUTIN( UCOPIV( A, B, N )           *** V 301 ***
.         COPY A(1)...A(N) INTO B(N)...B(1)
.         DO NOTHING IF N=0
. *   ERROR EXIT GERR$ IF OVERLAP OR N.LT.0
. *   THE CONDITION FOR NO OVERLAP ID N-1.LE.IABS(LOC(B)-LOC(A))
. *   ( A ONE-WORD OVERLAP IS POSSIBLE)
.
. **************************************
 
          AXR$
          FORT$
 
$(1),FIRS01
          +         'UCOPIV'
UCOPIV*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A3,2,XARG          . LOC(N)
          DL        A0,0,XARG          . LOC(A), LOC(B)
          L         A3,0,A3            . N
          ANU       A1,A0              .
          JZ        A3,RETUR3          . DO NOTHING IF N.EQ.0
          LMA       A2,A2              . (A2)=IABS(B-A)
          SMA       A3,R1
          JGD       A3,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF OVERLAP OR N.LT.0
          TLE       A2,A3
          J         LMJER1             . ERROR EXIT IF N-1.GT.ABS(B-A)
          A         A1,A3              . LOC (B(N) )
          LXI,U     A0,1
          LXI,U     A1,-1
          BT        A1,0,*A0
          J         RETUR3
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,UCOPIV
          +         0,DYN$
                    'UCOPIV'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'UCOPIV( A,B,N ) ERROR: OVERLAP OR N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
 END
#ifdef CERNLIB_TCGEN_UCOPIV
#undef CERNLIB_TCGEN_UCOPIV
#endif
