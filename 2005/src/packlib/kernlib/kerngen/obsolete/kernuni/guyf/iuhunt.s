*
* $Id: iuhunt.s,v 1.1.1.1 1996/02/15 17:54:02 mclareni Exp $
*
* $Log: iuhunt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:02  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     FUNCTION IUHUNT( IT,IVEC,N,INC )           *** V 304 ***
.     COMPARES THE WORD IT TO WORDS IVEC(1),IVEC(1+INC)..IVEC(1+((N-1)/INC)*INC)
.     IF N.GT.0
.     COMPARES THE WORD IT TO THE SAME WORDS WITH REVERSED ORDER IF N.LT.0
.     RETURNS THE FORTRAN LOCATION J OF THE FIRST WORD SUCH THAT IT=IVEC(J)
.     RETURNS  IUHUNT=0  IF THE COMPARISON FAILS
.     RETURNS  IUHUNT=0 IF  N.EQ.0
. *   ERROR EXIT  GERRB$ IF  INC.LT.0
. *   WORKS IF  IT = +0 OR -0
. *   USES N=1 IF INC=0
. *   SINGLE PRECISION IT,IVEC ---- DO NOT CHECK INC.LT.2**17
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1),FIRS01
          +         'IUHUNT'
IUHUNT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(IT),LOC(IVEC)
          DL        A0,2,XARG          . LOC(N), LOC(INC)
          L         A5,0,A0            . N
          L         A4,0,A1            . INC
          SZ        A0
          LMA       A1,A5              . IABS(N)
          JZ        A4,IUH1            . IF INC=0 USE N=1
          TP        A4
LMJER1    SLJ       GERR$              . ERROR EXIT IF  INC.LT.0
          DI        A0,A4              . IABS(N)/INC
IUH1
          TZ        A1
          A,U       A0,1
          S         A0,R1
          AN,U      A0,1
          JN        A5,IUH3
IUH2
          L         A5,0,A2            . IT
          LXI       A3,A4
          JZ        A5,IUH4
          SE        A5,0,*A3
          J         IUH6               . NO FIND
IUH21     AN        A0,R1              . COMPUTE FORTRAN LOCATION OF RESULT
          MSI       A0,A4
          A,U       A0,1
          J         RETUR4
.                                        N.LT.0
IUH3      MSI       A0,A4
          A         A3,A0              . LOC(LAST WORD) IN IVEC = FIRST TO SCAN
          LN        A4,A4
          SZ        A0
          J         IUH2
.                                      IT = 0
IUH4      JGD       R1,IUH5
          J         IUH6               . IF N.EQ.0
IUH5      TNZ       0,*A3
          J         IUH21
          JGD       R1,IUH5
IUH6      SZ        A0                 . NO FIND
          J         RETUR4
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,IUHUNT
          +         0,DYN$
          +         'IUHUNT'
          ON GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'IUHUNT(IT,IVEC,N,INC) ERROR: INC<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_IUHUNT
#undef CERNLIB_TCGEN_IUHUNT
#endif
