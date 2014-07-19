*
* $Id: locbyt.s,v 1.1.1.1 1996/02/15 17:54:00 mclareni Exp $
*
* $Log: locbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:00  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.         FUNCTION LOCBYT( IT,VEC,N,INC,L,NBITS )***M 428 ***
.     SEARCHES VECTOR VEC EVERY INC WORD FOR THE FIRST WORD
.     WHICH CONTAINS 'IT' IN THE BYTE (L,NBITS)
.     THE ADDRESS OF THAT WORD IS THE FUNCTION VALVE
.     SCANS FROM VEC(1) TO VEC(1+((N-1)/INC)*INC) IF N.GT.0
.     SCANS THE SAME WORDS WITH REVERSED ORDER IF N.LT.0
.     RETURNS LOCBYT=0 IF NO FIND OR N=0
. *   ERROR EXIT GERRB$ IF INC.LT.0
. *   DOES NOT CHECK  L  AND  NBITS  VALUES
. *   USES N=1 IF INC=0
. *   THE COMPARISON DOES NOT USE THE FULL WORD IT , BUT
. *   ONLY THE NBITS RIGHMOST BITS
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1),FIRS01
          +         'LOCBYT'
LOCBYT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(IT), (A3) = LOC(VEC)
          L         A4,0,A2            . IT
          DL        A1,4,XARG          . LOC(L), LOC(NBITS)
          LN,XU     A5,0               . MASK = -0
          LN        A2,0,A2            . -NBITS
          LN        A1,0,A1            . -L
          SSL       A5,36,A2           . KEEP ONLY NBITS BITS IN MASK
          SSC       A4,37,A1           . IT SHIFTED LEFT L-1 PLACES
          SSC       A5,37,A1           . SHIFT LEFT L-1 PLACES TO GET THE MASK
          DL        A1,2,XARG          . LOC(N), LOC(INC)
          S         A5,R2
          L         A5,0,A2            . INC
          L         A2,0,A1            . N
          SZ        A0
          LMA       A1,A2              . IABS(N)
          JZ        A5,IUH20           . IF  INC=0  USE  N=1
          TP        A5
LMJER1    SLJ       GERR$              . ERROR EXIT IF INC.LT.0
          DI        A0,A5              . IABS(N)/INC
IUH20     TZ        A1
          A,U       A0,1               . COVERED DIVIDE
          S         A0,R1              . NSCAN
          AN,U      A0,1               . NSCAN-1
          JN        A2,IUH60
IUH30     LXI       A3,A5
 
          MSE       A4,0,*A3
          J         IUH80
          AN        A0,R1
          MSI       A0,A5
          A,U       A0,1
          J         RETUR6
IUH60                                  . N.LT.0
          MSI       A0,A5
          A         A3,A0
          LN        A5,A5
          SZ        A0
          J         IUH30
IUH80                                  . RETURN IF NO MATCH
          SZ        A0
          J         RETUR6
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,LOCBYT
          +         0,DYN$
          +         'LOCBYT'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'LOCBYT( BYT,VEC,N,INC,L,NB ) ERROR: INC<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_LOCBYT
#undef CERNLIB_TCGEN_LOCBYT
#endif
