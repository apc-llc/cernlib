*
* $Id: vadd.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vadd.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE VADD( A,B,X,N )                 *** F121 ***
.     ADDS VECTOR B TO VECTOR A, STORES RESULT IN X
.     SUBROUTINE VSUB( A,B,X,N )
.     ADDS VECTOR -B TO VECTOR A
.     A AND B ARE SINGLE PRECISION, FLOATING POINT ARRAYS
.     SUBROUTINE IVADD( IA,IB,IX,N )
.     ADDS VECTOR IB TO VECTOR IA, STORES RESULT IN IX
.     SUBROUTINE IVSUB( IA,IB,IX,N )
.     ADDS VECTOR -IB TO VECTOR IA
.     IA AND IB ARE INTEGER ARRAYS
.
. *   DO NOTHING IF N.EQ.0
. *   ERROR EXIT GERRB$ IF N.LT.0
. *   NO CHECK FOR OVERLAPS
. *   (THE END OF X MAY OVERLAP THE BEGINNING OF A OR B BUT NOT VICE-VERSA)
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(0)
VA       FA     A4,0,*A1 .
IVA      A      A4,0,*A1 .
VS       FAN    A4,0,*A1 .
IVS      AN     A4,0,*A1 .
$(1),FIRS01
          +         'IVSUB '
IVSUB*
         LA     A5,IVS .
         J      ENTRY .
          +         IVADD '
IVADD*
         LA     A5,IVA .
         J      ENTRY .
          +         'VSUB  '
VSUB*
         LA     A5,VS .
         J      ENTRY .
          +         'VADD  '
VADD*
         LA     A5,VA .
ENTRY
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG          . LOC(X), LOC(N)
          DL        A0,0,XARG         . LOC(A), LOC(B)
          L         A3,0,A3            . N
          S         A5,CHG
          LXI,U     A0,1
          JZ        A3,RETUR4          . DO NOTHING IF N.EQ.0
          LXI,U     A1,1
          JGD       A3,$+2
LMJER1    SLJ       GERR$              . ERROR EXIF IF N.LT.0
          LXI,U     A2,1
VADD12    L         A4,0,*A0
CHG       +         $-$                . MODIFIED ACCORDING TO ENTRY POINT
          S         A4,0,*A2
          JGD       A3,VADD12
          J         RETUR4
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
          ON        GERMAX
DIAG1     F         077,050,3,IVSUB
          +         0,DYN$
          +         'IVSUB '
DIAG11    F         077,050,3,IVADD
          +         0,DYN$
          +         'IVADD '
DIAG21    F         077,050,3,VSUB
          +         0,DYN$
          +         'VSUB  '
DIAG31    F         077,050,3,VADD
          +         0,DYN$
          +         'VADD  '
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VADD,VSUB,IVADD,IVSUB( A,B,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG1     F         077,050,4,VADD
          +         0,DYN$
          +         'VADD,VSUB   '
DIAG11    F         077,061,2,IVSUB
          +         0,DIAG1
DIAG21    F         077,061,2,IVADD
          +         0,DIAG1
DIAG31    F         077,061,2,VSUB
          +         0,DIAG1
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
 END
#ifdef CERNLIB_TCGEN_VADD
#undef CERNLIB_TCGEN_VADD
#endif
#ifdef CERNLIB_TCGEN_VSUB
#undef CERNLIB_TCGEN_VSUB
#endif
