*
* $Id: iufind.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: iufind.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     FUNCTION IUFIND( IT,IVEC,JL,JR )           *** V 304 ***
.     COMPARES THE WORD IT TO WORDS IVEC(JL), ... ,IVEC(JR)
.     RETURNS IUFIND=J AT FIRST MATCH IT=IVEC(J)
.     RETURNS IUFIND=JR+1 IF THE COMPARISON FAILS
. *   IF JL.LE.0 REPLACE BY JL=1
. *   ERROR EXIT  GERRB$  IF JR.LT.MAX0(1,JL)
. *   WORKS IF  IT = +0 OR -0
. *   SINGLE PRECISION IT,IVEC
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1),FIRS01
          +         'IUFIND'
IUFIND*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG          . LOC(JL), LOC(JR)
          DL        A0,0,XARG         . LOC( IT ),( LOC( IVEC )
          L         A2,0,A2            . JL
          L         A4,0,A0           . IT
          L         A0,0,A3            . JR
          JGD       A2,$+2
          SZ        A2                 . MAX0(1,JL)-1
          ANU       A2,A0              . -JR+JL-1
          A         A2,A1              . LOC(IVEC(JL))
          TN        A3
LMJER1    SLJ       GERR$              . IF(JR-JL+1).LT.0
          SMA       A3,R1
          LXI,U     A2,1
          JZ        A4,IUF2
          SE        A4,0,*A2
          L,XU      R1,-1
IUF1      AN        A0,R1              . FORTRAN LOCATION OF RESULT
          J         RETUR4
.                                      IT = +0 OR -0
IUF2      JGD       R1,IUF3
IUF3      TZ        0,*A2
          JGD       R1,IUF3
          J         IUF1
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3),DIAG1  F       077,050,3,IUFIND
          +         0,DYN$
          +         'IUFIND'
          ON GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +        LDIAG3-2,DIAG2
 'IUFIND(IT,IVEC,JL,JR) ERROR: JR<MAX0(1,JL)'
LDIAG3    EQU       $-DIAG3
          OFF
          ON GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_IUFIND
#undef CERNLIB_TCGEN_IUFIND
#endif
