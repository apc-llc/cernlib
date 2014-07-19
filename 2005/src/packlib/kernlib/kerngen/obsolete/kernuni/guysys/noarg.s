*
* $Id: noarg.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: noarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
*  .CONSIDERED OBSOLETE, BETTER USE HINTRFC$...
#if defined(CERNLIB_NOARG)
. *************************************
.
.     SUBROUTINE  NOARG( NARG )                  *** Z 029 ***
. *   VERSION FOR FORTRAN COMPILER 11 A
.     THE FOLLOWING SEQUENCE
.     ---    SUBROUTINE AAA( A,B,C,D,$,F,G)  OR FUNCTION AAA( ... )
.     ---       ...
.     ---    CALL NOARG( I )
.     ---    ...
.     ---    RETURN
.     RETURNS  I=NUMBER OF ARGUMENTS THE ROUTINE AAA IS CALLED WITH
.     AND PROVIDES A CORRECT RETURN THROUGH THE RETURN INSTRUCTION
. *   SHOULD NOT BE CALLED FROM THE MAIN PROGRAM
. *   STORE THE NUMBER OF ARGUMENTS+1 IN  INJP$  OF ROUTINE AAA
. *   THEN YOU DON'T HAVE TO WORRY FOR THE RETURN INSTRUCTION
. *   NOARG MUST BE CALLED IF THE NUMBER OF ARGS. IS NOT CONSTANT
.
. *************************************
 
          AXR$
          FORT$
$(1)
NOARG*
          L,H2      A0,1,X11
          L,H2      A1,1,A0
          LXI,U     A1,1
N1        L,H1      A2,0,*A1
          JZ        A2,N1
          TG,U      A2,010000
          J         N3
N2        ANH       A1,1,A0
          AN,U      A0,2
          ANU,XU    A1,1
          S,H2      A1,0,A0
          S,H2      A2,*0,X11
          J         2,X11
N3        TE,U      A2,0742000
          J         N2
          J         N1
 
. ------------------
 
.     FUNCTION VAL( X )
. ENTRY POINTS : IVAL,VAL,LVAL,DVAL,CVAL FOR INTEGER,REAL,LOGICAL,
. DOUBLE PRECISION,COMPLEX ARGUMENTS
.     VAL = X
.     RETURN
.     END
 
CVAL*
DVAL*
IVAL*
LVAL*
VAL*
          DL        A0,*0,X11
          J         2,X11
          END
#endif
. **************************************
.
.     SUBROUTINE NOARG( N )
.     COMMON/ HNOARG/ NARG
.     N = NARG
.     RETURN   END
.
. **************************************
 
          AXR$
          FORT$
 
HNOARG    INFO  2  2
$(2),NARG  RES  1
 
$(1)
NOARG*
          L         A2,0,XARG
          L         A4,NARG
          S         A4,0,A2
          J         RETUR1
          END
