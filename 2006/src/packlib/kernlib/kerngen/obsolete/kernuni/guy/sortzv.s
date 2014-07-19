*
* $Id: sortzv.s,v 1.1.1.1 1996/02/15 17:54:01 mclareni Exp $
*
* $Log: sortzv.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:01  mclareni
* Kernlib
*
*
. **************************************
.
.     SUBROUTINE SORTZV( A, INDEX, N,  MODE, NWAY, NSORT )
.     METHOD = ALGORITHM 'QUICKERSORT', JACM
.     LUT IS THE STACK USED BY QUICKERSORT
 . *  DO NOTHING IF  N=0
. *   ERROR EXIT IF  N.LT .0
. *   DOES NOT CHECK  N.LT.2**17  ( N IS LIMITED BY THE DEPTH OF THE STACK)
 . *  SORT OF ALPHANUMERIC ARRAYS NOT IMPLEMENTED (USE INTEGER SORT)
.
. **************************************
.
          AXR$
          FORT$
SAVI      EQU       R1
SAVJ      EQU       R2
SAVIT     EQU       R3
SAVX11    EQUF      SAVXX,,H2
 
          ON  FTN
SAVXARG   EQUF      SAVXX,,H1
          OFF   . FTN
 
SLATE     INFO  2  2
$(2)
SAVX10    RES       1
SAVXX     RES       1
LUT       RES       38
 
$(1),FIRS01
          +         'SORTZV'
SORTZV*
          DL        A1,1,XARG         . LOC( INDEX ),  LOC( N )
          L         R1,0,A2           . N
          L         A3,5,XARG         . LOC( NSORT )
          TNZ       R1
          J         RETUR6            . DO NOTHING IF  N = 0
          JGD       R1,$+2
LMJER1    SLJ       GERR$             . ERROR EXIT IF N.LT.0
          TZ        0,A3
          J         SORT20            . GO TO 'CALL INSORT'
 
          LXI,U     A1,1              . FILL INDEX
          L,U       A4,1
SORT2     S         A4,0,*A1           . INDEX(I) = I
          A,U       A4,1
          JGD       R1,SORT2
SORT20
 DO FTN , S         XARG,SAVXARG
 
. ...................  INSORT
          S         X11,SAVX11
          L         A2,0,XARG
          S         X10,SAVX10
 DO FOR , L         A0,2,XARG
          L         X11,1,XARG
 DO FTN , L         A0,2,XARG
          L         A0,0,A0             . N
          L,U       X10,LUT
          AN,U      A2,1              . LOC( A(1) ) - 1
          AN,U      A0,1              . N - 1
          LXI,U     X10,2
          A         A0,X11            . LOC( INDEX(N) )
INT10
          ANU       A0,X11            . J - I
          TLE,U      A1,2
          J         INT200            . SEGMENT LENGTH .LE. 2
          AU        A0,X11            . I + J
          S         X11,SAVI
          SSA       A1,1              . IP = (I+J) / 2
          S         A0,SAVJ
          L         A4,0,A1           . IT = INDEX(IP)
          AU        A2,A4             .
          S         A4,SAVIT
          L         A5,0,A3           . T = A( IT )
          L         A4,0,X11          . INDEX(I)
          A,U       X11,1             . K= I+1
          S         A4,0,A1           . INDEX(IP) = INDEX(I)
INT20
          AU        A2,0,X11          . LOC( A(INDEX(K) ) )
          TG        A5,0,A3
          J         INT40             . A(INDEX(K)) .LE. T
INT24
          AU        A2,0,A0
          L         A4,0,A3           . A(INDEX(IQ) )
          TG        A4,A5
          J         INT30             . A(INDEX(IQ)) .GE. T
          L         A3,0,X11           . SWAP INDEX(K) AND INDEX(IQ)
          L         A4,0,A0           . I(IQ)
          S         A3,0,A0           . I(IQ) = I(K)
          S         A4,0,X11          . I(K) = I(IQ)
          AN,U      A0,1              . IQ = IQ - 1
INT40                                 .  NEXT  K
          A,U       X11,1             . K= K+1
          TG        A0,X11
          J         INT20
INT100
          L         X11,SAVI
          L         A4,0,A0           . I(IQ)
          AU        A0,A0             . IQ + IQ
          S         A4,0,X11          . I(I) = I(IQ)
          AN        A1,X11            . 2*IQ - I
          S         SAVIT,0,A0        . I(IQ) = IT
          AN        A1,SAVJ           . 2*IQ - (I+J)
          JN        A1,INT110
.              LARGER SEGMENT IS  ( I, IQ-1 )
          AN,U      A0,1
          S         X11,0,X10
          S         A0,1,*X10         . LIMITS SAVED IN LUT
          L,U       X11,2
          A         X11,A0            . NEW I = IQ+1
          L         A0,SAVJ           . NEW J = OLD J
          J         INT10
INT110         . LARGER SEGMENT IS   ( IQ+1, J )
          A,U       A0,1              . IQ+1
          L         A1,SAVJ
          DS        A0,0,*X10
          AN,U      A0,2              . NEW J = IQ-1
          J         INT10
INT30          .   NEXT  IQ
          AN,U      A0,1
          TG        A0,X11
          J         INT24
          J         INT100
INT200
          TE,U      A1,1              . IS IT A 2-WORD SEGMENT
          J         INT220            . NO
          DL        A4,0,X11          . INDEX(I), INDEX(I+1)
          AU        A2,A5
          L         A1,0,A3           . A(INDEX(I+1))
          AU        A2,A4
          TG        A1,0,A3
          J         INT220            . A(INDEX(I)) .LE. A(INDEX(I+1))
          DSC       A4,36
          DS        A4,0,X11
INT220
          A,XU      X10,-2              . X10 = X10 - 2
          L         A1,X10
          L         X11,0,X10
          LXI,U     A1,0
          L         A0,1,X10
          TG,U      A1,LUT
          J         INT10
          L         X10,SAVX10
          L         X11,SAVX11
. ..................... END INSORT
 
 DO FTN , L         XARG,SAVXARG
          L         A3,4,XARG         . LOC( NWAY )
          L         A5,0,A3           . NWAY
          JZ        A5,RETUR6
          DL        A1,1,XARG         . REORDER INDEX IF NWAY.NE.0
          L         A2,0,A2           . N
          L         A3,A1
          AN,U      A2,2
          JN        A2,RETUR6
          A         A3,A2
          LXI,U     A1,1
          SSA       A2,1              . N/2
          LXI,U     A3,-1
SORT44                        . REORDER
          L         A4,0,A1
          L         A5,1,A3
          S         A4,1,*A3
          S         A5,0,*A1
          JGD       A2,SORT44
          J         RETUR6
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS
#include "gerr_c.inc"
F        FORM       6,6,6,18
          INFO  010  03
$(3)
DIAG1     F         077,050,03,SORTZV
          +         0,DYN$
          +         'SORTZV'
          ON    GERMAX
DIAG2     F         077,067,02,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'SORTZV( ,,N,,, ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON    GERMAX=0
DIAG2     F         077,062,02,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_SORTZV
#undef CERNLIB_TCGEN_SORTZV
#endif
