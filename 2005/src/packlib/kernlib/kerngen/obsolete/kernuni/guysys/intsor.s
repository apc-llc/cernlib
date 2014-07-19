*
* $Id: intsor.s,v 1.1.1.1 1996/02/15 17:54:07 mclareni Exp $
*
* $Log: intsor.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:07  mclareni
* Kernlib
*
*
. **************************************
.
.     SUBROUTINE  INTSOR( A, N )                 *** M 103 ***
.     SUBROUTINE  FLPSOR( A, N )
.     SORTS THE N-VECTOR A INTO ITSELF, USING ASCENDING ORDER
.     METHOD = ALGORITHM 'QUICKERSORT' ALGORITM 241, COMM ACM 8(1965)P.669
.     A IS AN ARRAY OF INTEGER VALUES, OR REAL VALUES, OR ALPHANUMERIC VALUES
.     (THE COLLATING SEQUENCE IS THEN FIELDATA OR ASCII)
. *   DO NOTHING IF  N=0
. *   ERROR EXIT IF  N.LT.0
. *   DOES NOT CHECK  N.LT.2**18 ( N LIMITED BY THE DEPTH OF THE STACK )
.
. **************************************
 
          AXR$
          FORT$
SAVI      EQU       R2
SAVJ      EQU       R3
 
SLATE     INFO      2  2
$(2),LUT  RES       40
.     LUT IS THE STACK USED BY QUICKERSORT
 
$(1),FIRS01
          +         'INTSOR'
FLPSOR*
INTSOR*
          DL        A1,0,XARG
          L,U       A0,2
          L         A2,0,A2            . N
          SSC       A0,18              . A0 = 2,0 . INDEX WITHIN LUT
          JZ        A2,RETUR2         . DO NOTHING IF  N=0
          JGD       A2,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF  N.LT.0
          A         A2,A1              . LOC( A(N) )
INT10
          ANU       A2,A1              . J-I
          TLE,U     A3,2
          J         INT200             . SEGMENT LENGTH IS .LE. 2 WORDS
          AU        A2,A1              . I + J
          DS        A1,SAVI            . SAVE I AND J. A1,A2 HOLD K, IQ
          SSA       A3,1               . IP = (I+J)/2
          L         A4,0,A1            . A(I)
          L         A5,0,A3            . T = A(IP)
          S         A4,0,A3            . A(IP) = A(I)
          A,U       A1,1               . K = I + 1
INT20
          TG        A5,0,A1
          J         INT40              . A(K) .LE. T
INT24
          L         A4,0,A2            . A(IQ)
          TG        A4,A5
          J         INT30              . A(IQ) .GE. T
          L         A3,0,A1            . A(K)
          S         A4,0,A1            . A(K) = A(IQ)
          S         A3,0,A2            . A(IQ) = A(K)
          AN,U      A2,1
INT40                         .  NEXT  K
          A,U       A1,1               . K= K + 1
          TG        A2,A1
          J         INT20
INT100                        . SEGMENT SLITTED, STORE LARGER, PROCESS SHORTER
          L         A1,SAVI
          L         A4,0,A2            . A(IQ)
          AU        A2,A2
          S         A4,0,A1            . A(I) = A(IQ)
          AN        A3,A1
          S         A5,0,A2            . A(IQ) = T
          AN        A3,SAVJ
          JN        A3,INT110
.                             . LARGER IS ( I, IQ-1 )
          AN,U      A2,1               . IQ-1
          DS        A1,LUT,*A0
          L,U       A1,2
          A         A1,A2              . NEW I = IQ + 1
          L         A2,SAVJ            . NEWJ = OLD J
          J         INT10
INT110                        . LARGER IS  ( IQ+1, J )
          A,U       A2,1               . IQ+1
          L         A3,SAVJ            . J
          DS        A2,LUT,*A0         . STORE SEG( IQ+1, J )
          AN,U      A2,2               . NEW J = IQ - 1
          J         INT10
INT30                         .   NEXT  IQ
          AN,U      A2,1
          TG        A2,A1
          J         INT24
          J         INT100
INT200              . A SEGMENT WITH 0, 1, OR 2 ELTS
          TE,U      A3,1               . IS IT A 2-WORD SEGMEBT
          J         INT220             . NO
          DL        A3,0,A1            . A(I) AND A(I+1)
          TG        A4,A3              . ALREADY ORDERED
          J         INT220             . YES
          DSC       A3,36
          DS        A3,0,A1
INT220
          AN,U      A0,2               . DECREMENT STACK LUT
          AND,U     A0,0700            . TRICK TO CHECK A0.LT.0
          JNZ       A1,RETUR2
          DL        A1,LUT,A0
          J         INT10              . PROCESS SEGMENT TAKEN FROM STORE
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO  010  03
$(3)
DIAG1     F         077,050,04,INTSOR
          +         0,DYN$
                    'INTSORFLPSOR'
          ON    GERMAX
DIAG2     F         077,067,02,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'INTSOR,FLPSOR( A,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON    GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
