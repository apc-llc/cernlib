*
* $Id: sigop2.F,v 1.1.1.1 1995/12/12 14:36:17 mclareni Exp $
*
* $Log: sigop2.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:17  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/03 10/02/94  17.17.04  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIGOP2(A,B,X)
C
C
C    ....................................................
C         PURPOSE
C            PROCESS SIFAM0: KLASS=08 TO 21 AND SYSFUNS OF TWO ARGUMENTS
C
C         USAGE
C            CALL OP2(A,B,X)
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: DYNA(),KLASS,IZADA,IZMODA
C
C         REMARKS
C
C            THE SELECTION OF OP2 OR OP2COM IS DONE BY OPER2 USING THE V
C            OF MODE WHRERE MODE IS THE MODE OF THE RESULT
C            MODE=1 FORCED FOR KLASS=13 TO 21 (RELAIONALS, LOGICALS)
C            HENCE THEY ARE HANDLED ENTIRELY BY OP2
C            MODE=2  IS FORCED FOR KLASS=204  (CPLX)
C            HENCE CPLX IS HANDLED ENTIRELY BY OP2COM
C
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            BOO
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
C
C
      DIMENSION IAD(3),JMIP(3)
      EQUIVALENCE (IAD(1),IZAD(1),IADA),(IADB,IZADB)
      EQUIVALENCE (JMIP(1),IMODA,IZMOD(1)),(IMODB,IZMODB)
C
      DIMENSION RCA(2),RCB(2),RCX(2)
      EQUIVALENCE (CPLA,RCA(1)),(CPLB,RCB(1)),(CPLX,RCX(1))
      EQUIVALENCE(RA,RCA(1)),(CA,RCA(2)),(RB,RCB(1)),(CB,RCB(2))
     +,(RX,RCX(1)),(CX,RCX(2))
C
C 201    MOD
C
C 202    ATAN2
C
C 203    SIGN
C
C 204    CPLX
C
C 205    LS
C
C 206    MIN2
C
C 207    MAX2
C
C 208    MINV
C
C 209    MAXV
C
C
C
C
      X=0.
C
C
C
C   IF TWO ARGUMENT SYSFUNS  (SEE SYSFUN)
      IF(KLASS.GT.200) GOTO 200
C
C     KLASS=  8 ... **
C             9 ... *
C            10 ... /
C            11 ... -
C            12 ... +
C   THE FOLLOWING ARE HANDLED IN OP2 ONLY AND NEVER IN OP2COM
C            13 ... EQ
C            14 ... NE
C            15 ... GT
C            16 ... LT
C            17 ... GE
C            18 ... LE
C            19 ... CONCAT      (NOW HANDLED IN OPER2)
C            20 ... AND
C            21 ... OR
C
      K=KLASS-7
      GOTO(8,9,10,11,12,13,14,15,16,17,18,19,20,21),K
C
C
    8 CONTINUE
C 8                           **  OR  '
C   IF ZERO TO ANY POWER, LEAVE RESULT SET TO ZERO
      IF(A.EQ.0) GOTO 999
C   IF NOT INTEGER EXPONENT
      IB=IFIX(B)
      IF(B.NE.FLOAT(IB)) GOTO 91
      X=A**IB
      GOTO 999
C
C
   91 CONTINUE
C   IF A IS NEATIVE (EXPONENT IS FRACTIONAL HERE)
      IF(A.LT.0) GOTO 92
      X=A**B
      GOTO 999
C
C
   92 CONTINUE
C   NEGATIVE A TO NON-INTEGER POWER NEEDS COMPLEX RESULT
C   USE IZADA AS INDICATOR TO TRY AGAIN WITH COMPLEX  (OP2COM)
      IZADA=-1
      RETURN
C
C
    9 CONTINUE
C 9                           *
      X=A*B
      GOTO 999
C
C
   10 CONTINUE
C 10                           /
      IF(B.EQ.0.) GO TO 999
      X=A/B
      GOTO 999
C
C
   11 CONTINUE
C 11                           -
      X=A-B
      GOTO 999
C
C
   12 CONTINUE
C 12                           +
      X=A+B
      GOTO 999
C
C
   13 CONTINUE
C 13                           EQ   EQUALS TO
C   DO NOT IGNORE IMAGINARY PART AS OPERATION IS WELL DEFINED FOR CPLX
C   IF REAL PARTS NOT EQUAL
      IF(A.NE.B) GOTO 999
C   IF IMAGINARY PARTS NOT EQUAL
      X=1.
      GOTO 999
C
C
   14 CONTINUE
C 14                           NE   NOT EQUALS
C   DO NOT IGNORE IMAGINARY PART AS OPERATION IS WELL DEFINED FOR CPLX
      IF(A.EQ.B)GO TO 999
C
C      IF(CA.EQ.CB) GOTO 999
      X=1.
      GOTO 999
C
C
   15 CONTINUE
C 15                           GT   GREATHER THAN
C   IGNORE IMAGINARY PART
C
      IF(A.GT.B)X=1.
      GOTO 999
C
C
   16 CONTINUE
C 16                           LT   LESS THAN
C   IGNORE IMAGINARY PART
      IF(A.LT.B)X=1.
      GOTO 999
C
C
   17 CONTINUE
C 17                           GE   GREATHER OR EQUAL
C   IGNORE IMAGINARY PART
      IF(A.GE.B)X=1.
      GOTO 999
C
C
   18 CONTINUE
C 18                           LE   LESS OR EQUAL
C   IGNORE IMAGINARY PART
      IF(A.LE.B)X=1.
      GOTO 999
C
C
   19 CONTINUE
C 19                           ?    CONC (NOW HANDLED BY OPER2 DIRECTLY
      CALL SINERR(6)
      RETURN
C
C
   20 CONTINUE
C 20                           &    AND
C   IGNORE IMAGINARY PART
      IF((SIBOOL(A)+SIBOOL(B)) .EQ.2.)X=1.
      GOTO 999
C
C
   21 CONTINUE
C 21                           !    OR
C   IGNORE IMAGINARY PART
      IF((SIBOOL(A)+SIBOOL(B)) .GE.1.)X=1.
      GOTO 999
C
C
C
  200 CONTINUE
C   SYSFUNS WITH TWO ARGS GET HERE ONLY IF RESULT IS REAL
      K=KLASS-200
      GOTO(201,202,203,999,999,206,207,208,209),K
C
C
  201 CONTINUE
C 201                        MOD   REMAINDERING
      IF(B.NE.0.)X=AMOD(A,B)
      GOTO 999
C
C
  202 CONTINUE
C 202                        ATAN2
      X=0.
      IF(A.EQ.0..AND.B.EQ.0.)GOTO 999
      X=ATAN2(A,B)
      GOTO 999
C
C
  203 CONTINUE
C203                  SIGN    MAGNITUDE OF FIRST ARG WITH SIGN OF SECOND
      X=SIGN(A,B)
      GOTO 999
C
C 204              CPLX      SHOULD NEVER COME HERE  (OP2COM ONLY)
C 205              LS        SHOULD NEVER COME HERE  (OPER2 ONLY)
C
  206 CONTINUE
C 206                        MIN2
      X=A
      IF(A.GT.B)X=B
      GOTO 999
C
C
  207 CONTINUE
C 207                        MAX2
      X=A
      IF(A.LT.B)X=B
      GOTO 999
C
C
  208 CONTINUE
C 208                        MINV
      X=A
      IF(A.GT.B)X=B
      GOTO 999
C
C
C
  209 CONTINUE
C 209                        MAXV
      X=A
      IF(A.LT.B)X=B
      GOTO 999
C

 999  END
