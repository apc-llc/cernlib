*
* $Id: iusame.s,v 1.1.1.1 1996/02/15 17:54:03 mclareni Exp $
*
* $Log: iusame.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:03  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     FUNCTION IUSAME(A,JL,JR,MIN,LOC)           *** M 501 ***
.     SCANS VECTOR A(JL) THRU A(JR) TO FIND A STRING OF SAME WORDS,
.     AT LEAST MIN WORDS LONG , IF ANY
.     RETURNS IUSAME=NUMBER OF WORDS IN THE STRING
.     RETURNS LOC=FORTRAN INDEX OF THE FIRST WORD OF THE STRING
.     RETURNS IUSAME=0 AND LOC=JR+1 IF NO FIND
. *   IF JL.LE.0 USES JL=1
. *   ERROR EXIT IF  JR.LT.MAX0(1,JL)  OR  MIN.LE.0
.     THIS ROUTINE IS USED BY THE DUMP PROCESSORS.
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1),FIRS01
          +         'IUSAME'
IUSAME*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,1,XARG          . LOC(JL), LOC(JR)
          L         A2,0,A2            . JL
          LN        A5,0,A3            . -JR
          TLE,U     A2,1
          L,U       A2,1               . JL = MAX0(1,JL)
          AU        A2,A5              . (A3) = JL-JR
          A         A2,0,XARG          . LOC(A(JL)) + 1
          DL        A0,3,XARG          . LOC(MIN), LOC(LOC)
          AN,U      A2,1               LOC(A(JL))
          L         A0,0,A0            . MIN
          SMA       A3,R1              . LOOP COUNT = JR-JL
          TG,U      A0,1
          TG,U      A3,1
LMJER1    SLJ       GERR$              . IF MIN.LE.0 .OR. JR.LT.JL
          LXI,U     A2,1
                                       . LOOP
IUS20     L         A3,R1              . SAVE INITIAL VALUE OF COUNTER
          L         A4,0,A2            . A(JR-(R1))
          SNE       A4,1,*A2
          J         IUS40
          ANU       A3,R1              . NUMBER OF SAME WORDS
          TLE       A4,A0              . COMPARE TO MIN
          J         IUS20              . .LT.MIN, TRY AGAIN
IUS30
          L         A0,A4
IUS31     A         A3,A5
          SMA       A3,0,A1
          J         RETUR5
IUS40                                  . END OF SCAN
          AU,U      A3,1
          TLE       A4,A0
          J         IUS44
          J         IUS30
IUS44     LN,U      A3,1
          SZ        A0
          J         IUS31
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,IUSAME
          +         0,DYN$
          +         'IUSAME'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'IUSAME(A,JL,JR,MIN,L) ERROR: MIN.LE.0 OR JR<MAX0(1,JL)'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_IUSAME
#undef CERNLIB_TCGEN_IUSAME
#endif
