*
* $Id: noarg.s,v 1.1.1.1 1996/02/15 17:51:34 mclareni Exp $
*
* $Log: noarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:34  mclareni
* Kernlib
*
*
          IDENT NOARG
*
* CERN PROGLIB# Z029    NOARG           .VERSION KERNCDC  1.16  820129
*
*     SUBROUTINE NOARG (N)
*
*  A. YULE/L. POPE  CERN  DECEMBER 1971
*  CONDITIONAL ASSEMBLY FOR THE -RUN- COMPILER REMOVED JAN 1982
*
          ENTRY NOARG
          VFD   42/0LNOARG,18/1
 NOARG    DATA  0
*
*  ROUTINE FOR ZERO NUMBER OF ARGUMENTS
*
          SA2   NOARG         EQ00AAAAAA0000000000
          AX2   30            ALIGN ADDRESS OF CALL+1
          SA3   X2-1          GO BACK TO +RJ-TRACE
          SA2   X3            POINT TO TRACE WORD
          SX7   X2            LOOK AT LAST 18 BITS
          NZ    X7,ARGS
          SA7   X1            STORE AT ARG ADDRESS
          ZR    B0,NOARG      RETURN
*
*  CODE FOR NON-ZERO NUMBER OF ARGUMENTS
*
 ARGS     SA2   A0            POINT TO ARG LIST
          SB1   1             INIT INCREMENT REG
          SX7   B0            ZERO OUT X7 FOR COUNT
          ZR    X2,ENDSCAN    IF FIRST ADDRESS ZERO - END
 VFDSCAN  SA2   A2+B1         NEXT ARG ADDRESS
          SX7   X7+B1         INCREMENT COUNTER
          NZ    X2,VFDSCAN    SCAN UNTIL ZERO
 ENDSCAN  SA7   X1            STORE AT ARG ADDRESS
          ZR    B0,NOARG      RETURN
          END
#ifdef CERNLIB_TCGEN_NOARG
#undef CERNLIB_TCGEN_NOARG
#endif
