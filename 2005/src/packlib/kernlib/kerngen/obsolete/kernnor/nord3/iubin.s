*
* $Id: iubin.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: iubin.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_IUBIN
%
% CERN PROGLIB# Y202    IUBIN           .VERSION KERNNOR  1.05  810521
% ORIG.  H.OVERAS, CERN, 810609
%
% IX = IUBIN(A,PAR,SPILL)         HISTOGRAM BIN. PAR=NA,DA,ALOW
%
       EXPORT IUBIN
       ROUTINE IUBIN
       LIB IUBIN
VBAS:  STACK FIXED
ARG:   W BLOCK 3
       ENDSTACK
IUBIN:  ENTF VBAS
       W SET1 IND(B.ARG+8)
       W RLADDR IND(B.ARG+4)
       F SUB3 IND(B.ARG+0),R.8,R1
       F DIV2 R1,R.4
       F1+1.000001
       F WCONV R1,R1
       IF<=GO UNDER
       W2:=R.0
       W1 COMP R2
       IF>GO OVER
       W STZ IND(B.ARG+8)
       RET
UNDER: W1 CLR
       RET
OVER:  W1:=R2
       W INCR R1
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_IUBIN
#undef CERNLIB_TCGEN_IUBIN
#endif
