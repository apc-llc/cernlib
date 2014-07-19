*
* $Id: iuhist.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: iuhist.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_IUHIST
%
% CERN PROGLIB# Y203    IUHIST          .VERSION KERNNOR  1.05  810521
% ORIG.  H.OVERAS, CERN, 810609
%
% IX = IUHIST(A,PAR,SPILL)       HISTOGRAM BIN. PAR=NA,1./DA,ALOW
%
       EXPORT IUHIST
       ROUTINE IUHIST
       LIB IUHIST
VBAS:  STACK FIXED
ARG:   W BLOCK 3
       ENDSTACK
IUHIST:  ENTF VBAS
       W SET1 IND(B.ARG+8)
       W RLADDR IND(B.ARG+4)
       F SUB3 IND(B.ARG+0),R.8,R1
       F1 MULAD R.4,1.000001
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
#ifdef CERNLIB_TCGEN_IUHIST
#undef CERNLIB_TCGEN_IUHIST
#endif
