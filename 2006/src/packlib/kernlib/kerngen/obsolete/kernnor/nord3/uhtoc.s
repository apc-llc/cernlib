*
* $Id: uhtoc.s,v 1.1.1.1 1996/02/15 17:54:50 mclareni Exp $
*
* $Log: uhtoc.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:50  mclareni
* Kernlib
*
*
       MODULE M_UHTOC
%
% CERN PROGLIB# M409    UHTOC           .VERSION KERNNOR  2.05  880419
% ORIG.  H.OVERAS, CERN, 811201
% MODIF. P.S.Iversen, 8-dec-87
%
% CALL UHTOC(AN,N,CHAR,NCH)   CONVERT HOLLERITH AN TO CHARACTER
%
       IMPORT-D SLATE
       EXPORT UHTOC
       ROUTINE UHTOC
       LIB UHTOC
VBAS:  STACK FIXED
PAR:   W BLOCK 4
MAX:   W BLOCK 1
NBLK:  W BLOCK 1
       ENDSTACK
UHTOC:  ENTF VBAS
       W1:=IND(B.PAR+12)
       IF <= GO OUT
       W1-1
       W1=:B.MAX
       W RLADDR IND(B.PAR+8)
       W MOVE R.4,B.PAR+8 %CHANGE POINTER TO STRING ITSELF
LAB1:  W1 CLR
        W2 CLR
       W3:=5
       W4:=IND(B.PAR+4)
       IF <= GO OUT
       W4 COMP 4
       IF <= GO LAB2
       W4:=4
LAB2:  W SUB3 4,R4,B.NBLK
       W2-B.NBLK
BEGLP: W3 COMP R4
       IF <= GO LAB3
       W2+B.NBLK
       W SET1 R3
LAB3:  BY MOVE IND(B.PAR)(R2),IND(B.PAR+8)(R1)
       W2+1
       W3+1
       W LOOPI R1,B.MAX,BEGLP
       W1-1
       W3 DIV4 R1,4,R2
       W2+1
       W2=:SLATE
OUT:   RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_UHTOC
#undef CERNLIB_TCGEN_UHTOC
#endif
