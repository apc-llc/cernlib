*
* $Id: ucopyn.s,v 1.1.1.1 1996/02/15 17:54:50 mclareni Exp $
*
* $Log: ucopyn.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:50  mclareni
* Kernlib
*
*
       MODULE M_UCOPYN
%
% CERN PROGLIB# V301    UCOPYN          .VERSION KERNNOR  2.05  880419
% ORIG.  H.OVERAS, CERN, 791213
% MODIF. P.S.Iversen, 8-dec-87
%
% CALL UCOPYN(IV,IXV,N)      COPY NEGATIVE, IXV(J)=-IV(J)
%
       EXPORT UCOPYN
       ROUTINE UCOPYN
       LIB UCOPYN
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
UCOPYN:  ENTF VBAS
       W1:=0
       W2:=IND(B.PAR+8)
       IF <= GO OUT                              % If <= zero, exit
       W2-1
BEGLP: W3:=IND(B.PAR)(R1)
       W3 NEG
       W3=:IND(B.PAR+4)(R1)
       W LOOPI R1,R2,BEGLP
OUT:   RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_UCOPYN
#undef CERNLIB_TCGEN_UCOPYN
#endif
