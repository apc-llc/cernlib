*
* $Id: incbyt.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: incbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_INCBYT
%
% CERN PROGLIB# M423    INCBYT          .VERSION KERNNOR  1.10  811116
% ORIG.  H.OVERAS, CERN, 810609
%
% IX = INCBYT(INC,A,JTH,NBITS)     INCREMENT PACKED BYTE
%                                  NBITS(3)=NBIT,INWORD,MA
%
       EXPORT INCBYT
       ROUTINE INCBYT
       LIB INCBYT
VBAS:  STACK FIXED
PAR:   W BLOCK 4
       ENDSTACK
INCBYT:  ENTF VBAS
       W RLADDR IND(B.PAR+12)
       W4:=IND(B.PAR+8)
       W4-1
       W2 DIV4 R4,R.4,R4
       W2*R.0
       W3 GETBF IND(B.PAR+4)(R4),BY2,R.3
       W3+IND(B.PAR)
       W1 CLR
       W3 COMP R.8
       IF<=GO BACK
       W SUB3 R3,R.8,R1
       W3:=R.8
BACK:  W3 PUTBF IND(B.PAR+4)(R4),BY2,R.3
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_INCBYT
#undef CERNLIB_TCGEN_INCBYT
#endif
