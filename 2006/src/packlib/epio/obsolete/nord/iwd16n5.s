*
* $Id: iwd16n5.s,v 1.1.1.1 1996/03/08 15:21:56 mclareni Exp $
*
* $Log: iwd16n5.s,v $
* Revision 1.1.1.1  1996/03/08 15:21:56  mclareni
* Epio
*
*
#if defined(CERNLIB_ND50)||defined(CERNLIB_ND500)
       MODULE M_IWD16
% I*4=IWD16(A,N)
% RETURNS THE PACKED 16 BIT WORD NUMBER N OF STRING A
% VERSION 811002
       EXPORT IWD16
       ROUTINE IWD16
       LIB IWD16
VBAS:  STACK FIXED
PAR:   W BLOCK 2
       ENDSTACK
IWD16:  ENTF VBAS
       W2:=IND(B.PAR+4)
       W2-1
       H1:=IND(B.PAR)(R2)
       RET
       ENDROUTINE
       ENDMODULE
#endif
