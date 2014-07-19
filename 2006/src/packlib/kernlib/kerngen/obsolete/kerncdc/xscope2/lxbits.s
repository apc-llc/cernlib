*
* $Id: lxbits.s,v 1.1.1.1 1996/02/15 17:51:17 mclareni Exp $
*
* $Log: lxbits.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:17  mclareni
* Kernlib
*
*
          IDENT       LXBITS
*
* CERN PROGLIB# Z200    LXBITS          .VERSION KERNCDC  1.16  820129
*
          ENTRY       LXBITS
*
*FUNCTION  LXBITS(IUNIT)
*RETURNS IN X6 THE LENGTH IN BITS OF RECORD
*
          VFD         42/7HLXBITS ,18/LXBITS
 STOREA0  BSS         1
 LXBITS   DATA        0
          SX6         A0               SAVE A0
          SA6         STOREA0
          SA0         A1               IUNIT IN X1
          SA1         A0
          MX6   1
          BX1   X1+X6
          RJ          =XGETFIT.        GET ADDRESS OF FIT IN X1
          FETCH  X1,PTLW,X6
          SX5    X6          X5=NW
          AX6    18          X6=UBC
          SB1         2
          LX7         X5,B1  4*NW
          SB1    B1+B1
          LX5    X7,B1       64*NW
          IX4         X5-X7  60*NW
          IX6    X4-X6       60*
          SA5         STOREA0          RESTORE A0
          SA0         X5
          EQ          LXBITS
          END
#ifdef CERNLIB_XSCOPE_LXBITS
#undef CERNLIB_XSCOPE_LXBITS
#endif
