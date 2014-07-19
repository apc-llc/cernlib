*
* $Id: linkg900.inc,v 1.1.1.1 1996/02/15 17:48:31 mclareni Exp $
*
* $Log: linkg900.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:31  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_LINKG900_INC
#define CERNLIB_KERNNUM_LINKG900_INC
*
*
* linkg900.inc
*
          MACRO
          PROLOG    &REGLIM
          GBLC      &NAME               E.G. 'RANF  '
          USING     *,15
          B         A&SYSNDX
          DC        XL1'7',CL7'&NAME'
A&SYSNDX  DS        0H
          STM       14,&REGLIM,12(13)
          MEND
          MACRO
&LABEL    EXIT      &REGLIM
&LABEL    LM        14,&REGLIM,12(13)   RESTORE REGISTERS
          MVI       12(13),255
          BR        14
          MEND

#endif
