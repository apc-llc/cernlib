*
* $Id: hcform.inc,v 1.2 2001/10/10 08:07:28 couet Exp $
*
* $Log: hcform.inc,v $
* Revision 1.2  2001/10/10 08:07:28  couet
* - In this common block IJUNK has been replaced by IOCC (IJUNK was not used
*   in HBOOK). IOCC is returned by  MZFORM and used later to create the HLABEL
*   data bank.
*
* Revision 1.1.1.1  1996/01/16 17:07:51  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCFORM_INC
#define CERNLIB_HBOOK_HCFORM_INC
*
*
* hcform.inc
*
*CMZ :  4.19/00 13/04/93  16.36.40  by  Rene Brun
*-- Author :
      COMMON/HCFORM/IODIR,IOH1,IOH2,IOHN,IOCF1,IOCF2,IOCB1,IOCB2,
     +              IOCF4,IOFIT,IONT,IOBL,IOCC
*

#endif
