*
* $Id: rzmode.inc,v 1.1.1.1 1996/03/06 10:47:19 mclareni Exp $
*
* $Log: rzmode.inc,v $
* Revision 1.1.1.1  1996/03/06 10:47:19  mclareni
* Zebra
*
*
*
* rzmode.inc
*
#if defined(CERNLIB_RTOX)||defined(CERNLIB_RFRX)
      CHARACTER*11 FORM
      CHARACTER*1  MODE
      DATA FORM/'UNFORMATTED'/,MODE/'X'/
#endif
#if defined(CERNLIB_RTOA)||defined(CERNLIB_RFRA)
      CHARACTER*11 FORM
      CHARACTER*1  MODE
      DATA FORM/'FORMATTED'  /,MODE/'A'/
#endif
