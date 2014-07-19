*
* $Id: hcdire.inc,v 1.2 2003/02/07 12:13:37 couet Exp $
*
* $Log: hcdire.inc,v $
* Revision 1.2  2003/02/07 12:13:37  couet
* - The size of the HFNAME character string has been increased to 128. It
*   was 80 before. 80 was not long enough to support long file names one
*   can get using CASTOR files in a CHAIN command. The HFNAME length is now
*   defined as the parameter LENHFN.
*
* Revision 1.1.1.1  1996/01/16 17:07:51  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCDIRE_INC
#define CERNLIB_HBOOK_HCDIRE_INC
*
*
* hcdire.inc
*
*CMZ :  4.20/03 28/07/93  09.33.32  by  Rene Brun
*-- Author :
      PARAMETER (NLPATM=100, MXFILES=50, LENHFN=128)
      COMMON /HCDIRN/NLCDIR,NLNDIR,NLPAT,ICDIR,NCHTOP,ICHTOP(MXFILES)
     +              ,ICHTYP(MXFILES),ICHLUN(MXFILES)
      CHARACTER*16   CHNDIR,    CHCDIR,    CHPAT    ,CHTOP
      COMMON /HCDIRC/CHCDIR(NLPATM),CHNDIR(NLPATM),CHPAT(NLPATM)
     +              ,CHTOP(NLPATM)
      CHARACTER*(LENHFN) HFNAME
      COMMON /HCFILE/HFNAME(MXFILES)
*

#endif
