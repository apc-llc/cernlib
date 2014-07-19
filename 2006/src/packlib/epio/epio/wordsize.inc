*
* $Id: wordsize.inc,v 1.1.1.1 1996/03/08 15:21:47 mclareni Exp $
*
* $Log: wordsize.inc,v $
* Revision 1.1.1.1  1996/03/08 15:21:47  mclareni
* Epio
*
*
#ifndef CERNLIB_EPIO_WORDSIZE_INC
#define CERNLIB_EPIO_WORDSIZE_INC
*
*
* wordsize.inc
*
C    number of bits/word
      INTEGER NBITPW, NCHAPW, NBITPC, N16PW
#if defined(CERNLIB_CDC)
      PARAMETER   (NBITPW=60, NCHAPW=10, NBITPC=6)
#elif defined(CERNLIB_CRAY)
      PARAMETER   (NBITPW=64, NCHAPW= 8, NBITPC=8, N16PW=4)
#elif (defined(CERNLIB_CONVEX))&&(defined(CERNLIB_SINGLE))
      PARAMETER   (NBITPW=64, NCHAPW= 8, NBITPC=8, N16PW=4)
#elif 1
      PARAMETER   (NBITPW=32, NCHAPW=4, NBITPC=8, N16PW=2)
#endif

#endif
