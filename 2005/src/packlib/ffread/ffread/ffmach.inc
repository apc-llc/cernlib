*
* $Id: ffmach.inc,v 1.1.1.1 1996/03/08 11:50:40 mclareni Exp $
*
* $Log: ffmach.inc,v $
* Revision 1.1.1.1  1996/03/08 11:50:40  mclareni
* Ffread
*
*
#ifndef CERNLIB_FFREAD_FFMACH_INC
#define CERNLIB_FFREAD_FFMACH_INC
*
*
* ffmach.inc
*
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NCHWD  =   4,
     *           NBITCH =   8,
     *           NBITWD =  32,
     *           IQANIL = -77)
#endif
#if (defined(CERNLIB_SINGLE))&&(!defined(CERNLIB_CDC))
      PARAMETER (NCHWD  =   8,
     *           NBITCH =   8,
     *           NBITWD =  64,
     *           IQANIL = -77)
#endif
#if defined(CERNLIB_CDC)
      PARAMETER (NCHWD  =  10,
     *           NBITCH =   6,
     *           NBITWD =  60,
     *           IQANIL = -77)
#endif
#if defined(CERNLIB_UNIVAC)
      PARAMETER (NCHWD  =   4,
     *           NBITCH =   9,
     *           NBITWD =  36,
     *           IQANIL = -77)
#endif
C
C     NCHWD  - # of characters per word
C     NBITCH - # of bits per character
C     NBITWD - # of bits per word
C     IQANIL - # weard address for the user routine
C

#endif
