*
* $Id: ffparm.inc,v 1.1.1.1 1996/03/08 11:50:40 mclareni Exp $
*
* $Log: ffparm.inc,v $
* Revision 1.1.1.1  1996/03/08 11:50:40  mclareni
* Ffread
*
*
#ifndef CERNLIB_FFREAD_FFPARM_INC
#define CERNLIB_FFREAD_FFPARM_INC
*
*
* ffparm.inc
*
      PARAMETER (NCHKYD =   4,
     *           NCHMAX =  32,
     *           LENMIN =  72,
     *           LENDEF =  80,
     *           LENMAX = 132,
     *           LSTACK =  20,
     *           NCSIZE = 500,
     *           NKYOVH =   3,
     *           NOVERH =  23 + LSTACK)
C
C     NCHKYD - default key size in characters
C     NCHMAX - maximum key size in characters
C     LENMIN - minimum significant length of data card
C     LENDEF - default significant length of data card
C     LENMAX - maximum significant length of data card
C     LSTACK - size of input LUN stack
C     NCSIZE - default size of IWORDS
C     NKYOVH - overhead per key in words (in addition to NWDKEY)
C     NOVERH - words in common CFREAD before IWORDS
C
C     NB: If NCHMAX or LSTACK are changed, also modify the length of
C         the variable BLANK in FFGO to be MAX(LSTACK+2, NCHMAX-6).
C

#endif
