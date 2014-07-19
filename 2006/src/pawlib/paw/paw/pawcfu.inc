*
* $Id: pawcfu.inc,v 1.2 1996/03/01 16:49:55 cernlib Exp $
*
* $Log: pawcfu.inc,v $
* Revision 1.2  1996/03/01 16:49:55  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:49  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PAWCFU_INC
#define CERNLIB_PAW_PAWCFU_INC
*
*
* pawcfu.inc
*
*CMZ :  2.07/00 04/05/95  17.01.51  by  Julian Bunn
*-- Author :
      PARAMETER (MAXCLL=512,MAXLAB=1000,MAXFCU=2000,MAXSTK=13)
      PARAMETER (MXVAR=100, MAXFCC=44)
*
      INTEGER JADF
      COMMON/PAWCF2/JADF, IDCHAR(3), NVART, NVART2, NVART3, IDFNEW,
     $     LABELX, LABELY, LABELZ, ICTYPE,
     $     INDX(MAXCLL), INDX2(MAXCLL), ITYPE(MAXCLL), ISIZE(MAXCLL),
     $     IELEM(MAXCLL), IOFFST(MAXCLL), IOFFST2(MAXCLL)
*
      CHARACTER*32
     $     CSTACK(100)   , CHLABX(MAXLAB), CHLABY(MAXLAB),
     $     CHLABZ(MAXLAB), CHFCU         , VAR(MAXCLL),
     $     HFCU(100)
      COMMON/PAWCF3/VAR,CSTACK,CHLABX,CHLABY,CHLABZ,CHFCU,HFCU
*
* we allow up to 4 array expressions in a selection (bbbwaaaaa..)
      REAL          RFCU(MAXFCU*4)
      REAL*8        DFCU
      INTEGER       IFCU
      LOGICAL       LFCU
      COMMON/PAWCF4/DFCU,IFCU,LFCU,RFCU
*
      INTEGER      CFUINFO(0:MXVAR,6),CFUPOS
* we allow MAXSTK arrays of maximum length MAXFCU
      REAL         RSTACK(0:MAXSTK,0:MAXFCU)
* and MAXSTK character arrays of maximum length MAXFCC
      CHARACTER*32 CHPILE(0:MAXSTK,0:MAXFCC)
      LOGICAL      NOSEL
      COMMON/PAWCF5/RSTACK,CFUINFO,CFUPOS,NOSEL
      COMMON/PAWCF6/CHPILE
*

#endif
