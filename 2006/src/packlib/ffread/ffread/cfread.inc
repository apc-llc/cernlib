*
* $Id: cfread.inc,v 1.1.1.1 1996/03/08 11:50:41 mclareni Exp $
*
* $Log: cfread.inc,v $
* Revision 1.1.1.1  1996/03/08 11:50:41  mclareni
* Ffread
*
*
#ifndef CERNLIB_FFREAD_CFREAD_INC
#define CERNLIB_FFREAD_CFREAD_INC
*
*
* cfread.inc
*
#include "ffread/ffparm.inc"
      COMMON /CFREAD/  NWORDS, NCHKEY, NWDKEY, NKEYS,  MAXKEY,
     *                 NOFLEN, NOFTYP, NOFKEY,
     *                 LINPUT, LUNIN,  LUNOUT,
     *                 ISTACK, LUNS(LSTACK),
     *                 IFFOPT(8), KEYOPT(3),
     *                 IWORDS(NCSIZE)
#if defined(CERNLIB_SAVE)
#include "ffread/cfreadsv.inc"
#endif
      PARAMETER (NOFLOC = 0)
C
C     Common CFREAD contains all information FFREAD knows about.
C     The variables contain the following:
C
C     NWORDS - # of words available in array IWORDS for key storage
C
C     NCHKEY - # of characters per key - default is four
C     NWDKEY - # of words per key
C     NKEYS  - # of keys currently defined
C     MAXKEY - maximum # of keys IWORDS will hold, considering NCHKEY and NWORDS
C
C    (NOFLOC)- offset for storage of addresses in IWORDS
C              (This is declared a parameter, as it is implicitly 0.)
C     NOFLEN - ditto for array lengths
C     NOFTYP - ditto for type field
C     NOFKEY - ditto for keys
C
C     LINPUT - # of significant characters on data card
C     LUNIN  - unit number for reading
C     LUNOUT - unit number for output
C
C     IFFOPT - character options for FFGET/FFSET
C     KEYOPT - character options for FFKEY
C
C     ISTACK - stack pointer into LUNS()
C     LUNS() - stack for reading from files
C
C     IWORDS - storage for keys, value locations, their length, and their type.
C

#endif
