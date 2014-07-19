*
* $Id: ch3dat.inc,v 1.1.1.1 1996/02/15 17:48:43 mclareni Exp $
*
* $Log: ch3dat.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:43  mclareni
* Kernlib
*
*
*
* ch3dat.inc
*
          LOGICAL             OKFLAG,   CCFLAG
          COMMON    /CH3NUM/  OKFLAG,   CCFLAG
          COMMON    /CH3NUM/  TRELPR
          COMMON    /CH3NUM/  MINHEX,   MAXHEX,   MINHXT(3),MAXHXT(3)
          COMMON    /CH3NUM/  LTAB,     LTABT(3)
          COMMON    /CH3NUM/  KTAB(34), MTAB(34), NTAB(34)
          COMMON    /CH3NUM/  VOID(2),  NCNFG,    LENGTH,   IDIM
          COMMON    /CH3NUM/  L2,       L12,      L21,      L22
          CHARACTER*4         HTYPE
          COMMON    /CH3CHR/  HTYPE
