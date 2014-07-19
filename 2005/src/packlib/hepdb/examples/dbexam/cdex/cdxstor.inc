*
* $Id: cdxstor.inc,v 1.1.1.1 1996/02/28 16:23:38 mclareni Exp $
*
* $Log: cdxstor.inc,v $
* Revision 1.1.1.1  1996/02/28 16:23:38  mclareni
* Hepdb, cdlib, etc
*
*
*
* cdxstor.inc
*
cc\\* - - - - - -/PAWC/ - - - - - - - - - - - - - - - - - - -
cc\\      INTEGER NWPAWC , IPAWC
cc\\      PARAMETER (NWPAWC = 100000)
cc\\      COMMON /PAWC/ IPAWC(NWPAWC)
cc\\* - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cc\\*
cc\\+KEEP,CDXLQIQ.
cc\\* - - - - - - - LQIQ- - - - - - - - - - - - - - - - - - -
cc\\      INTEGER   LQ(8000) , IQ(2)
cc\\      REAL                 Q(2)
cc\\      EQUIVALENCE (LQ(1),IPAWC(11)),(IQ(1),LQ(9)),(Q(1),IQ(1))
cc\\* - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* - - - - - - - PAWLQIQ- - - - - - - - - - - - - - - - - - -
      COMMON /PAWC/ IPAWC(99)
      INTEGER       IPAWC
      INTEGER   LQ(88) , IQ(2)
      REAL                Q(2)
      EQUIVALENCE (LQ(1),IPAWC(11)),(IQ(1),LQ(9)),(Q(1),IQ(1))
* - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
cc\\+KEEP,CDXULNK.
cc\\* - - - - - -/CDULNK/- - - - - - - - - - - - - - - - - -
cc\\      COMMON /CDULNK/ IUSDIV , LUSDIV
cc\\      INTEGER         IUSDIV , LUSDIV
cc\\* - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
