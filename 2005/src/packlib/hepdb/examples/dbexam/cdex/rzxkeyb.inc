*
* $Id: rzxkeyb.inc,v 1.1.1.1 1996/02/28 16:23:40 mclareni Exp $
*
* $Log: rzxkeyb.inc,v $
* Revision 1.1.1.1  1996/02/28 16:23:40  mclareni
* Hepdb, cdlib, etc
*
*
*
* rzxkeyb.inc
*
*
* - - - - - -/RZXKEYB/- - - - - - - - - - - - - - - - - -
      INTEGER     MAXKEYB
      PARAMETER ( MAXKEYD=20 , MAXKEYB=200 )
      COMMON /RZXKEYB/ NRZKEYS, NRZKEYB, IRZKEYB(MAXKEYD,MAXKEYB)
      INTEGER          NRZKEYD, NRZKEYB, IRZKEYB
* - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
