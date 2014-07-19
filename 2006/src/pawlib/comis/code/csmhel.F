*
* $Id: csmhel.F,v 1.1.1.1 1996/02/26 17:16:20 mclareni Exp $
*
* $Log: csmhel.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:20  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.39  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSMHEL
***--------------------------------
*     types help about med commands
***--------------------------------
      CALL CSSOUT(' THE MED COMMANDS ARE: EDIT T S I D E Q H')
      CALL CSSOUT('EDIT   -               TO INVOKE SYSTEM EDITOR')
      CALL CSSOUT('[T] [<RANGE>]    -     TYPE LINES IN RANGE')
      CALL CSSOUT('S/OLD/NEW/ [<RANGE>] - SUBSTITUTIONS')
      CALL CSSOUT('I[N]/ LINE')
      CALL CSSOUT('      ....')
      CALL CSSOUT('      LINE/ -          INSERT AFTER LINE N')
      CALL CSSOUT('D[<RANGE>]        -    DELETE')
      CALL CSSOUT('E -           EXIT AND TRANSLATE; Q - EXIT')
      CALL CSSOUT('<RANGE> -->   N:N OR N OR +N OR -N')
      END
