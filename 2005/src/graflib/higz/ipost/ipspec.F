*
* $Id: ipspec.F,v 1.1.1.1 1996/02/14 13:11:07 mclareni Exp $
*
* $Log: ipspec.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:07  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.16/01 13/08/92  18.40.25  by  O.Couet
*-- Author :    P.Juillot   13/08/92
      SUBROUTINE IPSPEC
*.===========>
*.
*. Reencode the Greek (/Symbol) font into the special font (/Special).
*.
*..==========> (O.Couet)
      CALL IPPSTR('@/accspe [')
      CALL IPPSTR(' 65 /plusminus ')
      CALL IPPSTR(' 66 /bar ')
      CALL IPPSTR(' 67 /existential ')
      CALL IPPSTR(' 68 /universal ')
      CALL IPPSTR('@69 /exclam ')
      CALL IPPSTR(' 70 /numbersign')
      CALL IPPSTR(' 71 /greater ')
      CALL IPPSTR(' 72 /question ')
      CALL IPPSTR(' 73 /integral ')
      CALL IPPSTR('@74 /colon ')
      CALL IPPSTR(' 75 /semicolon ')
      CALL IPPSTR(' 76 /less ')
      CALL IPPSTR(' 77 /bracketleft ')
      CALL IPPSTR(' 78 /bracketright')
      CALL IPPSTR('@79 /greaterequal')
      CALL IPPSTR(' 80 /braceleft')
      CALL IPPSTR(' 81 /braceright')
      CALL IPPSTR(' 82 /radical')
      CALL IPPSTR('@83 /spade')
      CALL IPPSTR(' 84 /heart')
      CALL IPPSTR(' 85 /diamond')
      CALL IPPSTR(' 86 /club')
      CALL IPPSTR(' 87 /lessequal')
      CALL IPPSTR('@88 /multiply')
      CALL IPPSTR(' 89 /percent')
      CALL IPPSTR(' 90 /infinity')
      CALL IPPSTR(' 48 /circlemultiply')
      CALL IPPSTR(' 49 /circleplus')
      CALL IPPSTR('@50 /emptyset ')
      CALL IPPSTR(' 51 /lozenge')
      CALL IPPSTR(' 52 /bullet')
      CALL IPPSTR(' 53 /arrowright')
      CALL IPPSTR(' 54 /arrowup')
      CALL IPPSTR('@55 /arrowleft')
      CALL IPPSTR(' 56 /arrowdown')
      CALL IPPSTR(' 57 /arrowboth')
      CALL IPPSTR(' 48 /degree')
      CALL IPPSTR(' 44 /comma')
      CALL IPPSTR(' 43 /plus')
      CALL IPPSTR(' 45 /angle')
      CALL IPPSTR(' 42 /angleleft')
      CALL IPPSTR(' 47 /divide')
      CALL IPPSTR(' 61 /notequal')
      CALL IPPSTR(' 40 /equivalence')
      CALL IPPSTR(' 41 /second')
*
      CALL IPPSTR(' 97 /approxequal')
      CALL IPPSTR(' 98 /congruent')
      CALL IPPSTR(' 99 /perpendicular')
      CALL IPPSTR(' 100 /partialdiff')
      CALL IPPSTR(' 101 /florin')
      CALL IPPSTR(' 102 /intersection')
      CALL IPPSTR(' 103 /union')
      CALL IPPSTR(' 104 /propersuperset')
      CALL IPPSTR(' 105 /reflexsuperset')
      CALL IPPSTR(' 106 /notsubset')
      CALL IPPSTR(' 107 /propersubset')
      CALL IPPSTR(' 108 /reflexsubset')
      CALL IPPSTR(' 109 /element')
      CALL IPPSTR(' 110 /notelement')
      CALL IPPSTR(' 111 /gradient')
      CALL IPPSTR(' 112 /logicaland')
      CALL IPPSTR(' 113 /logicalor')
      CALL IPPSTR(' 114 /arrowdblboth')
      CALL IPPSTR(' 115 /arrowdblleft')
      CALL IPPSTR(' 116 /arrowdblup')
      CALL IPPSTR(' 117 /arrowdblright')
      CALL IPPSTR(' 118 /arrowdbldown')
      CALL IPPSTR(' 119 /ampersand')
      CALL IPPSTR(' 120 /omega1')
      CALL IPPSTR(' 121 /similar')
      CALL IPPSTR(' 122 /aleph')
      CALL IPPSTR(' ] def')
      CALL IPPSTR('/Symbol /Special accspe ReEncode@')
*
      END
