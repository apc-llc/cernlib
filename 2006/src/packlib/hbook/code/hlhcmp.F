*
* $Id: hlhcmp.F,v 1.2 1998/09/25 09:26:58 mclareni Exp $
*
* $Log: hlhcmp.F,v $
* Revision 1.2  1998/09/25 09:26:58  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author : Aubert Pierre
      INTEGER FUNCTION HLHCMP(H1,H2,LGR)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C HLHCMP compare two hollerith chacacters H1 and H2 of length LGR
C 4 by 4 blocks
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C H1,H2 two array of integer ( length LGR/4 )
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER H1(*),H2(*),LGR
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||defined(CERNLIB_MSDOS)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))||defined(CERNLIB_WINNT)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C WITH BYTE SWAPING
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CHARACTER*64 CH1,CH2
C
      CALL UHTOC(H1,4,CH1,LGR)
      CALL UHTOC(H2,4,CH2,LGR)
C
      IF(CH1(1:LGR).LT.CH2(1:LGR))THEN
         HLHCMP = -1
      ELSE IF(CH1(1:LGR).EQ.CH2(1:LGR))THEN
         HLHCMP = 0
      ELSE
         HLHCMP = 1
      ENDIF
      RETURN
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_DECS))&&(!defined(CERNLIB_MSDOS))&&(!(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)))&&(!defined(CERNLIB_WINNT))
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C WITHOUT BYTE SWAPING
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER NB,NBTOT,P1,P2
C
      IF(MOD(LGR,4).NE.0) LGR = LGR - MOD(LGR,4)
C
      NBTOT = LGR / 4
C
      NB = 1
   10 P1 = H1(NB)
      P2 = H2(NB)
      IF( P1 .LT. P2 )THEN
         HLHCMP = -1
         RETURN
      ELSE IF( P1 .GT. P2 )THEN
         HLHCMP = +1
         RETURN
      ELSE
         IF( NB .EQ. NBTOT )THEN
            HLHCMP =  0
            RETURN
         ELSE
            NB = NB + 1
            GO TO 10
         ENDIF
      ENDIF
C
#endif
      END
