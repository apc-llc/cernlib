C***********************************************************************
C $Id: arprda.F,v 1.2 1996/04/10 12:33:30 mclareni Exp $

      SUBROUTINE ARPRDA

C...ARiadne subroutine PRint DAta

C...Prints out parameters and switches used in Ariadne.

#include "arimpl.f"
#include "ardat1.f"


      WRITE(MSTA(7),*)
      WRITE(MSTA(7),1000)
      DO 100 I=1,20
        WRITE(MSTA(7),1010) I,MSTA(I),MSTA(I+20),PARA(I),PARA(I+20)
 100  CONTINUE
      WRITE(MSTA(7),*)

 1000 FORMAT(10X,'Parameters and switches used by Ariadne:',/,/,
     $     '         I   MSTA(I) MSTA(I+20)   PARA(I) PARA(I+20)',/)
 1010 FORMAT(2I10,I11,3X,2G11.3)

      RETURN

C**** END OF ARPRDA ****************************************************
      END
