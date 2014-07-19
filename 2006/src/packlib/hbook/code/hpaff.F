*
* $Id: hpaff.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hpaff.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HPAFF(CH,NL,CHPATH)
*.==========>
*.       HPAFF reconstructs into the character variable CHPATH the pathname
*.       stored into the character array CH.
*.           where NL is the number of levels starting from the root directory
*.
*. Input:
*.    CH  Character array of NL elements
*. Output:
*.   *CHPATH* Character variable  specifying the pathname
*..=========> ( R.Brun )
#include "hbook/hcmail.inc"
      CHARACTER*(*) CHPATH,CH(*)
      CHARACTER*16 CHL
*.___________________________________________
      MAXLEN=LEN(CHPATH)
      IF(MAXLEN.GT.110)MAXLEN=110
*
      CHPATH='//'//CH(1)
      LENG=LENOCC(CHPATH)
      IF(LENG.EQ.2) THEN
         CHPATH='//HOME'
         LENG=6
      ENDIF
      IF(NL.EQ.1) GOTO 99
*
      DO 20 I=2,NL
         CHL=CH(I)
         NMAX=LENOCC(CHL)
         IF(NMAX.EQ.0) GOTO 99
         IF(LENG+NMAX.GT.MAXLEN)NMAX=MAXLEN-LENG
         CHMAIL=CHPATH(1:LENG)//'/'//CHL(1:NMAX)
         CHPATH=CHMAIL
         LENG=LENG+NMAX+1
         IF(LENG.EQ.MAXLEN)GO TO 99
   20 CONTINUE
*
   99 RETURN
      END
