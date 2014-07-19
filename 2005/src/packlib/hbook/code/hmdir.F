*
* $Id: hmdir.F,v 1.1.1.1 1996/01/16 17:07:43 mclareni Exp $
*
* $Log: hmdir.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:43  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          16/05/95  18.30.41  by  Julian Bunn
*-- Author :
      SUBROUTINE HMDIR(CHDIR,CHOPT)
*.==========>
*.            Create a sub-directory of the current directory
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcform.inc"
#include "hbook/hcdire.inc"
      CHARACTER*(*) CHDIR,CHOPT
      CHARACTER*64 CHWOLD
      CHARACTER*16 CHD
      CHARACTER*16 CTHIS
      CHARACTER*8  TAGS(2)
*.___________________________________________
*
*       Check if directory to be created is in the CWD
*       If not, save CWD and go to the parent directory
*
      ICWD=0
      NCH=LENOCC(CHDIR)
      IF(NCH.GT.2)THEN
         IF(CHDIR(1:2).EQ.'//')THEN
            CALL HCDIR(CHWOLD,'R')
            DO 5 I=NCH,3,-1
               IF(CHDIR(I:I).EQ.'/')THEN
                  CHD=CHDIR(I+1:)
                  CALL HCDIR(CHDIR(1:I-1),' ')
                  ICWD=1
                  GO TO 7
               ENDIF
   5        CONTINUE
         ENDIF
      ENDIF
*
      IF(NCH.LE.16)THEN
         CHD=CHDIR
      ELSE
         CHD=CHDIR(1:16)
      ENDIF
*           Current directory refers to a RZ file.
*
   7  IF(ICHTOP(ICDIR).GT.0)THEN
         TAGS(1) = 'HBOOK-ID'
         TAGS(2) = 'VARIABLE'
         NWK=ICHTYP(ICDIR)
         CALL RZMDIR(CHD,NWK,'II',TAGS)
         GO TO 90
      ENDIF
*
*           Cannot create directory in a GLOBAL section
*
      IF(ICHTOP(ICDIR).LT.0)THEN
         CALL HBUG('Cannot create a directory in a global section',
     +              'HMDIR',0)
         GO TO 99
      ENDIF
*
*           Directory is in //PAWC
*
*           Search last Sub-directory or create first one
*
      IF(LQ(LCDIR-1).EQ.0)THEN
         CALL MZBOOK(IHDIV,LR1,LCDIR,-1,'HDIR',10,8,10,IODIR,0)
      ELSE
         LF=LQ(LCDIR-1)
C
C Check whether this existing directory matches the user's
C and do not create a new bank if so
C
  10     CALL UHTOC(IQ(LF+1),4,CTHIS,16)
         IF(CTHIS.EQ.CHD(:16)) THEN
            CALL HBUG('+Directory '//CTHIS//' already exists',
     &               'HMDIR',0)
            GOTO 90
         ENDIF
C
C No match, get the next directory bank in the list
C
         LR2=LQ(LF)
         IF(LR2.NE.0)THEN
            LF=LR2
            GO TO 10
         ENDIF
C
C We are at the end of the bank list, so we can put the new dir here
C
         LR2=LF
         CALL MZBOOK(IHDIV,LR1,LR2, 0,'HDIR',10,8,10,IODIR,0)
      ENDIF
      CALL VBLANK(IQ(LR1+1),4)
      IF(NCH.GT.16)NCH=16
      CALL UCTOH(CHD,IQ(LR1+1),4,NCH)
      CALL MZBOOK(IHDIV,LR2 ,LR1,-3,'HTAB',50,0,50,2,0)
*
C
C If Option 'S' is given, the set the current directory one down
C
  90  IF(CHOPT.EQ.'S')THEN
         CALL HCDIR(CHD,' ')
      ELSE
         IF(ICWD.NE.0)CALL HCDIR(CHWOLD,' ')
      ENDIF
*
  99  RETURN
      END
