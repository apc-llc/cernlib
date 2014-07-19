*
* $Id: dzedit.F,v 1.2 1997/03/14 14:20:36 mclareni Exp $
*
* $Log: dzedit.F,v $
* Revision 1.2  1997/03/14 14:20:36  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:25:01  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/03/04 16:13:20  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      PROGRAM DZEDMA
#include "dzeditkeep.inc"
      LOGICAL INTRAC
      CHARACTER*128 CSCRAT
*      INTEGER  MZLIST(4)
*      DATA MZLIST/3,0,3,3/
*      EXTERNAL IGTERM
#if defined(CERNLIB_QMIBM)
      CALL INITC()
#endif
      CALL DZDPRV
      CALL DZDIBR
C--     Declare the primary store
 
C--     Declare the primary store
C--      OPEN(3,FILE='zebra.log',STATUS='UNKNOWN')
C--      WRITE(3,*)'***** Dzedit:  Zebra log  *****'
C--      CALL DATIME(IDATE0,ITI)
C--      WRITE(3,*)'It is now: ',IDATE0,ITI
C--      CALL MZEBRA(MZLIST)
 
       CALL MZEBRA(-3)
      CALL MZSTOR (IXDZDS,'/MYCOMM/',' ',IDFENC,LQ,LQ,
     +             LQ,LQ(2000),LQ(NWDOCQ))
 
*      CALL MZLOGL(IXDZDS,2)
      CALL MZLINK(0,'DZDEDL',LTOP,LUP,LSUPL)
      CALL MZPAW(NWPAW,' ')
      CALL KUINIT(NWKUIP)
*      CALL KUTERM(IGTERM)
      CALL DZEDEF
      CALL DZEZER
      IQUITF=0
      CALL KUEXEC('SET/PROMPT ''DZEDIT>''')
      CALL KUEXEC('EXEC dzedit.kumac')
 
      IF(IQUITF.GT.0)GOTO 900
 
      LUNLAS=LUPLAS
      CALL KUOPEN(LUNLAS,'dzedit.las','OLD', ISTAT)
      IF(ISTAT .EQ. 0)THEN
         CALL DZDCRD(LUNLAS,CHRZF,IRC)
         IF(IRC.NE.0)GOTO 20
         CALL DZDCRD(LUNLAS,CHCARD,IRC)
         IF(IRC.NE.0)GOTO 20
         CALL DZDCRD(LUNLAS,CHFZF,IRC)
         IF(IRC.NE.0)GOTO 20
         CALL DZDCRD(LUNLAS,CTITLE,IRC)
         IF(IRC.NE.0)GOTO 20
         CALL DZDCRD(LUNLAS,CHFZFI,IRC)
         IF(IRC.NE.0)GOTO 20
 
*         READ(LUNLAS,'(A)',END=20)CHRZF
*         READ(LUNLAS,'(A)',END=20)CHCARD
*         READ(LUNLAS,'(A)',END=20)CHFZF
*         READ(LUNLAS,'(A)',END=20)CTITLE
*         READ(LUNLAS,'(A)',END=20)CHFZFI
      ENDIF
   20 CONTINUE
      CLOSE(LUNLAS)
      LUNLAS=0
      LUNGKS=LUPGKS
      CALL KUOPEN(LUNGKS,'grapherr.lis','UNKNOWN',ISTAT)
      CALL IGINIT(NWHIGZ)
      IF(INTRAC( ))THEN
         CALL IGWKTY(IWKTYP)
      ELSE
         IWKTYP=0
      ENDIF
      IERFIL = 6
      CALL IGSSE(LUNGKS,IWKTYP)
      IF(IWKTYP .NE. 0)THEN
         CALL ISTXFP(1,0)
         IOPWKF=1
*       CALL IDAWK(1)
      ELSE
         IWKID=0
         CALL IGRNG(20.,20.)
      ENDIF
      IF(IWKID.NE.0)THEN
         LUNPLO=LUPPLO
         CALL KUOPEN(LUNPLO,'dzedit.ps','UNKNOWN',ISTAT)
         ITYPLO=-111
         CALL IOPWK(IWMETA,LUNPLO,ITYPLO)
         CALL IGRNG(20.,20.)
      ELSE
         LUNPLO=0
      ENDIF
      CALL DZEDFF
      CALL IGSA(1)
      CALL KUWHAG
 
900   CONTINUE
      IF(LUNMET .NE. 0)CALL ICLWK(INTWST)
      IF(LUNPLO .NE. 0)CALL ICLWK(IWMETA)
      IF(LUNPLO.NE.0)THEN
         CALL KICLOS(LUNPLO,' ',ISTAT)
         CALL KUOPEN(LUNPLO,'dzedit.ps','UNKNOWN',ISTAT)
         DO 910 I=1,1000
            READ(LUNPLO,'(A)',END=920)CSCRAT
            IF(INDEX(CSCRAT,' Zone').NE.0)THEN
               CALL KICLOS(LUNPLO,' ',ISTAT)
               GOTO 930
            ENDIF
910      CONTINUE
920      CALL KICLOS(LUNPLO,'DELETE',ISTAT)
930      CONTINUE
      ENDIF
      IF(LUNGKS.NE.0)THEN
         CALL KUOPEN(LUNGKS,'grapherr.lis','UNKNOWN',ISTAT)
         CALL KICLOS(LUNGKS,'DELETE',ISTAT)
      ENDIF
      IF(LUNSGM .GT. 0)CALL DZEWEM(LUNSGM)
      IF(ISGMOP .NE. 0)THEN
         IF(LUNLST .GT. 0)CALL DZEWEM(LUNLST)
      ENDIF
      IF(IPOSTF.NE.0)THEN
         IF(ILSTOP.EQ.0)THEN
            LUNLIS=LUPLIS
            CALL KUOPEN(LUNLIS,CHLIST,'UNKNOWN',ISTAT)
         ELSE
            REWIND(LUNLIS)
         ENDIF
         REWIND(LUNSC1)
         CALL DZESGP(LUNSC1,0,LUNLIS,0)
      ENDIF
      LUNLAS=LUPLAS
      CALL KUOPEN(LUNLAS,'dzedit.las','UNKNOWN',ISTAT)
      WRITE(LUNLAS,'(A)')CHRZF
      WRITE(LUNLAS,'(A)')CHCARD
      WRITE(LUNLAS,'(A)')CHFZF
      WRITE(LUNLAS,'(A)')CTITLE
      WRITE(LUNLAS,'(A)')CHFZFI
      CALL RZCDIR('//RZDOC',' ')
      IF(IRWRZF.GT.1)CALL RZFREE('rzfile')
      CALL RZCLOS(' ','A')
      STOP 'DZEDIT: Normal exit'
      END
*********************************************************************
