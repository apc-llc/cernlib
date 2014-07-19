*
* $Id: uglast.F,v 1.2 1996/02/22 13:16:34 ravndal Exp $
*
* $Log: uglast.F,v $
* Revision 1.2  1996/02/22 13:16:34  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:19  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGLAST
C.    *
C.    *
C.    *      Termination routine to print histograms and statistics
C.    *
C.    *
#include "geant321/gcbank.inc"
#include "geant321/gcmate.inc"
#include "geant321/gcunit.inc"
#include "shobuf.inc"
      DIMENSION TITLE(20)
C
      CALL GLAST
C
C *** Print geometry
C
      JMA = LQ(JMATE-NMAT)
      CALL UBLOW(IQ(JMA+1),TITLE,20)
      WRITE(LOUT,501) NMAT,TITLE ,RADL
      XSIZE = NXBIN * XBIN
      YSIZE = NYBIN * YBIN
      ZSIZE = NZBIN * ZBIN
      WRITE(LOUT,502) XSIZE,YSIZE,ZSIZE
      XRDL  = XBIN/RADL
      YRDL  = YBIN/RADL
      ZRDL  = ZBIN/RADL
      WRITE(LOUT,503) NXBIN,XRDL,XBIN,NYBIN,YRDL,YBIN,NZBIN,ZRDL,ZBIN
C
C *** Print statistic on records written
C
      WRITE(LOUT,1000)
      NTRCOR = NENERG+1
      DO 9 IE=1,NENERG
        NFIRE(IE)=NFIRE(IE)/NSHOW(IE)
        TKBYT    =NRCOR(IE)*NBYTE*1.E-03
        NTRCOR   =NTRCOR+NRCOR(IE)
        WRITE(LOUT,1001) IE,ESHOW(IE)*1000.,NSHOW(IE),NFIRE(IE),
     +                      NRCOR(IE),TKBYT
    9 CONTINUE
      TKBYT       =NTRCOR*NBYTE*1.E-03
      WRITE(LOUT,1002) NTRCOR,TKBYT
C
C
      CALL HIDOPT(0,'BLAC')
      CALL HISTDO
C
  501 FORMAT(1H1,/,21X,'ABSORBER :',I3,1X,20A1,'  RADL =',F8.2,' CM')
  502 FORMAT(25X,'SIZE :  LX=',F8.2,' CM  LY=',F8.2,' CM  LZ=',
     *                             F8.2,' CM')
  503 FORMAT(25X,'GRID :  X:',I3,' BINS OF',F6.2,' RADL =',F8.2,' CM',
     *       /,33X,      'Y:',I3,' BINS OF',F6.2,' RADL =',F8.2,' CM',
     *       /,33X,      'Z:',I3,' BINS OF',F6.2,' RADL =',F8.2,' CM')
C
 1000 FORMAT(/,15X,'  ENERGY (MEV) ',5X,'# SHOWERS ',5X,
     *             '# CELLS/SHOWER ',5X,'# RECORDS ',5X,
     *             ' # KBYTES ',/,15X,80('*'),/)
 1001 FORMAT(10X,I5,F11.0,' MEV',5X,I5,15X,I6,10X,I6,8X,F8.3)
 1002 FORMAT(70X,25('-'),/,45X,'TOTAL (WITH DIRECTORIES) :',I6,8X,F8.3)
      END
