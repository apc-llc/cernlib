CDECK  ID>, BLANKDEK.   
*CMZ :  3.16/00 18/10/93  22.23.21  by  Federico Carminati
*-- Author :
CDECK  ID>, BLANKDEK.   
*CMZ :  3.16/00 18/10/93  22.23.21  by  Federico Carminati
*-- Author :
CDECK  ID>, BLANKDEK.   
*CMZ :  3.16/00 03/12/93  20.57.15  by  Federico Carminati
*-- Author :
CDECK  ID>, BLANKDEK.   
*CMZ :  3.16/00 19/11/92  15.01.21  by  Federico Carminati
*-- Author :
CDECK  ID>, GXINT.  
*CMZ :  3.16/00 06/12/93  11.22.04  by  Federico Carminati
*-- Author :    Rene Brun   03/01/89
      PROGRAM GXINT
*
*     GEANT main program. To link with the MOTIF user interface
*     the routine GPAWPP(NWGEAN,NWPAW) should be called, whereas
*     the routine GPAW(NWGEAN,NWPAW) gives access to the basic
*     graphics version.
*
      PARAMETER (NWGEAN=8000000,NWPAW=1000000)
      COMMON/GCBANK/GEANT(NWGEAN)
      COMMON/PAWC/PAW(NWPAW)
*
      CALL GPAW(NWGEAN,NWPAW)
*
      END
      SUBROUTINE QNEXT
      END
      SUBROUTINE CZOPEN
      END
      SUBROUTINE CZTCP
      END
      SUBROUTINE CZCLOS
      END
      SUBROUTINE CZPUTA
      END
