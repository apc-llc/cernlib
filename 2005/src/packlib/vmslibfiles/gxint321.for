CDECK  ID>, BLANKDEK.   
*CMZ :  3.21/02 29/03/94  15.41.18  by  S.Giani
*-- Author :
CDECK  ID>, BLANKDEK.   
*CMZ :  3.21/04 24/02/95  17.11.40  by  S.Giani
*-- Author :
CDECK  ID>, GEANFC. 
*CMZ :  3.21/04 29/03/94  15.41.50  by  S.Giani
*-- Author :
CDECK  ID>, GEANGC. 
*CMZ :  3.21/04 29/03/94  15.41.50  by  S.Giani
*-- Author :
CDECK  ID>, GEANHC. 
*CMZ :  3.21/04 29/03/94  15.41.50  by  S.Giani
*-- Author :
CDECK  ID>, GEANTC. 
*CMZ :  3.21/04 29/03/94  15.41.50  by  S.Giani
*-- Author :
CDECK  ID>, GEANMC. 
*CMZ :  3.21/04 29/03/94  15.41.50  by  S.Giani
*-- Author :
CDECK  ID>, BLANKDEK.   
*CMZ :  3.21/02 29/03/94  15.41.18  by  S.Giani
*-- Author :
CDECK  ID>, BLANKDEK.   
*CMZ :  3.21/02 29/03/94  15.41.18  by  S.Giani
*-- Author :
CDECK  ID>, BLANKDEK.   
*CMZ :  3.21/02 29/03/94  15.41.32  by  S.Giani
*-- Author :
CDECK  ID>, GXINT.  
*CMZ :  3.21/02 29/03/94  15.41.33  by  S.Giani
*-- Author :
      PROGRAM GXINT
*
*     GEANT main program. To link with the MOTIF user interface
*     the routine GPAWPP(NWGEAN,NWPAW) should be called, whereas
*     the routine GPAW(NWGEAN,NWPAW) gives access to the basic
*     graphics version.
*
      PARAMETER (NWGEAN=3000000,NWPAW=1000000)
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
