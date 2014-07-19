*
* $Id: sifind.F,v 1.1.1.1 1995/12/12 14:36:17 mclareni Exp $
*
* $Log: sifind.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:17  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.08/00 15/12/92  18.08.01  by  Rene Brun
*-- Author :
      SUBROUTINE SIFIND(NRES,NRESH,NCODE,NOTAB,L)
C
C    ....................................................
C         PURPOSE
C            TO PUT NRES AND NRESH TOGETHER IN CNAMES
C            FOR SEARCH TO FIND NCODE
C
C         USAGE
C            CALL SIFIND(NRES,NRESH,NCODE,NOTAB,L)
C
C         DESCRIPTION OF PARAMETERS
C            NRES,NRESH - ASSEMBLES NAME TO BE SEARCHED FOR
C            NCODE      - INTERNAL CODE OF ASSEMBLED CNAMES
C            NOTAB      - TABLE NO TO BE SEARCHED
C            L          - .TRUE. IF CNAMES FOUND IN TABLE
C                       - .FALSE. OTHERWISE
C
C         COMM. BLOCKS USED
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SEARCH
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
      LOGICAL L
      CHARACTER CNAMES*8
      CHARACTER CHTEM1*4
      CHARACTER CHTEM2*4
      CALL UHTOC(NRES,4,CHTEM1,4)
      CALL UHTOC(NRESH,4,CHTEM2,4)
      CNAMES=CHTEM1//CHTEM2
      CALL SIARCH(CNAMES,NCODE,NOTAB,L)
C
 999  END
