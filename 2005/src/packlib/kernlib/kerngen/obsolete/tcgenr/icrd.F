*
* $Id: icrd.F,v 1.1.1.1 1996/02/15 17:53:26 mclareni Exp $
*
* $Log: icrd.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:26  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION ICRD (IBLOC)
C
C
      COMMON /CARDS/IC( 7)
      COMMON /PHYS/ IP(20)
      EQUIVALENCE (NCBLOC,IC), (NPBLOC,IP)
C
C--      ENTRY ICRD
C
      II= 2
      M= 1
      IBL= IBLOC
   10 IF (IBL.EQ.IC(II+1))  GO TO 19
   12 M= M+1
      IF (M.GT.NCBLOC)  GO TO 91
      II= IC(II)
      GO TO 10
C
   19 ICRD= II+1
      RETURN
C
         ENTRY NCRD                                                      CDC
C        ENTRY NCRD    (IBLOC)                                          -CDC
C
      GO TO 12
C
         ENTRY ITIT                                                      CDC
C        ENTRY ITIT    (IBLOC)                                          -CDC
C
      JJ= 2
C     NPBLC= NPBLOC                                                     UNI
C     DO 22 N=1,NPBLC                                                   UNI
      DO 22 N=1,NPBLOC                                                  -UNI
      IF (IBLOC.EQ.IP(JJ+1))  GO TO 99
   22 JJ= IP(JJ)
C
   91 ICRD= 0
C     NCRD= 0                                                           IBM
C     ITIT= 0                                                           IBM
      RETURN
C
   99 ICRD= JJ+1
C     ITIT= ICRD                                                        IBM
      RETURN
      END
