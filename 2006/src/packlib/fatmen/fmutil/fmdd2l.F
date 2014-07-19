*
* $Id: fmdd2l.F,v 1.1.1.1 1996/03/07 15:18:16 mclareni Exp $
*
* $Log: fmdd2l.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:16  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMDD2L(DDNAME,LUN,IRC)
*
*     Convert a DDNAME to the corresponding logical unit
*
      CHARACTER*(*) DDNAME
 
      IRC = 0
      LDD = LENOCC(DDNAME)
*
*     Get logical unit from DDNAME
*
      IF(DDNAME(1:2).EQ.'FT') THEN
*
*     IBM FORTRAN
*
         READ(DDNAME(3:4),'(I2)') LUN
 
      ELSEIF(DDNAME(1:6).EQ.'IOFILE') THEN
*
*     IOPACK
*
         READ(DDNAME(7:8),'(I2)') LUN
 
      ELSEIF(DDNAME(1:2).EQ.'VM') THEN
*
*     VMIO
*
         READ(DDNAME(3:4),'(I2)') LUN
 
      ELSEIF(DDNAME(1:3).EQ.'FOR') THEN
*
*     VAX FORTRAN
*
         READ(DDNAME(4:6),'(I3)') LUN
 
      ELSEIF(DDNAME(1:5).EQ.'fort.') THEN
*
*     Unix FORTRAN
*
         IF(LDD.EQ.6) THEN
            READ(DDNAME(6:6),'(I1)') LUN
         ELSE
            READ(DDNAME(6:7),'(I2)') LUN
         ENDIF
 
      ELSEIF(DDNAME(1:3).EQ.'ftn') THEN
*
*     Unix FORTRAN (e.g. HP/UX)
*
         READ(DDNAME(4:5),'(I2)') LUN
*
*
*     Character lun
*
      ELSEIF(ICNUM(DDNAME,1,LDD).EQ.LDD+1) THEN
         LUN = ICDECI(DDNAME,1,LDD)
      ENDIF
 
      END
