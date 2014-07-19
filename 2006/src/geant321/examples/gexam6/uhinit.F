*
* $Id: uhinit.F,v 1.2 1996/02/22 13:16:35 ravndal Exp $
*
* $Log: uhinit.F,v $
* Revision 1.2  1996/02/22 13:16:35  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:19  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UHINIT
C     *
C     *       To book the user's histograms
C     *
#include "shobuf.inc"
*
*             Open a HBOOK direct access file
*
      CALL HRFILE(34,'HBOOK','N')
*
      VMX16 = 65535
      CALL HBOOK1( 1,'  ENERGY DEPOSITED PER CELL (IN MEV)$',
     *            100, 0. , 10. , VMX16)
C
      VMX8 = 255
      DO 1 I=1,NENERG
      ID=1000*(ESHOW(I)+0.0005)
      CALL HBOOK1(ID,' TOTAL ENERGY DEPOSITED (IN PERCENT)$',
     *            100, 91.0 , 101.0 , VMX8)
    1 CONTINUE
C
      END
