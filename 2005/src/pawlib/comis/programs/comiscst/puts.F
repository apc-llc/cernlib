*
* $Id: puts.F,v 1.1.1.1 1996/02/26 17:16:40 mclareni Exp $
*
* $Log: puts.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:40  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.13/00 27/03/92  14.34.06  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE PUTS(NAME,IADR)
************************************************
*                                              *
*   SYSTEM-INDEPENDENT ROUTINE: PUT THE RECORD *
*   CONTAINING THE SUBROUTINE NAME INTO THE    *
*   TABLE                                      *
*                                              *
*   NAME - ARRAY OF TWO WORDS, CONTAINING THE  *
*          NAME OF THE SUBROUTINE ( WITH       *
*          POSSIBLE TRAILING BLANKS ),         *
*                                              *
*   IADR - THE ADDRESS OF THE SUBROUTINE       *
*                                              *
************************************************
*
*   THE NUMBER OF COMIS SYSTEM'S ROUTINES NOT TO BE INCLUDED IN THE TABLE:
*
      PARAMETER (NSSUB=3)
      PARAMETER (NSDIM=NSSUB*2,NSLOOP=NSSUB*2-1)
*
      COMMON /MDPOOL/ IQ(6)
#include "comis/cstbls.inc"
      DIMENSION NAMSUB(NSDIM),NAME(2)
*
*   THE NAMES OF THE ROUTINES NOT TO BE INCLUDED IN THE TABLE:
*
      DATA NAMSUB/4HCSIN,4HIT  ,4HCSPA,4HUS  ,4HCSTE,4HXT  /
*
      DO 1 K=1,NSLOOP,2
      IF(NAME(1).EQ.NAMSUB(K) .AND. NAME(2).EQ.NAMSUB(K+1)) RETURN
    1 CONTINUE
*
************
*
      I=MLINS(ITBLS,NAME)
      IF(I.GT.0) IQ(I)=IADR
      RETURN
      END
