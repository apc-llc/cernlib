*
* $Id: memor2.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: memor2.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
. **************************************
.
.     FUNCTION  MEMOR2( LADR )
. *   ASSUMES NO MCORE$ PERFORMED BY THE FTN LIBRARY, I.E THE IO BUFFER
. *   NEEDED BY FTN WAS PROVIDED BY USER  (ELT F2FCA OF THE FTN LIBRARY)
.
. **************************************
 
          AXR$
          FORT$
 
LIMSIZ    EQU       0200000
MSTEP     EQU       010000
 
MEMORC    INFO  2  2
$(2)
LASTAD    +         LASTD$
LIMABS    +         LIMSIZ-1
 
$(1)
MEMOR2*
          L         A2,0,XARG         . LOC( LADR )
          L         A1,0,A2           . LADR
          SZ        A0
          DI,U      A0,MSTEP
          MSI,U     A0,MSTEP
          A,U       A0,MSTEP-1
          ANU       A0,LASTAD
          JZ        A1,RETUR1
          JN        A1,LCOR0
. ----- MCORE REQUESTED, CHECK  LASTAD .LE. LIMABS
          L         A4,LIMABS
          TLE,U     A4,LASTD$
          S         A0,LIMABS
          TG        A0,LIMABS
          L         A0,LIMABS
          S         A0,LASTAD
          ER        MCORE$
          J         RETUR1
. ----- CORE REDUCTION  REQUESTED, CHECK  LASTAD .GE. INITIAL USER SPACE LASTD$
LCOR0     TLE,U     A0,LASTD$
          J         LCOR4
LCOR2     S         A0,LASTAD
          ER        LCORE$
          J         RETUR1
LCOR4
          L,U       A0,LASTD$
          TE        A0,LASTAD
          J         LCOR2
          J         RETUR1
          END
