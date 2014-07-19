*
* $Id: memor4.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: memor4.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
. **************************************
.
.     FUNCTION  MEMOR4( LADR )
.     LADR AND THE FUNCTION VALUE ARE FORTRAN INDICES IN BLANK$COMMON
. *   ASSUMES NO MCORE$ PERFORMED BY THE FTN LIBRARY, I.E THE IO BUFFER
. *   NEEDED BY FTN WAS PROVIDED BY USER  (ELT F2FCA OF THE FTN LIBRARY)
.
. **************************************
 
          AXR$
          FORT$
 
LIMSIZ    EQU       0200000
MSTEP     EQU       010000
 
          INFO  4  4
$(4),IQ   RES  1
 
MEMORC    INFO  2  2
$(2)
LASTAD    +         LASTD$
LIMABS    +         LIMSIZ-1
 
$(1)
MEMOR4*
          L         A2,0,XARG         . LOC( LADR )
          L         A1,0,A2           . LADR
          A,U       A1,IQ-1
          SZ        A0
          DI,U      A0,MSTEP
          MSI,U     A0,MSTEP
          A,U       A0,MSTEP-1
          ANU       A0,LASTAD
          JZ        A1,MEXIT
          JN        A1,LCOR0
. ----- MCORE REQUESTED, CHECK  LASTAD .LE. LIMABS
          L         A4,LIMABS
          TLE,U     A4,LASTD$
          S         A0,LIMABS
          TG        A0,LIMABS
          L         A0,LIMABS
          S         A0,LASTAD
          ER        MCORE$
MEXIT
          AN,U      A0,IQ-1
          J         RETUR1
. ----- CORE REDUCTION  REQUESTED, CHECK  LASTAD .GE. INITIAL USER SPACE LASTD$
LCOR0     TLE,U     A0,LASTD$
          J         LCOR4
LCOR2     S         A0,LASTAD
          ER        LCORE$
          J         MEXIT
LCOR4
          L,U       A0,LASTD$
          TE        A0,LASTAD
          J         LCOR2
          J         MEXIT
          END
