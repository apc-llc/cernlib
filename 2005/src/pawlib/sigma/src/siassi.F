*
* $Id: siassi.F,v 1.1.1.1 1995/12/12 14:36:15 mclareni Exp $
*
* $Log: siassi.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:15  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/02 06/08/93  16.52.57  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIASSI
C
C
C    ..................................................
C         PURPOSE
C            ASSIGNS THE VALUE (X) OF THE RHS TO THE UNSUBSCRIPTED NAME
C            OF THE LHS OF AN ASSIGNMENT STATEMENT  RES=X.
C
C            SIASSI(IN CONTRAST TO GET)ONLY FOLLOWS LINKED CHAINS
C            IT DOES NOT CHECK IF RES IS A SUBPROGRAM NAME ON LEVEL ZERO
C
C
C         TRACKS USED
C            TRACK 33      FOR GENERAL TRACING OF KEY INDICES
C         COMM. BLOCKS USED
C            COM1
C
C         REMARKS
C            TOP OF STACK SHOULD CONTAIN  X
C            TOP-1        SHOULD CONTAIN  RES
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SINERR
C            IGETT2
C            ITRAK
C            SILSKK
C            SISTAK
C            NGET
C            RELAREA
C            SEE
C            SISTR2
C            SIUSTC
C            SIUSTR
C
C         METHOD
C            X IS COPIED IF NOT AN INTERMEDIATE RESULT ALREADY
C            INTERMEDIATE RESULT IS ATTACHED TO THE NAME RES
C
C         AUTHOR.   JURIS REINFELDS      DATE 05/06/74
C
C...  PAW VERSION  ...  MAY 1988
C
C    ..................................................
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
#include "sigma/siclin.inc"
C
      DIMENSION XNCO(10)
      DIMENSION DIM(10)
      INTEGER IDIM(10)
      CHARACTER CDOLLA*1,CL*8
      EQUIVALENCE(CDOLLA,CL)
C
      CHARACTER*4  TYPE
C
      CALL SITRAC(' SIASSI  ')
C
C
C   IF RHS IS NOT AN ARRAY
C   SET PROPERTIES OF X (RHS) INTO COMM. BLOCK VARIABLES
C---  DEFINE XNCO(1) TO SIGNAL TO NGET THAT DIM HAS TO BE DEFINED
      XNCO(1)=1.0
      CALL SINGET(ISI,0,XNCO)
      IF(IERRNO.NE.0)RETURN
      IF(ISI.GE.3) GOTO 965
C
      IF(SITRAK(33).EQ.1)WRITE(NPRINT,1000) (XNCO(I),I=1,NDIM)
 1000 FORMAT(' XNCO ',10F5.0)
C
C   A DOLLAR NAME RHS IS ALREADY A COPY
C   CHECK LOCAL NAME TO AVOID KILLING $-NAME ACTUAL PARAMETERS
      CALL SILSKK(0,INAME,CL)
      CALL SINGET(II,0,DIM)
      MORHS=MODE
      IF(IERRNO.NE.0)RETURN
      IF(KIT.GT.0) GOTO 901
      LA1=IADDR
      LENGT=LENGTH
      CALL SILSKK(1,INAME,CNAME)
      CALL SIGSEE
      IF(KIT.LT.0)CALL KUVDEL(CNAME)
      TYPE='R'
       MODE=MORHS
       IF(MODE.EQ.2)TYPE='C'
******************************************
      IDIM(1)=DIM(1)
      IDIM(2)=DIM(2)
      IDIM(3)=DIM(3)
      CALL KUVCRE(CNAME(1:8),TYPE,IDIM,LLOW,LHIGH)
      IF(LLOW.EQ.0)GOTO 903
******************************************
CEV   IF(SITRAK(3).EQ.1)PRINT *,' IN SIASSI DIM=',DIM(1),DIM(2),DIM(3)
      DO 100 J=1,LENGT
         DYNA(LLOW)=DYNA(LA1)
         LLOW=LLOW+1
         LA1=LA1+1
  100 CONTINUE
C
      CALL SIUSTK
      CALL SIUSTR
      CALL SIUSTK
      RETURN
C
965   CALL SINERR(65)
      RETURN
901   CALL SINERR(1)
      RETURN
903   CALL SINERR(3)
  999 END
