*
* $Id: csmap.F,v 1.1.1.1 1996/02/26 17:16:21 mclareni Exp $
*
* $Log: csmap.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:21  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 19/09/94  10.05.14  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSMAP(NAME)
      CHARACTER*(*) NAME
#if defined(CERNLIB_VAX)
***********************************************************
*              VAX version:                                           *
*   THIS ROUTINE CREATES THE HEADERS FOR TWO TABLES:      *
*   THE TABLE OF COMMON-BLOCKS' ADDRESSES AND THE         *
*   TABLE OF ROUTINES' ADDRESSES. THESE TABLES ARE        *
*   EXPECTED TO BE GENERATED EARLIER IN CST-FILE 'NAME'   *
*   BY PROGRAM CST.                                       *
*   THIS ROUTINE TRYES TO 'CONNECT' THE HEADERS TO THE    *
*   CORRESPONDING TABLES IN THE FILE. ACCESS TO TABLES    *
*   FROM THE USER'S VIEWPOINT:                            *
*                                                         *
*      I=MLSEAR(ITB,NM)                                   *
*                                                         *
*   WHERE  ITB - TABLE INDEX: ITBC FOR COMMONS,           *
*                             ITBS FOR ROUTINES;          *
*          NM  - ARRAY OF TWO WORDS, CONTAINING THE NAME  *
*                OF THE ITEM (COMMON OR ROUTINE) TO       *
*                SEARCH: 8 CHARACTERS (INCLUDING POSSIBLE *
*                TRAILING BLANKS).                        *
*                                                         *
*   OUTPUT: IF (I.EQ.0) THEN NO SUCH ITEM IN THE TABLE;   *
*           OTHERWIZE IQ(I) CONTAINES THE ADDRESS OF THE  *
*           ITEM.                                         *
*                                                         *
***********************************************************
#include "comis/cslun.inc"
      EXTERNAL MLBSRP,MLEMPT,MLCMP8

*
      COMMON /MDIND/ MSTCK,MHEAP
*
*   COMMON, CONTAINING THE TABLES' INDEXES:
*
      COMMON /CSTBCS/ ITBC,ITBS
*
      DIMENSION IPGP(5)
*   IPGP(4) CONTAINS THE RECORD SIZE,
*   IPGP(5) CONTAINS THE NUMBER OF PAGES:
      DATA IPGP/0,0,0,127,3/
*
      IF(ISTMAP.NE.0)RETURN
*   TRY TO OPEN CST-FILE (SYSTEM-DEPENDENT!):
      OPEN (UNIT=LUNMAP,FILE=NAME,STATUS='OLD',ACCESS='DIRECT',
     *      RECL=127,RECORDTYPE='FIXED',ORGANIZATION='RELATIVE',
     *      FORM='UNFORMATTED',DEFAULTFILE='.CST',ERR=10)
* IF SUCCESSEFUL, THEN INITIALIZE THE PAGING MEMORY:
      IPGP(1)=LUNMAP
      ISTMAP=1
      IPG=MPDEF(MHEAP,MHEAP,IPGP)
*   TABLES' ROOTS:
      IROOTC=MIMPJW(IPG,0)
      IROOTS=MIMPJW(IPG,1)
*   INITIALIZE THE TABLES IF THEIR ROOTS ARE DEFINED:
      IF(IROOTC.EQ.0) GO TO 1
      ITBC=MLDEF(MHEAP,MLBSRP,MLEMPT,MLEMPT,MLEMPT,MLEMPT)
      CALL MLBINP(ITBC,MHEAP,8,1,1,IPG,MLCMP8,0)
      CALL MLSTRT(ITBC,IROOTC)
    1 IF(IROOTS.EQ.0) GO TO 10
      ITBS=MLDEF(MHEAP,MLBSRP,MLEMPT,MLEMPT,MLEMPT,MLEMPT)
      CALL MLBINP(ITBS,MHEAP,8,1,1,IPG,MLCMP8,0)
      CALL MLSTRT(ITBS,IROOTS)
      RETURN
*   IF ERROR IN FILE OPENING, THEN DO NOTHING:
   10 CALL CSSOUT(NAME)
      CALL CSSOUT('MAP FILE WAS NOT OPEN')
      RETURN
#endif
#if defined(CERNLIB_SHL)
      COMMON /CSTBCS/ ITBC,ITBS
      CHARACTER *80 LIBNAME,SYMBOL*32
      INTEGER CS_SHL_LOAD
      LIBNAME=NAME
      N=LENOCC(LIBNAME)
      CALL CUTOL(LIBNAME(1:N))
      IF(ITBS.NE.0)THEN
        NS=-1
        CALL CS_SHL_SYMBOLS(LIBNAME(1:N),NS,SYMBOL)
        IF(NS.EQ.-2)GO TO 20
 10     L=LENOCC(SYMBOL)
        IF(SYMBOL(L:L).EQ.'_')L=L-1
        CALL CSCHID(SYMBOL(:L))
        IT=CSLTGP(IPVS)
        IF(IT.GT.0)CALL CSDPRO(IT)
        CALL CS_SHL_SYMBOLS(LIBNAME(1:N),NS,SYMBOL)
        IF(NS.NE.-2)GO TO 10
        CALL CS_SHL_UNLOAD(LIBNAME(1:N))
        ITBS=ITBS-1
 20     CONTINUE
      ENDIF
      IERR=CS_SHL_LOAD(LIBNAME(1:N))
      IF(IERR.EQ.0)THEN
        ITBS=ITBS+1
      ELSE
        CALL CS_SHL_UNLOAD(LIBNAME(1:N))
      ENDIF
#endif
      END
