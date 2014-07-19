*
* $Id: siini.F,v 1.1.1.1 1996/02/26 17:16:40 mclareni Exp $
*
* $Log: siini.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:40  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.13/00 27/03/92  14.34.06  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE SIINI(MDSIZE,NPSIZE,NCHAN)
********************************************
*                                          *
*   SYSTEM-INDEPENDENT INITIALIZATION      *
*                                          *
*   MDSIZE - SIZE OF COMMON/MDPOOL/        *
*                                          *
*   NPSIZE - PAGE SIZE                     *
*                                          *
*   NCHAN  - CHANNEL NUMBER, CORRESPONDING *
*            TO CST-FILE                   *
*                                          *
********************************************
      EXTERNAL MDEXP,MLEMPT,MLGETM,MLPUTP,MLLISM,MLBISP,MLALNF,MLCMP8
      COMMON /MDIND/ MSTCK,MHEAP
********
*
*   ITBLC - INDEX OF THE TEMPORARY TABLE OF COMMON BLOCKS
*           (BALANCED BINARY TREE),
*
*   ITBLS - INDEX OF THE TEMPORARY TABLE OF SUBROUTINES
*           (BALANCED BINARY TREE),
*
*   ITBFC - INDEX OF THE TABLE OF COMMON BLOCKS
*           (BINARY TREE IN PAGING MEMORY)
*
*   ITBFS - INDEX OF THE TABLE OF SUBROUTINES
*           (BINARY TREE IN PAGING MEMORY)
*
********
#include "comis/cstbls.inc"
      COMMON /CSPAG/ IPAG
      INTEGER IPG(5)
      DATA IPG/5*0/
*
      IPG(1)=NCHAN
      IPG(4)=NPSIZE
      IPG(5)=400
*
*   INITIALIZE THE DYNAMIC MEMORY:
*
      CALL MDINIT(MDSIZE,MDEXP)
      CALL MHDEF(MDSIZE-6,MDEXP)
*
*   INITIALIZE THE PAGING MEMORY:
*
      IPAG=MPDEF(MHEAP,MHEAP,IPG(1))
*
*   RESERVE TWO WORDS IN PAGING MEMORY:
*
      CALL MLALNF(0,IPAG,I,2)
*
*   INITIALIZE TWO TEMPORARY TABLES:
*
      ITBLC=MLDEF(MHEAP,MLEMPT,MLLISM,MLEMPT,MLEMPT,MLGETM)
      CALL MLLINM(ITBLC,MHEAP,8,1,1,MHEAP,MLCMP8,0)
      ITBLS=MLNEW(MHEAP,ITBLC)
*
*   INITIALIZE TWO TABLES IN PAGING MEMORY:
*
      ITBFC=MLDEF(MHEAP,MLEMPT,MLBISP,MLEMPT,MLPUTP,MLEMPT)
      CALL MLBINP(ITBFC,MHEAP,8,1,1,IPAG,MLCMP8,MJSUBR(MLALNF))
      ITBFS=MLNEW(MHEAP,ITBFC)
*
      RETURN
      END
