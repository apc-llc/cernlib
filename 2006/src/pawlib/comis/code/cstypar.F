*
* $Id: cstypar.F,v 1.1.1.1 1996/02/26 17:16:33 mclareni Exp $
*
* $Log: cstypar.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:33  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 13/01/95  11.35.47  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSTYPAR(IADDR,IPAR,TPAR,ISIZE,NVDIM,IVDIM,IERR)
***--------------------------------
* input:  IADDR -- cs-address of routine (iaddr=csaddr('name'))
* input:  IPAR  -- the parameter index
* output: TPAR(1:1)='I' for integer
*                  ='R' for real
*                  ='C' for character
*                  ='L' for logical
*                  ='D' for double pr.
*                  ='S or E' for external
*                  ='X' for complex
* output: ISIZE -- the size in bytes of the base type, 4 for REAL ...
* NVDIM, input: size of IVDIM; output: the number of array dimensions
* output: IVDIM for each dimension the range
* output: IERR 0-OK, 1- addr is wrong, 2-ipar is wrong, 3- ivdim to small
*              4- no info
***--------------------------------
         COMMON/MDPOOL/IQ(100)
         CHARACTER *(*) TPAR,REPL*8
         INTEGER IVDIM(NVDIM)
         PARAMETER (KSDESP=5)
         DATA REPL/'IRCLDSXE'/
      IERR=0
      IF(IADDR.GT.0)THEN
        I=IQ(IADDR+KSDESP)
        NPAR=0
        IF(I.EQ.0)THEN
          IERR=4
          RETURN
        ENDIF
  1     IF(I.GT.0)THEN
          NPAR=NPAR+1
          IF(NPAR.EQ.IPAR)THEN
* load info
            ITPAR=IQ(I+1)
            ISIZE=IQ(I+2)
            NDIM=IQ(I+3)
            TPAR(1:1)=REPL(ITPAR:ITPAR)
            IF(NVDIM.LT.NDIM)THEN
              IERR=3
              RETURN
            ENDIF
            DO K=1,NDIM
              IVDIM(K)=IQ(I+K+3)
            ENDDO
            NVDIM=NDIM
          ELSE
            I=IQ(I)
            GO TO 1
          ENDIF
        ELSE
          IERR=2
        ENDIF
      ELSE
        IERR=1
      ENDIF
      END



