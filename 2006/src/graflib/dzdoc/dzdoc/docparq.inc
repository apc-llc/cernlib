*
* $Id: docparq.inc,v 1.1.1.1 1996/03/04 16:12:58 mclareni Exp $
*
* $Log: docparq.inc,v $
* Revision 1.1.1.1  1996/03/04 16:12:58  mclareni
* Dzdoc/Zebpack
*
*
#ifndef CERNLIB_DZDOC_DOCPARQ_INC
#define CERNLIB_DZDOC_DOCPARQ_INC
*
*  Definitions for DZDOC
*
* docparq.inc
*
C
C----           PARAMETER definitions for DZDOC
C
C--     Offset of DOWN bank for text buffers in master DOC babk
      PARAMETER (MDOCDQ = -1)
C--     Offset of number of words used
      PARAMETER (MDCENQ = 1 )
 
C----   Description of the documentation bank
C       =====================================
 
C--     Bank header part
C       ----------------
C--     Number of words in the header
      PARAMETER (NBHEAQ = 20)
C--     Offset of encoded bank IDH
      PARAMETER (MBIDHQ = 1)
C--     Offset of number of words in use
      PARAMETER (MBUSEQ = MBIDHQ+1)
C--     Offset of number of words in the header
      PARAMETER (MBHEAQ = MBUSEQ+1)
C--     Offset of total number of links
      PARAMETER (MBNNDQ = MBHEAQ+1)
C--     Offset of number of structural links
      PARAMETER (MBNNLQ = MBNNDQ+1)
C--     Offset of number of data words
      PARAMETER (MBNNSQ = MBNNLQ+1)
 
C--     Offset of number of words with general information
      PARAMETER (MBGENQ = 11)
C--     Offset of number of words describing the links
      PARAMETER (MBLINQ = MBGENQ+1)
C--     Offset of number of words describing the ref links
      PARAMETER (MBRLIQ = MBLINQ+1)
C--     Offset of number of words describing the status bits
      PARAMETER (MBBITQ = MBRLIQ+1)
C--     Offset of number of words describing the data words
      PARAMETER (MBDATQ = MBBITQ+1)
 
C---             Bank Description pointers and flags
C---             ===================================
 
C--              Normal entry
C                ------------
C--     Number of words in bank description pointers and flag section
      PARAMETER (NBDSCQ = 4)
C--     Offset of pattern word inside section
      PARAMETER (MBPATQ = 1)
C--     Offset of the two index words inside the section
      PARAMETER (MBIX1Q = MBPATQ+1  , MBIX2Q = MBIX1Q+1)
 
C--              End of sequence
C                ---------------
C--     Number of words in bank description for end of sequence entry
      PARAMETER (NBEOSQ = 2)
 
C--     Descriptor for normal entry
C       ---------------------------
C--     Start bit and number of bits for tag information
      PARAMETER (ICHTGQ = 0 , NCHTGQ = 2)
C--     Multiple choice bit
      PARAMETER (ICHBTQ = 2)
C--     Start bit and number of bits for identifier information
      PARAMETER (ICHIDQ = 10 , NCHIDQ = 5)
C--     Start bit and number of bits for REP. level information
      PARAMETER (IRPLVQ = ICHIDQ+NCHIDQ , NRPLVQ = 4)
C--     Bit for end-of-sequence flag
      PARAMETER (IBEOSQ = IRPLVQ+NRPLVQ)
 
C--     Descriptor for CHOICE tag
C       -------------------------
 
C--     Number of words for CHOICE flag descriptor
      PARAMETER (NCDSCQ = 4)
C--     Offset of pattern word inside section
      PARAMETER (MCPATQ = 1)
 
C--     Offset of the word offset in the bank and its contents
      PARAMETER (MCWOFQ = MCPATQ+1  , MCWCOQ = MCWOFQ+1)
 
C--     Bit flags choice by number (dynamic sector)
      PARAMETER (ICNUMQ = 0 )
C--     Bit flags choice by format (self describing sector)
      PARAMETER (ICFORQ = ICNUMQ + 1)
C--     Bit flags choice by contents
      PARAMETER (ICCONQ = ICFORQ + 1)
C--     Offset of self description information and width
      PARAMETER (ICSELQ = 10, NCSELQ=4)
C--     Offset of selection byte fields in word and width
      PARAMETER (ICBY1Q = ICSELQ+NCSELQ, NCBYTQ=8)
      PARAMETER (ICBY2Q = ICBY1Q+NCBYTQ)
 

#endif
