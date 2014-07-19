*
* $Id: bktgparq.inc,v 1.1.1.1 1996/03/04 16:12:58 mclareni Exp $
*
* $Log: bktgparq.inc,v $
* Revision 1.1.1.1  1996/03/04 16:12:58  mclareni
* Dzdoc/Zebpack
*
*
#ifndef CERNLIB_DZDOC_BKTGPARQ_INC
#define CERNLIB_DZDOC_BKTGPARQ_INC
*
*   Description of the bank tag parameters bank parser
*
* bktgparq.inc
*
 
 
C--     Description of the action flags
 
C-      Code for NO  entry
      PARAMETER (IBKANQ = -1)
C-      Code for YES entry
      PARAMETER (IBKAYQ = -2)
C-      Flag for numeric tag field
      PARAMETER (IBKAMQ = -3)
C-      Flag for special decoding(CHOICE tag)
      PARAMETER (IBKASQ = -9)
 
C--     Description of the action fields
 
C-      Offset of state information
      PARAMETER (MBKASQ = 1)
C-      Offset of tag flag (1,2 words , numeric)
      PARAMETER (MBKAGQ = MBKASQ+1)
C-      Offset of repetition flag
      PARAMETER (MBKARQ = MBKAGQ+1)
C-      Offset of choice flag
      PARAMETER (MBKAHQ = MBKARQ+1)
C-      Number of action fields
      PARAMETER (NBKACQ = MBKAHQ)
 
C----   Identifiers of various card tags
 
C--               AUthor
      PARAMETER (ITGAUQ = 1)
C--               BIt (status)
      PARAMETER (ITGBIQ = ITGAUQ+1)
C--               CHoice section
      PARAMETER (ITGCHQ = ITGBIQ+1)
C--               DAta section
      PARAMETER (ITGDAQ = ITGCHQ+1)
C--               Division
      PARAMETER (ITGDVQ = ITGDAQ+1)
C--               LInk section
      PARAMETER (ITGLIQ = ITGDVQ+1)
C--               RLink section
      PARAMETER (ITGRLQ = ITGLIQ+1)
C--               Number of data words (ND)
      PARAMETER (ITGNDQ = ITGRLQ+1)
C--               Total Number of Links (NL)
      PARAMETER (ITGNLQ = ITGNDQ+1)
C--               Number of Structural links (NS)
      PARAMETER (ITGNSQ = ITGNLQ+1)
C--               NeXt link
      PARAMETER (ITGNXQ = ITGNSQ+1)
C--               ORigin link
      PARAMETER (ITGORQ = ITGNXQ+1)
C--               REpetition section
      PARAMETER (ITGREQ = ITGORQ+1)
C--               STore
      PARAMETER (ITGSTQ = ITGREQ+1)
C--               UP link
      PARAMETER (ITGUPQ = ITGSTQ+1)
C--               VErsion
      PARAMETER (ITGVEQ = ITGUPQ+1)
C--               Entry
      PARAMETER (ITGENQ = ITGVEQ+1)
C--               NZERO words to preset to zero
      PARAMETER (ITGNZQ = ITGENQ+1)
C--               IO characteristics
      PARAMETER (ITGIOQ = ITGNZQ+1)
C--               Numerical identifier
      PARAMETER (ITGNIQ = ITGIOQ+1)
C--               IDem qualifier
      PARAMETER (ITGIDQ = ITGNIQ+1)
C--               Tag length for entry
      PARAMETER (NWTENQ = 2)
 

#endif
