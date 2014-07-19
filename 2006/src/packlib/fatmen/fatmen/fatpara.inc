*
* $Id: fatpara.inc,v 1.1.1.1 1996/03/07 15:17:58 mclareni Exp $
*
* $Log: fatpara.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:58  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATPARA_INC
#define CERNLIB_FATMEN_FATPARA_INC
*
*
* fatpara.inc
*
* Start of FATMEN sequence FATPARA
#include "fatmen/fttpara.inc"
*
** ***     Data set bank mnemonics
*
*          Keys
      PARAMETER ( MKSRFA= 1, MKFNFA= 2, MKCLFA=7, MKMTFA=8
     1           ,MKLCFA= 9, MKNBFA=10, NKDSFA=10,MKDRFA=2)
*
** ***     Bank offsets
*
      PARAMETER ( MFQNFA=  1, MHSNFA= 65, MCPLFA= 67, MDRPFA= 67
     1           ,MMTPFA= 68, MLOCFA= 69, MHSTFA= 70, MHOSFA= 74
     2           ,MVSNFA= 77, MVIDFA= 79, MVIPFA= 81, MDENFA= 82
     3           ,MVSQFA= 83, MFSQFA= 84, MSRDFA= 85, MERDFA= 86
     4           ,MSBLFA= 87, MEBLFA= 88, MRFMFA= 89, MRLNFA= 90
     5           ,MBLNFA= 91, MFLFFA= 92, MFUTFA= 93, MCRTFA= 94
     6           ,MCTTFA= 95, MLATFA= 96, MCURFA= 97, MCIDFA= 99
     7           ,MCNIFA=101, MCJIFA=103, MFPRFA=105, MSYWFA=106
     8           ,MUSWFA=116, MUCMFA=126, NWDSFA=145
     9           ,MFSZFA=MSYWFA,MUSCFA=MSYWFA+1)
 
*
** ***     Lengths of character fields in bytes
*
      PARAMETER ( NFQNFA = (MHSNFA-MFQNFA)*4,
     +            NHSNFA = (MCPLFA-MHSNFA)*4,
     +            NHSTFA = (MHOSFA-MHSTFA)*4,
     +            NHOSFA = (MVSNFA-MHOSFA)*4,
     +            NVSNFA = (MVIDFA-MVSNFA)*4,
     +            NVIDFA = (MVIPFA-MVIDFA)*4,
     +            NRFMFA = (MRLNFA-MRFMFA)*4,
     +            NFLFFA = (MFUTFA-MFLFFA)*4,
     +            NFUTFA = (MCRTFA-MFUTFA)*4,
     +            NCURFA = (MCIDFA-MCURFA)*4,
     +            NCIDFA = (MCNIFA-MCIDFA)*4,
     +            NCNIFA = (MCJIFA-MCNIFA)*4,
     +            NCJIFA = (MFPRFA-MCJIFA)*4,
     +            NUCMFA = (NWDSFA-MUCMFA)*4)
*
** ***     Lengths of integer fields in words
*
      PARAMETER ( NCPLFA = 1  )
      PARAMETER ( NMTPFA = 1  )
      PARAMETER ( NLOCFA = 1  )
      PARAMETER ( NVIPFA = 1  )
      PARAMETER ( NDENFA = 1  )
      PARAMETER ( NVSQFA = 1  )
      PARAMETER ( NFSQFA = 1  )
      PARAMETER ( NSRDFA = 1  )
      PARAMETER ( NERDFA = 1  )
      PARAMETER ( NSBLFA = 1  )
      PARAMETER ( NEBLFA = 1  )
      PARAMETER ( NRLNFA = 1  )
      PARAMETER ( NBLNFA = 1  )
      PARAMETER ( NCRTFA = 1  )
      PARAMETER ( NCTTFA = 1  )
      PARAMETER ( NLATFA = 1  )
      PARAMETER ( NFPRFA = 1  )
      PARAMETER ( NFSZFA = 1  )
      PARAMETER ( NUSCFA = 1  )
      PARAMETER ( NUSWFA = 10 )
      PARAMETER ( NSYWFA = 10 )
*
*     Definitions of 'Copy Level' (i.e. Data representation)
*     These definitions adhere to those used in FPACK
*
*     PARAMETER (MFIEEE= 1, MFEEEI= 5, MFIBM  =2, MFCRAY= 4,
      PARAMETER (MFIEEE= 1, MFEEEI= 4, MFIBM  =2, MFCRAY= 5,
     +           MFVAX = 3)
 
*
*     Maximum # of location codes, copy levels, media types and luns
*
      PARAMETER (KMXLOC=99, KMXCPL=99, KMXMTP=99, KMXLUN=99)
      PARAMETER (KMXKEY=99)
 
      INTEGER     IBTYPE(35)
      INTEGER     IBOFFS(35)
      INTEGER     IBLENG(35)
      CHARACTER*6 CHBANK(35)
* End of FATMEN sequence FATPARA

#endif
