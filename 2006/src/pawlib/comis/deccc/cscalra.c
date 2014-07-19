/*
 * $Id: cscalra.c,v 1.1.1.1 1996/02/26 17:16:55 mclareni Exp $
 *
 * $Log: cscalra.c,v $
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if defined(CERNLIB_ALPHA_OSF)
/*CMZ :  1.17/03 18/11/93  16.53.06  by  Vladimir Berezhnoi*/
/*-- Author :*/

#define cscalr
#undef  cscalr

#ifdef __osf__

#ifdef JMP_DEBUG
#define PRINTF(x)       printf x
#else
#define PRINTF(x)
#endif

/*
 * Note from Antonio Pastore - DJP
 *
 * This package HAS to be LINKED with the following options:
 *
 *	f77 -D 4000000 -T 2000000
 *
 * if LINK_LOW is defined!
 */

float
cscalr_ (fptr,n,p)
	int *fptr;
	int *n;
	int p[15];
{
#ifdef LINK_LOW
	float (*name)();
	name = (float (*)())*fptr;

#else
	int jumpad_();
	float (*name)();
	unsigned long ptr = (unsigned long)jumpad_;

	ptr += *fptr;
	name = (float (*)())ptr;
#endif

	switch (*n)
	{
	case 0:
		return( (*name)());
	case 1:
		return( (*name)(p[0]));
	case 2:
		return( (*name)(p[0],p[1] ));
	case 3:
		return( (*name)(p[0],p[1],p[2] ));
	case 4:
		return( (*name)(p[0],p[1],p[2],p[3] ));
	case 5:
		return( (*name)(p[0],p[1],p[2],p[3],p[4] ));
	case 6:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5] ));
	case 7:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6] ));
	case 8:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7] ));
	case 9:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8] ));
	case 10:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9] ));
	case 11:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10] ));
	case 12:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11] ));
	case 13:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12] ));
	case 14:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12],p[13] ));
	case 15:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14]));
	case 16:
		return( (*name)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]
		    ,p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14],p[15]));
	default:
		printf("\n More then 16 arguments in call users routine");
	}

}


#endif
#endif
