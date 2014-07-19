/*
 *  hcpiaf.h  --
 *	Map the /HCPIAF/ /HCPIA1/ and /HCPIAC/ commons
 *
 *  Original:  3-Apr-1995 10:59
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_hcpiaf.h,v 1.3 1999/06/28 15:08:26 couet Exp $
 *
 *  $Log: c_hcpiaf.h,v $
 *  Revision 1.3  1999/06/28 15:08:26  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.2  1996/04/23 18:37:47  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_HCPIAF
#define CERN_HCPIAF

#include	<cfortran/cfortran.h>


#define	MXSLPF	32


typedef struct {
	int	ntpiaf;
	int	connpf;
	int	servpf;
	int	mastpf;
	int	slavpf;
	int	clinpf;
	int	clutpf;
	int	loglpf;
	int	nslave;
	int	nslbad;
	int	nslvpf;
	int	slinpf[MXSLPF];
	int	slutpf[MXSLPF];
	int	ngsize;
	int	mysid;
	int	smask;
	float	srin;
	float	srout;
	float	svmio;
	float	stuser;
	float	stcpu;
	int	maspid;
} hcpiaf_def;

#define HCPIAF COMMON_BLOCK(HCPIAF,hcpiaf)
COMMON_BLOCK_DEF(hcpiaf_def,HCPIAF);


typedef struct {
	char	slhopf[MXSLPF][16];
	char	slhhad[MXSLPF][16];
	char	piafrc[64];
} hcpia1_def;

#define HCPIA1 COMMON_BLOCK(HCPIA1,hcpia1)
COMMON_BLOCK_DEF(hcpia1_def,HCPIA1);


typedef struct {
	char	chsmpf[80];
	char	chlmpf[256];
} hcpiac_def;

#define HCPIAC COMMON_BLOCK(HCPIAC,hcpiac)
COMMON_BLOCK_DEF(hcpiac_def,HCPIAC);


#endif	/*	CERN_HCPIAF	*/
