/*
 *  hash_int_table.c  --
 *	'instantiate' a hash table (string,int)
 *
 *  Original: 10-Jan-1996 11:56
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: hash_int_table.c,v 1.2 1996/04/23 18:37:57 maartenb Exp $
 *
 *  $Log: hash_int_table.c,v $
 *  Revision 1.2  1996/04/23 18:37:57  maartenb
 *  - Add RCS keywords
 *
 *
 */

#define	HASH_PREF	HashInt
#define	HashData	int
#define HASH_IMPLEMENT

#include	"hash_table.h"
