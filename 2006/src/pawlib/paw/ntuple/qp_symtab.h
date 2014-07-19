/*
 *  qp_symtab.h  --
 *	Declare routines for symbol table management
 *	used to store static typing of the 'identifier'
 *	type entities.
 *
 *  Original: 19-Oct-1994 15:57
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_symtab.h,v 1.3 1996/04/23 18:39:04 maartenb Exp $
 *
 *  $Log: qp_symtab.h,v $
 *  Revision 1.3  1996/04/23 18:39:04  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_SYMTAB
#define CERN_SYMTAB

#ifndef CERN_SIGNATURE
#include	"qp_signature.h"
#endif


typedef struct _stentry_ STEntry;
struct _stentry_ {
	STEntry		*next;
	char		*name;
	Signature	*sig_list;
};

typedef struct _symtab_ {
	int	size;
	int	mark;
	int	cnt;
	STEntry	*tab;
	int	hsize;
	STEntry	**htab;
} SymTab;


extern SymTab *sf_symtab;


SymTab *
st_new( unsigned int size );

STEntry *
st_lookup( SymTab * st, char * const name );

STIndex
st_index( SymTab * st, char * const name );

STEntry *
st_entry( SymTab * st, STIndex index );

int
st_insert( SymTab * st, char * const name, Signature * const s );

void
st_del( SymTab * st, char * const name );

void
st_mark( SymTab * st );

void
st_sweep( SymTab * st );

#endif	/*	CERN_SYMTAB	*/
